
library(haven)
library(data.table)
library(tidyverse)
library(lubridate) #to calculate differences between dates
library(zoo) 
library(cowplot) #To save plots
library(fixest) #For feols
library(ggfixest) #To plot the DiD with ggiplot
library(broom)
library(modelsummary)


theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")


########### RDD GRAPHS ##############







################Function ###########


load("anykind_cohort_panel.Rdata")

create_twfe_dataframe<- function(df, first_variable, last_variable, time_dif, treatment_time, months ){


dffnames<-names(df[,first_variable:last_variable])
beginning<-treatment_time-months

df1<-mutate(df[df$cohort %in%c(beginning:(treatment_time-1)),], time2=time+time_dif)

df2<- mutate(df[df$cohort >=(beginning+time_dif),], time2=time)

for (x in dffnames){
  for (i in 1:6) {
    
    cutoff<-treatment_time-i #For the doughnut RDD
    
    df1<-df1 %>% 
      arrange(cohort, -time) %>% 
      group_by(cohort) %>% 
      mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:(treatment_time-1)),NA, lag(get(x), i)),
             treatment=0)
    
    df2<-df2 %>% 
      arrange(cohort, -time) %>% 
      group_by(cohort) %>% 
      mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:(treatment_time-1)),NA, lag(get(x), i)),
             treatment=1)
  }
}

df1<-df1 %>% filter(cohort==time)
df2<-df2 %>% filter(cohort==time)

df<-rbind(df1,df2)

}

dftwfe<-create_twfe_dataframe(dff, 
                      first_variable = 2,
                      last_variable = 15,
                      time_dif = 12, 
                      treatment_time = 25,
                      months = 21)





################



load("anykind_cohort_panel.Rdata")
dffnames<-names(dff[,2:15])

dff1<-mutate(dff[dff$cohort %in%c(4:24),], time2=time+12)
dff2<- mutate(dff[dff$cohort >15,], time2=time)

for (x in dffnames){
  for (i in 1:6) {
    
cutoff<-25-i #For the doughnut RDD

    dff1<-dff1 %>% 
      arrange(cohort, -time) %>% 
      group_by(cohort) %>% 
      mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:24),NA, lag(get(x), i)) )
    
    dff2<-dff2 %>% 
      arrange(cohort, -time) %>% 
      group_by(cohort) %>% 
      mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:24),NA, lag(get(x), i)) )
  }
}



dff1<-dff1 %>% filter(cohort==time)
dff2<-dff2 %>% filter(cohort==time)


for (x in dffnames){
  dffrdd1<-dff1 %>% 
    select(cohort, time, time2, starts_with(x)) %>% 
    select(-x) %>% 
    pivot_longer(cols = starts_with(x)) %>% 
    mutate(after=ifelse(time2>24, "after", "before")) %>% 
    pivot_wider(names_from = after, values_from = value) %>% 
    mutate(treatment= "Placebo")
    
  dffrdd2<-dff2 %>% 
    select(cohort, time, time2,starts_with(x)) %>% 
    select(-x) %>% 
    pivot_longer(cols = starts_with(x)) %>% 
    mutate(after=ifelse(time2>24, "after", "before")) %>% 
    pivot_wider(names_from = after, values_from = value) %>% 
    mutate(treatment="treatment")
  
dfrdd<-rbind(dffrdd1, dffrdd2)
  
  
gg<-
    dfrdd%>% 
    mutate(after=ifelse(after%in% c(0,1), NA, after)) %>% 
    mutate(time=time-25) %>% 
    ggplot(aes(x=time2,color=as.factor(treatment))) +
    geom_point(aes(y=before, shape=as.factor(treatment), alpha=as.factor(treatment))) +
      geom_line(aes(y=before, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
    geom_point(aes(y=after, shape= as.factor(treatment), alpha=as.factor(treatment)))+
      geom_line(aes(y=after, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
    geom_line(stat = "smooth", se=F, aes(y=after, alpha=as.factor(treatment)), method = "lm")+
    geom_line(stat="smooth", se=F, aes(y=before, alpha=as.factor(treatment)), method = "lm")+
    geom_vline(xintercept = 25)+
    scale_color_manual(values = c("#00203FFF", "#008080"))+
    scale_alpha_manual(values = c(.7,1))+
    scale_shape_manual(values = c(18,16))+
      scale_linetype_manual(values = c("dashed", "solid"))+
      theme_bw()+
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          axis.title.y=element_blank())+
    facet_wrap(~name)
  ggsave2(gg, file=paste0("../../../../../../Plots/RDD_descriptive3/last_try",x, ".jpeg"), width=8, height = 7)
}





### ACTUAL DIFF-IN-DIFF ###
dftwfe<-rbind(dff1 %>% mutate(treatment=0), 
              dff2 %>% mutate(treatment=1))



#Creating the function that will perform the DID and create the plots:


create_twfeplots<- function(twfe, new_directory, treatment_date){
  
dir.create(paste0("../../../../../../Plots/", new_directory))
  
dff1<-dftwfe[dftwfe$time2<max(dftwfe$time2),]
dff2<-dftwfe[dftwfe$time2<max(dftwfe$time2)-1,]
dff3<-dftwfe[dftwfe$time2<max(dftwfe$time2)-2,]
dff4<-dftwfe[dftwfe$time2<max(dftwfe$time2)-3,]
dff5<-dftwfe[dftwfe$time2<max(dftwfe$time2)-4,]
dff6<-dftwfe[dftwfe$time2<max(dftwfe$time2)-5,]


## Days worked

mod_1 <- feols(days_worked1 ~ i(time2, treatment, ref = +treatment_date), 
               data = dff1)
mod_2 <- feols(days_worked2 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff2)

mod_3 <- feols(days_worked3 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff3)

mod_4 <- feols(days_worked4 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff4)

mod_5 <- feols(days_worked5 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff5)

mod_6 <- feols(days_worked6 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff6)

gg<-ggiplot(list("1 month later"=mod_1, 
                 "2 months later"=mod_2,
                 "3 months later"= mod_3,
                 "4 months later"= mod_4,
                 "5 months later"=mod_5,
                 "6 months later"=mod_6), 
            pt.join=TRUE, 
            pt.pch=19,
            multi_style = 'facet',
            facet_args = list(ncol=2, 
                              scales= "free_y",
                              labeller= labeller(category="")
            ),
            main= "Treatment effects on days worked"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory,"/DID_daysworked.jpeg"), width = 7, height = 5)

#Salaries

mod_1 <- feols(salaries1 ~ i(time2, treatment, ref = +treatment_date)  ,
               data = dff1)

mod_2 <- feols(salaries2 ~ i(time2, treatment, ref = +treatment_date) ,
               data = dff2)

mod_3 <- feols(salaries3 ~ i(time2, treatment, ref = +treatment_date) ,
               data = dff3)

mod_4 <- feols(salaries4 ~ i(time2, treatment, ref = +treatment_date) ,
               data = dff4)

mod_5 <- feols(salaries5 ~ i(time2, treatment, ref = +treatment_date) ,
               data = dff5)

mod_6 <- feols(salaries6 ~ i(time2, treatment, ref = +treatment_date) ,
               data = dff6)


gg<-
  ggiplot(list("1 month later"=mod_1, 
                 "2 months later"=mod_2,
                 "3 months later"= mod_3,
                 "4 months later"= mod_4,
                 "5 months later"=mod_5,
                 "6 months later"=mod_6), 
            pt.join=TRUE, 
            pt.pch=19,
            multi_style = 'facet',
            facet_args = list(ncol=2, 
                              scales= "free_y",
                              labeller= labeller(category="")
            ),
            main= "Treatment effects on salaries"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/salaries.jpeg"), width = 7, height = 5)






#Number of contracts

mod_1 <- feols(ncontracts1 ~ i(time2, treatment, ref = +treatment_date) ,
               
               data = dff1)

mod_2 <- feols(ncontracts2 ~ i(time2, treatment, ref = +treatment_date) ,
               
               data = dff2)

mod_3 <- feols(ncontracts3 ~ i(time2, treatment, ref = +treatment_date) ,
               
               data = dff3)

mod_4 <- feols(ncontracts4 ~ i(time2, treatment, ref = +treatment_date) ,
               data = dff4)

mod_5 <- feols(ncontracts5 ~ i(time2, treatment, ref = +treatment_date),
               
               data = dff5)

mod_6 <- feols(ncontracts6 ~ i(time2, treatment, ref = +treatment_date)  ,
               
               data = dff6)

gg<-ggiplot(list("1 month later"=mod_1, 
                 "2 months later"=mod_2,
                 "3 months later"= mod_3,
                 "4 months later"= mod_4,
                 "5 months later"=mod_5,
                 "6 months later"=mod_6), 
            pt.join=TRUE, 
            pt.pch=19,
            multi_style = 'facet',
            facet_args = list(ncol=2, 
                              scales= "free_y",
                              labeller= labeller(category="")
            ),
            main= "Treatment effects on number of contracts"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/ncontracts.jpeg"), width = 7, height = 5)



## Open_ended

mod_1 <- feols(open_ended1 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff1)

mod_2 <- feols(open_ended2 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff2)

mod_3 <- feols(open_ended3 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff3)

mod_4 <- feols(open_ended4 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff4)

mod_5 <- feols(open_ended5 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff5)

mod_6 <- feols(open_ended6 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff6)


gg<-ggiplot(list("1 month later"=mod_1, 
                 "2 months later"=mod_2,
                 "3 months later"= mod_3,
                 "4 months later"= mod_4,
                 "5 months later"=mod_5,
                 "6 months later"=mod_6), 
            pt.join=TRUE, 
            pt.pch=19,
            multi_style = 'facet',
            facet_args = list(ncol=2, 
                              scales= "free_y",
                              labeller= labeller(category="")
            ),
            main= "Treatment effects on open_ended contracts"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/open_ended.jpeg"), width = 7, height = 5)






##Permanent


mod_1 <- feols(permanent1 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff1)

mod_2 <- feols(permanent2 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff2)

mod_3 <- feols(permanent3 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff3)

mod_4 <- feols(permanent4 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff4)

mod_5 <- feols(permanent5 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff5)

mod_6 <- feols(permanent6 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff6)


gg<-
ggiplot(list("1 month later"=mod_1, 
             "2 months later"=mod_2,
             "3 months later"= mod_3,
             "4 months later"= mod_4,
             "5 months later"=mod_5,
             "6 months later"=mod_6), 
        pt.join=TRUE, 
        pt.pch=19,
        multi_style = 'facet',
        facet_args = list(ncol=2, 
                          scales= "free_y",
                          labeller= labeller(category="")
        ),
        main= "Treatment effects on permanent"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/permanent.jpeg"), width = 7, height = 5)






##Project_based

mod_1 <- feols(project_based1 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff1)

mod_2 <- feols(project_based2 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff2)

mod_3 <- feols(project_based3 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff3)

mod_4 <- feols(project_based4 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff4)

mod_5 <- feols(project_based5 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff5)

mod_6 <- feols(project_based6 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff6)



gg<-ggiplot(list("1 month later"=mod_1, 
                 "2 months later"=mod_2,
                 "3 months later"= mod_3,
                 "4 months later"= mod_4,
                 "5 months later"=mod_5,
                 "6 months later"=mod_6), 
            pt.join=TRUE, 
            pt.pch=19,
            multi_style = 'facet',
            facet_args = list(ncol=2, 
                              scales= "free_y",
                              labeller= labeller(category="")
            ),
            main= "Treatment effects on project_based contracts"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/project_based.jpeg"), width = 7, height = 5)





## self_employed
mod_1 <- feols(self_emp1 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff1)

mod_2 <- feols(self_emp2 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff2)

mod_3 <- feols(self_emp3 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff3)

mod_4 <- feols(self_emp4 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff4)

mod_5 <- feols(self_emp5 ~ i(time2, treatment, ref = +treatment_date) , 
               
               data = dff5)

mod_6 <- feols(self_emp6 ~ i(time2, treatment, ref = +treatment_date), 
               
               data = dff6)


gg<-ggiplot(list("1 month later"=mod_1, 
                 "2 months later"=mod_2,
                 "3 months later"= mod_3,
                 "4 months later"= mod_4,
                 "5 months later"=mod_5,
                 "6 months later"=mod_6), 
            pt.join=TRUE, 
            pt.pch=19,
            multi_style = 'facet',
            facet_args = list(ncol=2, 
                              scales= "free_y",
                              labeller= labeller(category="")
            ),
            main= "Treatment effects on self employed rate"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/self_employed.jpeg"), width = 7, height = 5)



##Unemployed

mod_1 = feols(unemployed1~ i(time2, treatment, ref = +treatment_date),  
               
              data = dff1)

mod_2 = feols(unemployed2~ i(time2, treatment, ref = +treatment_date),
              
              data = dff2)

mod_3 = feols(unemployed3~ i(time2, treatment, ref = +treatment_date),
              
              data=dff3)

mod_4 = feols(unemployed4~ i(time2, treatment, ref = +treatment_date),
              
              data = dff4)

mod_5 = feols(unemployed5~ i(time2, treatment, ref = +treatment_date),
              
              data = dff5)

mod_6 = feols(unemployed6~ i(time2, treatment, ref = +treatment_date),
              
              data = dff6)


gg<-ggiplot(list("1 month later"=mod_1, 
                 "2 months later"=mod_2,
                 "3 months later"= mod_3,
                 "4 months later"= mod_4,
                 "5 months later"=mod_5,
                 "6 months later"=mod_6), 
            pt.join=TRUE, 
            pt.pch=19,
            multi_style = 'facet',
            facet_args = list(ncol=2, 
                              scales= "free_y",
                              labeller= labeller(category="")
            ),
            main= "Policy treatment effects on unemployment"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/unemployment.jpeg"), width = 7, height = 5)
}


create_twfeplots(dftwfe, "DID2", 25)

######################### Analysis disaggregating by type of contract#############################

load("situation_cohort_panel.Rdata")

for (i in c("permanent", "open-ended", "project-based", "production circumstances")){
dftwfe<-create_twfe_dataframe(dff[dff$group==i,], 
                              first_variable = 3,
                              last_variable = 16,
                              time_dif = 12, 
                              treatment_time = 37,
                              months = 28)


create_twfeplots(dftwfe, paste0("DID_long", i), 37)
}


###############NO DONUT DESIGN ###########
load("anykind_cohort_panel.Rdata")

create_twfe_dataframe<- function(df, first_variable, last_variable, time_dif, treatment_time, months ){
  
  
  dffnames<-names(df[,first_variable:last_variable])
  beginning<-treatment_time-months
  
  df1<-mutate(df[df$cohort %in%c(beginning:(treatment_time-1)),], time2=time+time_dif)
  
  df2<- mutate(df[df$cohort >=(beginning+time_dif),], time2=time)
  
  for (x in dffnames){
    for (i in 1:6) {
      
      df1<-df1 %>% 
        arrange(cohort, -time) %>% 
        group_by(cohort) %>% 
        mutate(!!paste0(x, i):=lag(get(x), i),
               treatment=0)
      
      df2<-df2 %>% 
        arrange(cohort, -time) %>% 
        group_by(cohort) %>% 
        mutate(!!paste0(x, i):=lag(get(x), i),
               treatment=1)
    }
  }
  
  df1<-df1 %>% filter(cohort==time)
  df2<-df2 %>% filter(cohort==time)
  
  df<-rbind(df1,df2)
  
}



dftwfe<-create_twfe_dataframe(dff, 
                      first_variable = 2,
                      last_variable = 15,
                      time_dif = 12, 
                      treatment_time = 25,
                      months = 28)

create_twfeplots(dftwfe, new_directory = "no-donut_long", treatment_date = 25)



for (i in c("permanent", "open-ended", "project-based", "production circumstances")){
  dftwfe<-create_twfe_dataframe(dff[dff$group==i,], 
                                first_variable = 3,
                                last_variable = 16,
                                time_dif = 12, 
                                treatment_time = 37,
                                months = 21)
  
  
  create_twfeplots(dftwfe, paste0("DID_nodonut", i), 37)
}
