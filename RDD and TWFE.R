
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
library(texreg) #for the tables


theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")





load("anykind_cohort_panel.Rdata")


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



