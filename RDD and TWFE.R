
library(haven)
library(data.table)
library(tidyverse)
library(lubridate) #to calculate differences between dates
library(zoo) 
library(cowplot) #To save plots
library(fixest) #For feols
library(ggfixest) #To plot the DiD with ggiplot

theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")

# install.packages("RDestimate")
# library(RDestimate)

########### RDD GRAPHS ##############


load("aggr_pc_panel.Rdata")
load("aggr_other_panel.Rdata")
load("aggr_2perm_panel.Rdata")
load("aggr_perm_panel.Rdata")
load("aggr_2pc_panel.Rdata")

dffnames<-names(dff[,3:16])

dff1<-dff

for (x in dffnames){
  for (i in 1:6) {
    
cutoff<-25-i #For the doughnut RDD

    dff1<-dff1 %>% 
      arrange(cohort, treatment, -time) %>% 
      group_by(cohort, treatment) %>% 
      mutate(!!paste0(x, i):=ifelse(time %in% c(cutoff:24),NA, lag(get(x), i)) )
  }
}

dff1<-dff1 %>% filter(cohort==time)


for (x in dffnames){
  dffrdd<-dff1 %>% 
    select(cohort, treatment, time, starts_with(x)) %>% 
    select(-x) %>% 
    pivot_longer(cols = starts_with(x)) %>%  
    mutate(after=ifelse(time>25,"after","before")) %>% 
    pivot_wider(names_from = after, values_from = value)
  
  gg<-
    dffrdd%>% 
    mutate(time=time-25) %>% 
    ggplot(aes(x=time)) +
    geom_point(aes(y=before, shape=as.factor(treatment),color=as.factor(treatment), alpha=as.factor(treatment))) +
    geom_point(aes(y=after, shape= as.factor(treatment), color=as.factor(treatment), alpha=as.factor(treatment)))+
    geom_smooth(se=F, aes(y=after, color=as.factor(treatment)), method = "lm")+
    geom_smooth(se=F, aes(y=before, color=as.factor(treatment)), method = "lm")+
    geom_vline(xintercept = 0)+
    scale_color_manual(values = c("#00203FFF", "#008080"))+
    scale_alpha_manual(values = c(.3,1))+
    scale_shape_manual(values = c(16,18))+
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          axis.title.y=element_blank())+
    facet_wrap(~name)
  ggsave2(gg, file=paste0("../../../../../../Plots/RDD_descriptive2/2pc_",x, ".jpeg"), width=7, height = 7)
}



#data("df_het", package = "didimputation")

####### Staggered DiD #########

#load("finalfullcohortdf.Rdata")

load("finalexitcohortdf.Rdata")

dffnames<-names(dff[,3:16])

dff1<-dff

for (x in dffnames){
  for (i in 1:6) {
    dff1<-dff1 %>% 
      arrange(cohort, treatment, -time) %>% 
      group_by(cohort, treatment) %>% 
      mutate(!!paste0(x, i):=lag(get(x), i)) 
  }
}

dff1<-dff1 %>% 
  filter(cohort==time)

dff2<-dff1 %>% 
  filter(time>12) %>% 
  arrange(treatment, time) %>% 
  group_by(treatment) %>% 
  mutate(base100=permanent4*100/first(permanent4))

dff2 %>% 
  filter(time>12) %>% 
  ggplot(aes(x=time, y=base100, group=as.factor(treatment),color=as.factor(treatment)))+
  geom_line()+
  geom_vline(xintercept = 25)


### ACTUAL DIFF-IN-DIFF ###

dff1<-dff1[dff1$time<36,]
dff2<-dff1[dff1$time<35,]
dff3<-dff1[dff1$time<34,]
dff4<-dff1[dff1$time<33,]
dff5<-dff1[dff1$time<32,]
dff6<-dff1[dff1$time<31,]

## Days worked

mod_1 <- feols(days_worked1 ~ i(time, treatment, ref = +25), 
               cluster = treatment,
               data = dff1)
mod_2 <- feols(days_worked2 ~ i(time, treatment, ref = +25), 
               data = dff2)

mod_3 <- feols(days_worked3 ~ i(time, treatment, ref = +25), 
               data = dff3)

mod_4 <- feols(days_worked4 ~ i(time, treatment, ref = +25), 
               data = dff4)

mod_5 <- feols(days_worked5 ~ i(time, treatment, ref = +25), 
               data = dff5)

mod_6 <- feols(days_worked6 ~ i(time, treatment, ref = +25), 
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

ggsave2(gg, file="../../../../../../Plots/DID/DID_daysworked.jpeg", width = 7, height = 5)

#Salaries

mod_1 <- feols(salaries1 ~ i(time, treatment, ref = +25) + permanent+ days_worked  + ncontracts + unemployed, 
               data = dff1)

mod_2 <- feols(salaries2 ~ i(time, treatment, ref = +25) + permanent+days_worked  + ncontracts + unemployed, 
               data = dff2)

mod_3 <- feols(salaries3 ~ i(time, treatment, ref = +25) + days_worked +permanent  + ncontracts + unemployed, 
               data = dff3)

mod_4 <- feols(salaries4 ~ i(time, treatment, ref = +25) + permanent+ days_worked  + ncontracts + unemployed, 
               data = dff4)

mod_5 <- feols(salaries5 ~ i(time, treatment, ref = +25) + permanent+ days_worked + project_based + ncontracts + unemployed, 
               data = dff5)

mod_6 <- feols(salaries6 ~ i(time, treatment, ref = +25) + permanent+ days_worked + project_based + ncontracts + unemployed, 
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
            main= "Treatment effects on salaries"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

ggsave2(gg, file="../../../../../../Plots/DID/salaries.jpeg", width = 7, height = 5)






#Number of contracts

mod_1 <- feols(ncontracts1 ~ i(time, treatment, ref = +25) + salaries + project_based + days_worked + unemployed, 
               data = dff1)

mod_2 <- feols(ncontracts2 ~ i(time, treatment, ref = +25) + salaries + project_based + days_worked + unemployed, 
               data = dff2)

mod_3 <- feols(ncontracts3 ~ i(time, treatment, ref = +25) + salaries + project_based + days_worked + unemployed, 
               data = dff3)

mod_4 <- feols(ncontracts4 ~ i(time, treatment, ref = +25) + salaries + project_based + days_worked + unemployed, 
               data = dff4)

mod_5 <- feols(ncontracts5 ~ i(time, treatment, ref = +25) + salaries + project_based + days_worked + unemployed, 
               data = dff5)

mod_6 <- feols(ncontracts6 ~ i(time, treatment, ref = +25) + salaries + project_based + days_worked + unemployed, 
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

ggsave2(gg, file="../../../../../../Plots/DID/ncontracts.jpeg", width = 7, height = 5)



## Open_ended

mod_1 <- feols(open_ended1 ~ i(time, treatment, ref = +25) + ncontracts + salaries + project_based + days_worked + unemployed, 
               data = dff1)

mod_2 <- feols(open_ended2 ~ i(time, treatment, ref = +25) + ncontracts + salaries + project_based + days_worked + unemployed, 
               data = dff2)

mod_3 <- feols(open_ended3 ~ i(time, treatment, ref = +25) + ncontracts + salaries + project_based + days_worked + unemployed, 
               data = dff3)

mod_4 <- feols(open_ended4 ~ i(time, treatment, ref = +25) + ncontracts + salaries + project_based + days_worked + unemployed, 
               data = dff4)

mod_5 <- feols(open_ended5 ~ i(time, treatment, ref = +25) + ncontracts + salaries + project_based + days_worked + unemployed, 
               data = dff5)

mod_6 <- feols(open_ended6 ~ i(time, treatment, ref = +25) + ncontracts + salaries + project_based + days_worked + unemployed, 
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

ggsave2(gg, file="../../../../../../Plots/DID/open_ended.jpeg", width = 7, height = 5)






##Permanent

# dff1<-dff1 %>% 
#   mutate(trb=ifelse(treatment==1 & time>24,1,0))
# 
# twowayfeweights(dff1, "permanent1", "treatment", "time", "trb", type = "feTR", summary_measures = T)

mod_1 <- feols(permanent1 ~ i(time, treatment, ref = +25)+salaries+days_worked+ncontracts+ unemployed , 
               data = dff1)
ggiplot(mod_1)
mod_2 <- feols(permanent2 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff2)

mod_3 <- feols(permanent3 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff3)

mod_4 <- feols(permanent4 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff4)

mod_5 <- feols(permanent5 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff5)

mod_6 <- feols(permanent6 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff6)


#gg<-
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

ggsave2(gg, file="../../../../../../Plots/DID/permanent2.jpeg", width = 7, height = 5)







##Project_based

mod_1 <- feols(project_based1 ~ i(time, treatment, ref = +25) + ncontracts + salaries + permanent + days_worked + unemployed, 
               data = dff1)

mod_2 <- feols(project_based2 ~ i(time, treatment, ref = +25) + ncontracts + salaries + permanent + days_worked + unemployed, 
               data = dff2)

mod_3 <- feols(project_based3 ~ i(time, treatment, ref = +25) + ncontracts + salaries + permanent + days_worked + unemployed, 
               data = dff3)

mod_4 <- feols(project_based4 ~ i(time, treatment, ref = +25) + ncontracts + salaries + permanent + days_worked + unemployed, 
               data = dff4)

mod_5 <- feols(project_based5 ~ i(time, treatment, ref = +25) + ncontracts + salaries + permanent + days_worked + unemployed, 
               data = dff5)

mod_6 <- feols(project_based6 ~ i(time, treatment, ref = +25) + ncontracts + salaries + permanent + days_worked + unemployed, 
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

ggsave2(gg, file="../../../../../../Plots/DID/project_based.jpeg", width = 7, height = 5)





## self_employed
mod_1 <- feols(self_emp1 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff1)

mod_2 <- feols(self_emp2 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff2)

mod_3 <- feols(self_emp3 ~ i(time, treatment, ref = +25) + ncontracts + salaries + days_worked + unemployed, 
               data = dff3)

mod_4 <- feols(self_emp4 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
               data = dff4)

mod_5 <- feols(self_emp5 ~ i(time, treatment, ref = +25) + ncontracts + salaries + days_worked + unemployed, 
               data = dff5)

mod_6 <- feols(self_emp6 ~ i(time, treatment, ref = +25) + ncontracts + salaries  + days_worked + unemployed, 
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

ggsave2(gg, file="../../../../../../Plots/DID/self_employed.jpeg", width = 7, height = 5)



##Unemployed

mod_1 = feols(unemployed1~ i(time, treatment, ref = +25)+ salaries+ project_based+ ncontracts+days_worked,                             ## FEs                ## Clustered SEs
              data = dff1)

mod_2 = feols(unemployed2~ i(time, treatment, ref = +25)+ salaries+ project_based+ ncontracts+days_worked,                             ## FEs                ## Clustered SEs
              data = dff2)

mod_3 = feols(unemployed3~ i(time, treatment, ref = +25)+ salaries+ project_based+ ncontracts+days_worked,                             ## FEs                ## Clustered SEs
              data = dff3)

mod_4 = feols(unemployed4~ i(time, treatment, ref = +25)+ salaries+ project_based+ ncontracts+days_worked,                             ## FEs                ## Clustered SEs
              data = dff4)

mod_5 = feols(unemployed5~ i(time, treatment, ref = +25)+ salaries+ project_based+ ncontracts+days_worked,                             ## FEs                ## Clustered SEs
              data = dff5)

mod_6 = feols(unemployed6~ i(time, treatment, ref = +25)+ salaries+ project_based+ ncontracts+days_worked,                             ## FEs                ## Clustered SEs
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

ggsave2(gg, file="../../../../../../Plots/DID/unemployment.jpeg", width = 7, height = 5)













##I don't even know at this point

dff1<-dff %>% 
  mutate(temporary_rate= internship+other_temporary + pre_retirement+ production_circumstances+project_based+replacement+other,
         tempoe= internship+other_temporary + open_ended+ pre_retirement+ production_circumstances+project_based+replacement+other,
         id=paste0(cohort,treatment),
         treat=cohort*treatment) %>% 
  filter(cohort <25)


m1 <- did_imputation(
  data = dff1, yname = "days_worked", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)

m2 <- did_imputation(
  data = dff1, yname = "salaries", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)


m3 <- did_imputation(
  data = dff1, yname = "ncontracts", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)


m3 <- did_imputation(
  data = dff1, yname = "open_ended", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)


m4 <- did_imputation(
  data = dff1, yname = "permanent", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)


m5 <- did_imputation(
  data = dff1, yname = "production_circumstances", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)


m6 <- did_imputation(
  data = dff1, yname = "project_based", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)


m7 <- did_imputation(
  data = dff1, yname = "unemployed", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)


m8 <- did_imputation(
  data = dff1, yname = "self_emp", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)

m9 <- did_imputation(
  data = dff1, yname = "temporary_rate", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)

m10 <- did_imputation(
  data = dff1, yname = "tempoe", gname = "treat",
  tname = "time", idname = "id",
  # event-study
  horizon = TRUE, pretrends = -12:-1
)

es<-rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

es %>% 
  ggplot(aes(x=as.numeric(term), y=estimate))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color="#008080")+
  geom_line()+
  facet_wrap(~lhs, scales = "free")

