
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

source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")


variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6

#Using the functions

load("finalpanel2019b.Rdata")

dfincome<-create_income_df(min_year=2019)

df<-create_manageable_df(df, min_year = 2019)

min_time<- min(df$time)
max_time<-max(df$time)



############### FIRST ANALYSIS AGGREGATED ################


create_cohort(df, name="fromanykind")

load("fromanykind_panel.Rdata")

dftwfe<-create_twfe_dataframe(dff,
                              first_variable = 2, 
                              last_variable = 15, 
                              time_dif = 12,
                              treatment_time = 37,
                              months = 30)

dfsrdd<-create_twfe_dataframe(dff,
                              first_variable = 2, 
                              last_variable = 15,
                              time_dif = 12,
                              treatment_time = 37,
                              months = 30, 
                              rdd = T)
df1<-dfsrdd$df1
df2<-dfsrdd$df2
dfnames<-dfsrdd$dfnames

create_rdd_figures(df1 = df1, df2 = df2, dffnames = dfnames, treatment_time = 37, new_directory = "verylong2")



result_models<-create_results(dftwfe = dftwfe, new_directory = "verylongtwfe2")

dir.create("../../../../../../Tables/DID_aggregated/")


for (variable in variable_names) {
  cat(texreg(list(result_models$did_coefs[[paste("mod", variable, 1, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 2, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 3, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 4, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 5, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 6, sep = "_")]]),
             custom.model.names = labels2,
             custom.coef.map = list("ATT"="Days worked"),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE), file = paste0("../../../../../../Tables/DID_aggregated/", variable,".tex")
  )
}






################# SECOND ANALYSIS, BY SITUATION ###################

create_cohort(df, name = "bysituation", aggregation = "situation")

load("bysituation_panel.Rdata")



result_models<- list()

contracts<-c("permanent", "open-ended", "project-based")


for (g in contracts){
dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                              first_variable = 3, 
                              last_variable = 16, 
                              time_dif = 12,
                              treatment_time = 37,
                              months = 30)
result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE )
}






results<-data.frame()

for (contract in contracts){
  for (variable in variable_names){
    for(number in 1:6){
results1<- rownames_to_column(as.data.frame(result_models[[contract]]$models[[paste("mod", contract, variable, number, sep = "_")]]$coeftable)) %>% 
  mutate(contract=contract, 
         variable= variable, 
         months_later=number)
results<-rbind(results, results1)
  }
}
}

results<-results %>% 
  filter(rowname!="(Intercept)") %>% 
  transmute(time=as.numeric(gsub(".*:(-?\\d+):.*", "\\1", rowname)),
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            contract=contract,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96)




for (x in variable_names){
gg<-results %>% 
  filter(variable==x) %>%  
  ggplot(aes(x=time, group=contract, color=contract))+
  geom_point(aes(y=coefficient), alpha=.7)+
  geom_line(aes(y=coefficient), alpha=.9)+
  geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
  facet_wrap(~months_later, labeller = labeller(months_later=labels2))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c("#065465", "#008080", "grey20"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        title = element_text(hjust=.5))
ggsave2(gg, file=paste0("../../../../../../Plots/DID_bycontract/", x,".jpeg"), width = 7, height = 6)

}


# dir.create("../../../../../../Tables")
# dir.create("../../../../../../Tables/did_bycontract")

for (contract in contracts) {
  for (variable in variable_names) {
    cat(texreg(list(result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 1, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 2, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 3, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 4, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 5, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 6, sep = "_")]]),
               custom.model.names = labels2,
               custom.coef.map = list("ATT"="Days worked"),
               stars=c("*"=.1, "**"=.05, "***"=.01),
               include.rsquared = FALSE,
               include.adjrs = FALSE,
               include.nobs = FALSE,
               include.rmse = FALSE), file = paste0("../../../../../../Tables/DID_bycontract/", contract,"_", variable,".tex")
    )
  }
  
}


###############THIRD ANALYSIS BY GENDER ####################

create_cohort(df, name = "bygender", aggregation = "sex")

load("bygender_panel.Rdata")

dff<-dff %>% 
  mutate(group=ifelse(group==1, "men", "women"))


result_models<- list()

groups<-c("men", "women")


for (g in groups){
  dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                                first_variable = 3, 
                                last_variable = 16, 
                                time_dif = 12,
                                treatment_time = 37,
                                months = 30)
  result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE )
}



results<-data.frame()

for (g in groups){
  for (variable in variable_names){
    for(number in 1:6){
      results1<- rownames_to_column(as.data.frame(result_models[[g]]$models[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(group=g, 
               variable= variable, 
               months_later=number)
      results<-rbind(results, results1)
    }
  }
}

results<-results %>% 
  filter(rowname!="(Intercept)") %>% 
  transmute(time=as.numeric(gsub(".*:(-?\\d+):.*", "\\1", rowname)),
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            group=group,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96)




for (x in variable_names){
  gg<-results %>% 
    filter(variable==x) %>%  
    ggplot(aes(x=time, group=group, color=group))+
    geom_point(aes(y=coefficient), alpha=.7)+
    geom_line(aes(y=coefficient), alpha=.9)+
    geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
    facet_wrap(~months_later, labeller = labeller(months_later=labels2))+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("#008080","#b67182",  "grey20"))+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          title = element_text(hjust=.5))
  ggsave2(gg, file=paste0("../../../../../../Plots/DID_bygender/", x,".jpeg"), width = 7, height = 6)
  
}


 dir.create("../../../../../../Tables/did_bygender")

for (g in groups) {
  for (variable in variable_names) {
    cat(texreg(list(result_models[[g]]$did_coefs[[paste("mod", g, variable, 1, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 2, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 3, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 4, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 5, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 6, sep = "_")]]),
               custom.model.names = labels2,
               custom.coef.map = list("ATT"="Days worked"),
               stars=c("*"=.1, "**"=.05, "***"=.01),
               include.rsquared = FALSE,
               include.adjrs = FALSE,
               include.nobs = FALSE,
               include.rmse = FALSE), file = paste0("../../../../../../Tables/did_bygender/", g,"_", variable,".tex")
    )
  }
  
}




################ FOURTH: ANALYSIS BY PROVINCE ##############



# Change the local codes to province codes
 
df$person_muni_latest <- replace_province(df$person_muni_latest)

 
################ FIFTH: BY OCCUPATION ###################
 
create_cohort(df, aggregation = "occupation", "byocuppation")
 


############### SIXTH: BY SECTOR #######################

 df$sector <- ifelse(df$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
                                         61, 62, 71,72, 81, 89, 91, 99),
                                 paste0("0", df$sector),
                                 df$sector) 
 

df$sector<-substr(df$sector, 1,2)




# 
# load("anykind_cohort_panel.Rdata")
# dffnames<-names(dff[,2:15])
# 
# dff1<-mutate(dff[dff$cohort %in%c(4:24),], time2=time+12)
# dff2<- mutate(dff[dff$cohort >15,], time2=time)
# 
# for (x in dffnames){
#   for (i in 1:6) {
#     
# cutoff<-25-i #For the doughnut RDD
# 
#     dff1<-dff1 %>% 
#       arrange(cohort, -time) %>% 
#       group_by(cohort) %>% 
#       mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:24),NA, lag(get(x), i)) )
#     
#     dff2<-dff2 %>% 
#       arrange(cohort, -time) %>% 
#       group_by(cohort) %>% 
#       mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:24),NA, lag(get(x), i)) )
#   }
# }
# 
# 
# 
# dff1<-dff1 %>% filter(cohort==time)
# dff2<-dff2 %>% filter(cohort==time)
# 
# 
# for (x in dffnames){
#   dffrdd1<-dff1 %>% 
#     select(cohort, time, time2, starts_with(x)) %>% 
#     select(-x) %>% 
#     pivot_longer(cols = starts_with(x)) %>% 
#     mutate(after=ifelse(time2>24, "after", "before")) %>% ############treatment time -1
#     pivot_wider(names_from = after, values_from = value) %>% 
#     mutate(treatment= "Placebo")
#     
#   dffrdd2<-dff2 %>% 
#     select(cohort, time, time2,starts_with(x)) %>% 
#     select(-x) %>% 
#     pivot_longer(cols = starts_with(x)) %>% 
#     mutate(after=ifelse(time2>24, "after", "before")) %>% ############### treatment time-1
#     pivot_wider(names_from = after, values_from = value) %>% 
#     mutate(treatment="treatment")
#   
# dfrdd<-rbind(dffrdd1, dffrdd2)
#   
#   
# gg<-
#     dfrdd%>% 
#     mutate(after=ifelse(after%in% c(0,1), NA, after)) %>% 
#     mutate(time=time-25) %>%                     ##########treatment time
#     ggplot(aes(x=time2,color=as.factor(treatment))) +
#     geom_point(aes(y=before, shape=as.factor(treatment), alpha=as.factor(treatment))) +
#       geom_line(aes(y=before, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
#     geom_point(aes(y=after, shape= as.factor(treatment), alpha=as.factor(treatment)))+
#       geom_line(aes(y=after, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
#     geom_line(stat = "smooth", se=F, aes(y=after, alpha=as.factor(treatment)), method = "lm")+
#     geom_line(stat="smooth", se=F, aes(y=before, alpha=as.factor(treatment)), method = "lm")+
#     geom_vline(xintercept = 25)+ ############### treatment time
#     scale_color_manual(values = c("#00203FFF", "#008080"))+
#     scale_alpha_manual(values = c(.7,1))+
#     scale_shape_manual(values = c(18,16))+
#       scale_linetype_manual(values = c("dashed", "solid"))+
#       theme_bw()+
#     theme(legend.position = "bottom", 
#           legend.title = element_blank(),
#           axis.title.y=element_blank())+
#     facet_wrap(~name)
#   ggsave2(gg, file=paste0("../../../../../../Plots/RDD_descriptive3/last_try",x, ".jpeg"), width=8, height = 7)
# }
# 
# 



### ACTUAL DIFF-IN-DIFF ###
# dftwfe<-rbind(dff1 %>% mutate(treatment=0), 
#               dff2 %>% mutate(treatment=1))
# 



######################### Analysis disaggregating by type of contract#############################

load("situation_cohort_panel.Rdata")

for (g in c("permanent", "open-ended", "project-based", "production circumstances")){
dftwfe<-create_twfe_dataframe(dff[dff$group==g,], 
                              first_variable = 3,
                              last_variable = 16,
                              time_dif = 12, 
                              treatment_time = 37,
                              months = 28)


create_twfeplots(dftwfe, paste0("DID_long", g), 37)
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



