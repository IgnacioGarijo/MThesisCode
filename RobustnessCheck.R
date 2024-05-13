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
library(broom)   #Para los mapas
library(sf)
library(pROC)
library(ggpubr) #To grid the plots
library(gsynth)


theme_set(theme_bw())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")

source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Functions.R")

# for (i in 1957:2006){
#   load(paste0("finalcohort", i, ".Rdata"))
#   finalcohort<-finalcohort[finalcohort$year>=2018]
#   finalcohort$birth_date= finalcohort$birth_date.x
#   finalcohort$firm_id_sec<-finalcohort$firm_id_sec.x
#   finalcohort<-finalcohort %>% select(-c(firm_id_sec.y, firm_id_sec.x, birth_date.x, birth_date.y, province_resid, selection, spells_number, firm_main_prov,
#                                          contract_type))
#   save(finalcohort, file=paste0("rc/reducedcohort18", i, ".Rdata"))
#   gc()
# }
# 
# #Final panel for robustness checks
# df<-data.frame()
# 
# for (i in 1958:2006){
#   load(paste0("rc/reducedcohort18", i, ".Rdata"))
#   finalcohort<-finalcohort[,c("person_id", "exit_date", "entry_date", "year", "month", "days_spell", "job_relationship", "regime", 
#                               "contr_type", "ncontracts", "occupation", "sector", "birth_date", "sex","person_muni_latest")]
#   df<-rbind(df, finalcohort)
#   gc()
# }
# 
# save(df, file="rc/rcfinalpanel18.Rdata")
# 
# source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")
# 
# 
# load("rc/rcfinalpanel18.Rdata")
# 
# min_year=2018
# min_month=1
# 
# df[, c("time", "yearmonth", "exit_month"):= list((year - min_year) * 12 + (month - min_month + 1),
#                                                  as.numeric(paste0(year, sprintf("%02d", month))),
#                                                  exit_date %/% 100)]
# 
# 
# df<-df %>% select(-c(exit_date, year, month))
# 
# df[, emp:= ifelse(!is.na(contr_type)| job_relationship %in% c(87,901,902,910,930,932,937,951),1,0)]
# 
# df[,  situation:= ifelse(!is.na(contr_type),
#                          contr_type,
#                          ifelse(regime %in% c(500:540, 721:740),
#                                 "self-emp",ifelse(emp==1, "other",
#                                                   "unemp")
#                          )
# )
# ]
# 
# df$situation[is.na(df$situation)]<-"unemp"
# 
# 
# save(df, file="rc/manageabledf18.Rdata")
# 
# 
# dfincome <-create_income_df(min_year = 2018)
# 
# save(dfincome, file="rc/manageabledfincome.Rdata")
# 



load("rc/manageabledf18.Rdata")
load("rc/manageabledfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)

create_cohort(df = df,name = "rc/allkinds")
create_cohort(df=df, name ="rc/bysituation", aggregation="situation")


variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")





### FIRST ALL KINDS ####
load("rc/allkinds_panel.Rdata")
dff$salaries<- dff$salaries/100


dff<-dff[dff$cohort %in% c(1:24, 37:60),]

dff1<-dff %>% 
  mutate(time = ifelse(cohort>25, time-12, time),
         cohort=ifelse(cohort>25, cohort-12, cohort))


dftwfe<-create_twfe_dataframe(dff1,
                              first_variable = 2, 
                              last_variable = 15, 
                              time_dif = 24,
                              treatment_time = 37,
                              months = 36) %>% 
  filter(!(cohort >= 25  & treatment == 0))



result_models<-create_results(dftwfe = dftwfe, new_directory = "rc/allkinds", figures = T)



#dir.create("../../../../../../Tables/rc/allkinds/")


for (variable in variable_names) {
  cat(texreg(list(result_models$did_coefs[[paste("mod", variable, 1, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 2, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 3, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 4, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 5, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 6, sep = "_")]]),
             custom.model.names = labels2,
             custom.coef.map = list("ATT"=variable),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE), file = paste0("../../../../../../Tables/rc/allkinds/", variable,".tex")
  )
}



results<-data.frame()

for (variable in variable_names){
  for(number in 1:6){
    pretrends<-result_models$pre_trends[[paste("mod", variable, number, sep = "_")]]$p
    results1<- rownames_to_column(as.data.frame(result_models$models[[paste("mod", variable, number, sep = "_")]]$coeftable)) %>% 
      mutate(variable= variable, 
             months_later=number,
             pre_trends=pretrends)
    results<-rbind(results, results1)
  }
}


results<-results %>% 
  filter(rowname!="(Intercept)") %>% 
  transmute(time=as.numeric(gsub(".*:(-?\\d+):.*", "\\1", rowname)),
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends)



plots<-list()
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")

for (x in variable_names){
  label<-labs[[x]]
  gg<-
    results %>% 
    filter(variable==x, months_later %in% c(1,3,5)) %>%  
    ggplot(aes(x=time, color=as.factor(months_later)))+
    geom_point(aes(y=coefficient), alpha=.9)+
    geom_line(aes(y=coefficient), alpha=.7)+
    geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("#065465", "#008080", "#b67182"), labels= labels2)+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          title = element_text(hjust=.5))+
    ggtitle(label)
  plots[[paste(x)]]<-gg
  ggsave2(gg, file=paste0("../../../../../../Plots/rc/allkinds", x,".jpeg"), width = 7, height = 5)
  
}


plots<-ggarrange(plotlist = plots, common.legend = T, legend="bottom",
                 ncol=2, nrow=4)
ggsave2(plots, file="../../../../../../Plots/rc/verylongtwfe3/grid.jpeg", width = 7, height = 9)


############2.A excluding cohorts #######################


load("exludingcohort_panel.Rdata")

dff$salaries<-dff$salaries/100
result_models<- list()

contracts<-c("permanent", "open-ended", "project-based")


for (g in contracts){
  dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                                first_variable = 3, 
                                last_variable = 16, 
                                time_dif = 12,
                                treatment_time = 25,
                                months = 24)
  result_models[[g]]<-create_results(dftwfe, paste0("DID_exluding", g),disaggregation = TRUE, figures = T )
}



results<-data.frame()

for (contract in contracts){
  for (variable in variable_names){
    for(number in 1:6){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[contract]]$models[[paste("mod", contract, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(contract=contract, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
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
            minus95=coefficient-se*1.96,
            pretrends=pre_trends)



plots<-list()
for (x in variable_names){
  label<-labs[[x]]
  gg<-
    results %>% 
    filter(variable==x, months_later %in% c(1,3,5)) %>%  
    ggplot(aes(x=time, group=contract, color=contract))+
    geom_point(aes(y=coefficient), alpha=.7)+
    geom_line(aes(y=coefficient), alpha=.8)+
    geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
    facet_wrap(~months_later, labeller = labeller(months_later=labels2))+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("permanent"="#065465", "open-ended"= "#008080","project-based"= "#b67182"))+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          title = element_text(hjust=.5))+
    ggtitle(label)
  plots[[paste(x)]]<-gg
  ggsave2(gg, file=paste0("../../../../../../Plots/DID_excluding/", x,".jpeg"), width = 7, height = 4)
  
}

plots1<-ggarrange(plotlist = plots[c("unemployed", "salaries", "days_worked" , "ncontracts")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)
plots2<-ggarrange(plotlist = plots[c("project_based", "open_ended", "permanent" , "self_emp")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)
ggsave2(plots1, file="../../../../../../Plots/DID_excluding/grid1.jpeg", width = 12, height = 10)
ggsave2(plots2, file="../../../../../../Plots/DID_excluding/grid2.jpeg", width = 12, height = 10)

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
               custom.coef.map = list("ATT"=variable),
               stars=c("*"=.1, "**"=.05, "***"=.01),
               include.rsquared = FALSE,
               include.adjrs = FALSE,
               include.nobs = FALSE,
               include.rmse = FALSE), file = paste0("../../../../../../Tables/DID_excluding/", contract,"_", variable,".tex")
    )
  }
}







############ SECOND BY SITUATION ##############


gc()
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6


load("rc/bysituation_panel.Rdata")

dff$salaries<- dff$salaries/100

result_models<- list()

contracts<-c("permanent", "open-ended", "project-based")


dff<-dff[dff$cohort %in% c(1:24, 37:60),]

dff1<-dff %>% 
  mutate(time = ifelse(cohort>25, time-12, time),
         cohort=ifelse(cohort>25, cohort-12, cohort))


for (g in contracts){
  dftwfe<-create_twfe_dataframe(dff1[dff1$group==g,],
                                first_variable = 3, 
                                last_variable = 16, 
                                time_dif = 24,
                                treatment_time = 37,
                                months = 36)%>% 
    filter(!(cohort >= 25  & treatment == 0))
  result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE, figures = F )
}



results<-data.frame()

for (contract in contracts){
  for (variable in variable_names){
    for(number in 1:6){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[contract]]$models[[paste("mod", contract, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(contract=contract, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
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
            minus95=coefficient-se*1.96,
            pretrends=pre_trends)



plots<-list()
for (x in variable_names){
  label<-labs[[x]]
  gg<-
    results %>% 
    filter(variable==x, months_later %in% c(1,3,5)) %>%  
    ggplot(aes(x=time, group=contract, color=contract))+
    geom_point(aes(y=coefficient), alpha=.7)+
    geom_line(aes(y=coefficient), alpha=.8)+
    geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
    facet_wrap(~months_later, labeller = labeller(months_later=labels2))+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("permanent"="#065465", "open-ended"= "#008080","project-based"= "#b67182"))+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          title = element_text(hjust=.5))+
    ggtitle(label)
  plots[[paste(x)]]<-gg
  ggsave2(gg, file=paste0("../../../../../../Plots/rc/DID_bycontract/", x,".jpeg"), width = 7, height = 4)
  
}

plots1<-ggarrange(plotlist = plots[c("unemployed", "salaries", "days_worked" , "ncontracts")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)
plots2<-ggarrange(plotlist = plots[c("project_based", "open_ended", "permanent" , "self_emp")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)
ggsave2(plots1, file="../../../../../../Plots/rc/DID_bycontract/grid1.jpeg", width = 12, height = 10)
ggsave2(plots2, file="../../../../../../Plots/rc/DID_bycontract/grid2.jpeg", width = 12, height = 10)

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
               custom.coef.map = list("ATT"=variable),
               stars=c("*"=.1, "**"=.05, "***"=.01),
               include.rsquared = FALSE,
               include.adjrs = FALSE,
               include.nobs = FALSE,
               include.rmse = FALSE), file = paste0("../../../../../../Tables/rc/DID_bycontract/", contract,"_", variable,".tex")
    )
  }
}

############## SECTORS AND PROFESSIONS ############


load("rc/manageabledf18.Rdata")
df<-df[df$time>36,]


load("rc/manageabledfincome.Rdata")
dfincome<-dfincome[dfincome$time>36,]

df<-left_join(df, dfincome)
rm(dfincome)

df$sector <- ifelse(df$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
                                         61, 62, 71,72, 81, 89, 91, 99),
                                 paste0("0", df$sector),
                                 df$sector)


df$sector<-substr(df$sector, 1,2)
df$ncontracts[df$ncontracts==0]<-NA

df1<-df %>% 
  group_by(sector, yearmonth) %>% 
  summarise(days_worked=mean(days_spell, na.rm=T),
            salaries=mean(income, na.rm=T),
            ncontracts=mean(ncontracts, na.rm = T),
            days_worked=mean(days_spell, na.rm = T),
            internship = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                0,
                                sum(situation == "Internship or training") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            open_ended = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                0,
                                sum(situation == "open-ended") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            other_temporary = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                     0,
                                     sum(situation == "other temporary") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            permanent = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                               0,
                               sum(situation == "permanent") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            pre_retirement = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                    0,
                                    sum(situation == "Pre-retirement") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            production_circumstances = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                              0,
                                              sum(situation == "production circumstances") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            project_based = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                   0,
                                   sum(situation == "project-based") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            replacement = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                 0,
                                 sum(situation == "Replacement") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            other= ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                          0,
                          sum(situation == "other") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            unemployed= sum(situation=="unemp", na.rm=T)/n(),
            self_emp= ifelse(sum(situation!= "unemp", na.rm=T) ==0, 0, sum(situation=="self-emp", na.rm=T)/sum(situation!="unemp", na.rm=T))
            
  )


df1<-df1 %>% 
  mutate(year= yearmonth %/% 100) %>% 
  group_by(sector, year) %>%
  mutate(yearproj=mean(project_based, na.rm = T))
  

df2<-df1 %>% 
  group_by(yearmonth) %>% 
  mutate(treatment=ifelse(yearproj>median(yearproj), 1, 0)) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(treatment2=ifelse(any(yearmonth==202112 & treatment==1), 1,0))

df2$time2<-as.Date(paste0(as.character(df2$yearmonth),"01"), format="%Y%m%d")

df2$year<- df2$yearmonth %/% 100
df2$month<-as.numeric(substr(df2$yearmonth,5,6))
df2$timeto<- (df2$year - 2021) * 12 + df2$month-13
df2<-df2 %>% filter(!is.na(sector))

df2$inttreat<-ifelse(df2$timeto>=0 & df2$treatment2 ==1, 1, 0) 


labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")
variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")

models<-list()
plots<-list()
for (variable in variable_names) {
  # Create gsynth model
  model <- gsynth(as.formula(paste0(variable, " ~ inttreat")), data = df2, 
                  index = c("sector", "timeto"), force = "two-way", 
                  CV = TRUE, se = TRUE, 
                  inference = "parametric", nboots = 1000, 
                  parallel = FALSE)
  
  # Store the model
  models[[variable]] <- model
  
  # Generate plot and store it
  plots[[variable]] <- plot(model, main = labs[[variable]],cex.main=.5)
}

bysectorgrid<-gridExtra::grid.arrange(grobs = plots, ncol = 2)
ggsave2(bysectorgrid, file=paste0("../../../../../../Plots/rc/synth/bysector", x,".jpeg"), width = 6, height = 8)

######### By profession ############
df1<-df %>% 
  filter(!is.na(occupation)) %>% 
  group_by(occupation, yearmonth) %>% 
  summarise(days_worked=mean(days_spell, na.rm=T),
            salaries=mean(income, na.rm=T),
            ncontracts=mean(ncontracts, na.rm = T),
            days_worked=mean(days_spell, na.rm = T),
            internship = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                0,
                                sum(situation == "Internship or training") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            open_ended = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                0,
                                sum(situation == "open-ended") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            other_temporary = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                     0,
                                     sum(situation == "other temporary") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            permanent = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                               0,
                               sum(situation == "permanent") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            pre_retirement = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                    0,
                                    sum(situation == "Pre-retirement") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            production_circumstances = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                              0,
                                              sum(situation == "production circumstances") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            project_based = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                   0,
                                   sum(situation == "project-based") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            replacement = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                 0,
                                 sum(situation == "Replacement") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            other= ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                          0,
                          sum(situation == "other") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            unemployed= sum(situation=="unemp", na.rm=T)/n(),
            self_emp= ifelse(sum(situation!= "unemp", na.rm=T) ==0, 0, sum(situation=="self-emp", na.rm=T)/sum(situation!="unemp", na.rm=T))
            
  )


df2<-df1 %>% 
  group_by(yearmonth) %>% 
  mutate(treatment=ifelse(project_based>median(project_based), 1, 0)) %>% 
  ungroup() %>% 
  group_by(occupation) %>% 
  mutate(treatment2=ifelse(any(yearmonth==202112 & treatment==1), 1,0))

df2$time2<-as.Date(paste0(as.character(df2$yearmonth),"01"), format="%Y%m%d")

df2$year<- df2$yearmonth %/% 100
df2$month<-as.numeric(substr(df2$yearmonth,5,6))
df2$timeto<- (df2$year - 2021) * 12 + df2$month-13

df2$inttreat<-ifelse(df2$timeto>=0 & df2$treatment2 ==1, 1, 0) 



variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")


models2<-list()
plots2<-list()
for (variable in variable_names) {
  # Create gsynth model
  model <- gsynth(as.formula(paste0(variable, " ~ inttreat")), data = df2, 
                  index = c("occupation", "timeto"), force = "two-way", 
                  CV = TRUE, se = TRUE, 
                  inference = "parametric", nboots = 1000, 
                  parallel = FALSE)
  
  # Store the model
  models2[[variable]] <- model
  
  # Generate plot and store it
  plots2[[variable]] <- plot(model, main = variable)
}

byoccgrid<-gridExtra::grid.arrange(grobs = plots2, ncol = 2)
ggsave2(byoccgrid, file=paste0("../../../../../../Plots/rc/synth/byocc", x,".jpeg"), width = 6, height = 8)



############# DENSITY PLOTS FOR PB ONLY ######################


load("manageable_df.Rdata")
load("manageable_dfincome.Rdata")
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Functions.R")

df1 <- df %>%
  filter(time %in% c(37:47,25:35) & yearmonth == exit_month & situation == "project-based") %>% 
  mutate(treatment= as.factor(ifelse(time %in% c(37:47), "2022", "2021")))


df1$days_spell[is.na(df1$days_spell)]<-0
df1$ncontracts[df1$ncontracts==0]<-NA

df1<-merge(df1, dfincome, by=c("person_id", "time"), all.x = T )

df1$income[is.na(df1$income)] <- 0

df1$income=df1$income/100

df1$person_muni_latest <- replace_province(df1$person_muni_latest)

shapefile_provincias <- st_read("provincias/Provincias_ETRS89_30N.shp")

shapefile_provincias<-as_Spatial(shapefile_provincias)

nombres_provincias <- tibble(provincia=shapefile_provincias$Texto,
                             id=as.numeric(shapefile_provincias$Codigo))

df1<-df1 %>% 
  mutate(id=as.numeric(person_muni_latest))

df1<-left_join(df1, nombres_provincias, by="id") 

df1$sector <- ifelse(df1$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
                                     61, 62, 71,72, 81, 89, 91, 99),
                    paste0("0", df1$sector),
                    df1$sector)


df1$sector<-substr(df1$sector, 1,2)


df1<-df1 %>%
  mutate(group3=as.numeric(sector),
         sector=case_when(group3 %in% c(1:3) ~ "AGRICULTURE, FORESTRY AND FISHING",
                           group3 %in% c(5:9) ~ "MINING AND QUARRYING",
                           group3 %in% c(10:33) ~ "MANUFACTURING",
                           group3 ==35 ~ "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
                           group3 %in% c(36:39)~"WATER SUPPLY AND WASTE MANAGEMENT",
                           group3 %in% c(41:43) ~ "CONSTRUCTION",
                           group3 %in% c(45:47) ~ "WHOLESALE & REPAIR OF MOTOR VEHICLES",
                           group3 %in% c(49:53) ~ "TRANSPORTATION AND STORAGE",
                           group3 %in% c(55,56) ~ "ACCOMMODATION AND FOOD SERVICE",
                           group3 %in% c(58:63) ~ "INFORMATION AND COMMUNICATION",
                           group3 %in% c(64:66)~ "FINANCIAL AND INSURANCE ACTIVITIES",
                           group3 == 68 ~ "REAL ESTATE",
                           group3 %in% c(69:75)~ "PROFESSIONAL, SCIENTIFIC AND TECHNICAL",
                           group3 %in% c(77:82) ~ "ADMINISTRATIVE & SUPPORT SERVICE ACTIVITIES",
                           group3==84 ~ "PUBLIC ADMINISTRATION AND DEFENCE",
                           group3==85 ~ "EDUCATION",
                           group3 %in% c(86:88) ~ "HUMAN HEALTH AND SOCIAL WORK",
                           group3 %in% c(90:93) ~ "ARTS, ENTERTAINMENT AND RECREATION",
                           group3 %in% c(94,95) ~ "OTHER SERVICES",
                           group3 %in% c(96:98) ~ "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS",
                           group3 == 99 ~ "EXTRATERRITORIAL ORGANIZATIONS ACTIVITIES",
                           TRUE ~ NA_character_
         ))


df1$age<-df1$yearmonth%/%100- df1$birth_date %/% 100


df1$age_group[df1$age<25]<- "<25"
df1$age_group[df1$age %in% c(25:34)]<- "25-34"
df1$age_group[df1$age %in% c(35:44)]<- "35-44"
df1$age_group[df1$age %in% c(45:54)]<- "45-54"
df1$age_group[df1$age %in% c(55:64)]<- "55-64"
df1$age_group[df1$age > 64]<- "65+"


df1<-df1 %>% 
  mutate(group=occupation,
occupation= case_when(
  group == 1 ~ "Engineers/Top Management",
  group == 2 ~ "Technical Engineers/Experts",
  group == 3 ~ "Managers",
  group == 4 ~ "UntitledAssistants",
  group == 5 ~ "Administrative Officers",
  group == 6 ~ "Subordinates",
  group == 7 ~ "Administrative Assistants",
  group == 8 ~ "1st 2nd Grade Officers",
  group == 9 ~ "3rd Grade Officers/Specialists",
  group == 10 ~ "Unqualified +18",
  group ==11 ~ "<18 years old",
  TRUE ~ NA_character_
))

df1<-df1 %>% 
  mutate(gender=ifelse(sex==1, "Men", "Women"))



df2<-df1 %>%
  mutate(ncontracts=ifelse(ncontracts>10, 10,ncontracts),
         income= ifelse(income>4000, 4000, income)) %>% 
  pivot_longer(cols = c("days_spell","ncontracts","income"),names_to = "var", values_to = "value") 


gg<-
  df2 %>% 
  ggplot(aes(x=value,color= stage(as.factor(treatment), after_scale = alpha(color, 1)),
             fill= stage(as.factor(treatment), after_scale= alpha(fill, .2))
             ))+
  geom_density(linewidth=.5)+
  facet_wrap(~var, scales = "free", nrow = 3)+
  scale_x_continuous(
    limits = ~ c(min(.x), ceiling(max(.x))),
    breaks = ~ seq(min(.x), max(.x), length.out = 5),
    expand = c(0, 0),
    labels = scales::number_format(scale = 1) 
  )+
    scale_color_manual(values = c("#0e387a","#008080"))+
    scale_fill_manual(values = c("#0e387a","#008080"))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

ggsave2(gg, file=paste0("../../../../../../Plots/rc/bc/pbonly.jpeg"), width = 7, height = 5)

df3<-df1 %>% 
  mutate(occupation=as.factor(occupation), 
         sector=as.factor(sector), 
         age_group=as.factor(age_group), 
         gender=as.factor(gender),
         person_muni_latest=as.factor(person_muni_latest)) %>% 
  pivot_longer(cols = c("occupation", "sector", "age_group", "gender", "provincia"))

df3 <- df3 %>%
  group_by(name,treatment) %>%
  mutate(totalworkers = n()) %>% 
  group_by(treatment,name,value) %>% 
  summarise(share=n()/mean(totalworkers))

g1<-
  df3 %>% 
  filter(!is.na(value), name!="sector") %>% 
  ggplot(aes(y=value,
             x=share,  
             color= stage(as.factor(treatment), after_scale = alpha(color, 1)),
             fill= stage(as.factor(treatment), after_scale= alpha(fill, .7))
             ))+
  geom_bar(position="dodge2", stat="identity")+
  facet_wrap(~name, scales = "free", nrow = 3)+
  scale_x_continuous(labels = scales::percent)+
    scale_color_manual(values = c("#0e387a","#008080"))+
    scale_fill_manual(values = c("#0e387a","#008080"))+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank())

g2<-
  df3 %>% 
  filter(!is.na(value), name=="sector") %>% 
    ggplot(aes(y=value,
               x=share,  
               color= stage(as.factor(treatment), after_scale = alpha(color, 1)),
               fill= stage(as.factor(treatment), after_scale= alpha(fill, .7))
    ))+
    geom_bar(position="dodge2", stat="identity")+
    facet_wrap(~name, scales = "free", nrow = 3)+
    scale_x_continuous(labels = scales::percent)+
    scale_color_manual(values = c("#0e387a","#008080"))+
    scale_fill_manual(values = c("#0e387a","#008080"))+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank())

gg<-plot_grid(g1+theme(legend.position = "none"), g2, nrow = 2, rel_widths = c(1,2), rel_heights = c(2,1))

ggsave2(gg, file=paste0("../../../../../../Plots/rc/bc/pbonly2.jpeg"), width = 9, height = 15)


########Balancing checks for everyone ###############

df1 <- df %>%
  filter(time %in% c(37:47,25:35) & yearmonth == exit_month) %>% 
  mutate(treatment= as.factor(ifelse(time %in% c(37:47), "2022", "2021")))


df1$days_spell[is.na(df1$days_spell)]<-0
df1$ncontracts[df1$ncontracts==0]<-NA

df1<-merge(df1, dfincome, by=c("person_id", "time"), all.x = T )

df1$income[is.na(df1$income)] <- 0

df1$income=df1$income/100

df1$person_muni_latest <- replace_province(df1$person_muni_latest)

shapefile_provincias <- st_read("provincias/Provincias_ETRS89_30N.shp")

shapefile_provincias<-as_Spatial(shapefile_provincias)

nombres_provincias <- tibble(provincia=shapefile_provincias$Texto,
                             id=as.numeric(shapefile_provincias$Codigo))

df1<-df1 %>% 
  mutate(id=as.numeric(person_muni_latest))

df1<-left_join(df1, nombres_provincias, by="id") 

df1$sector <- ifelse(df1$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
                                       61, 62, 71,72, 81, 89, 91, 99),
                     paste0("0", df1$sector),
                     df1$sector)


df1$sector<-substr(df1$sector, 1,2)


df1<-df1 %>%
  mutate(group3=as.numeric(sector),
         sector=case_when(group3 %in% c(1:3) ~ "AGRICULTURE, FORESTRY AND FISHING",
                          group3 %in% c(5:9) ~ "MINING AND QUARRYING",
                          group3 %in% c(10:33) ~ "MANUFACTURING",
                          group3 ==35 ~ "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
                          group3 %in% c(36:39)~"WATER SUPPLY AND WASTE MANAGEMENT",
                          group3 %in% c(41:43) ~ "CONSTRUCTION",
                          group3 %in% c(45:47) ~ "WHOLESALE & REPAIR OF MOTOR VEHICLES",
                          group3 %in% c(49:53) ~ "TRANSPORTATION AND STORAGE",
                          group3 %in% c(55,56) ~ "ACCOMMODATION AND FOOD SERVICE",
                          group3 %in% c(58:63) ~ "INFORMATION AND COMMUNICATION",
                          group3 %in% c(64:66)~ "FINANCIAL AND INSURANCE ACTIVITIES",
                          group3 == 68 ~ "REAL ESTATE",
                          group3 %in% c(69:75)~ "PROFESSIONAL, SCIENTIFIC AND TECHNICAL",
                          group3 %in% c(77:82) ~ "ADMINISTRATIVE & SUPPORT SERVICE ACTIVITIES",
                          group3==84 ~ "PUBLIC ADMINISTRATION AND DEFENCE",
                          group3==85 ~ "EDUCATION",
                          group3 %in% c(86:88) ~ "HUMAN HEALTH AND SOCIAL WORK",
                          group3 %in% c(90:93) ~ "ARTS, ENTERTAINMENT AND RECREATION",
                          group3 %in% c(94,95) ~ "OTHER SERVICES",
                          group3 %in% c(96:98) ~ "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS",
                          group3 == 99 ~ "EXTRATERRITORIAL ORGANIZATIONS ACTIVITIES",
                          TRUE ~ NA_character_
         ))


df1$age<-df1$yearmonth%/%100- df1$birth_date %/% 100


df1$age_group[df1$age<25]<- "<25"
df1$age_group[df1$age %in% c(25:34)]<- "25-34"
df1$age_group[df1$age %in% c(35:44)]<- "35-44"
df1$age_group[df1$age %in% c(45:54)]<- "45-54"
df1$age_group[df1$age %in% c(55:64)]<- "55-64"
df1$age_group[df1$age > 64]<- "65+"


df1<-df1 %>% 
  mutate(group=occupation,
         occupation= case_when(
           group == 1 ~ "Engineers/Top Management",
           group == 2 ~ "Technical Engineers/Experts",
           group == 3 ~ "Managers",
           group == 4 ~ "UntitledAssistants",
           group == 5 ~ "Administrative Officers",
           group == 6 ~ "Subordinates",
           group == 7 ~ "Administrative Assistants",
           group == 8 ~ "1st 2nd Grade Officers",
           group == 9 ~ "3rd Grade Officers/Specialists",
           group == 10 ~ "Unqualified +18",
           group ==11 ~ "<18 years old",
           TRUE ~ NA_character_
         ))

df1<-df1 %>% 
  mutate(gender=ifelse(sex==1, "Men", "Women"))



df2<-df1 %>%
  mutate(ncontracts=ifelse(ncontracts>10, 10,ncontracts),
         income= ifelse(income>6000, 6000, income)) %>% 
  pivot_longer(cols = c("days_spell","ncontracts","income"),names_to = "var", values_to = "value") 

labs1<-c("N. days worked", "Income", "N.contracts", "Open-ended", "Permanent", "Project-based", "Self-employment", "Not employed")
names(labs1)<-c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")


gg<-
  df2 %>% 
  ggplot(aes(x=value,color= stage(as.factor(treatment), after_scale = alpha(color, 1)),
             fill= stage(as.factor(treatment), after_scale= alpha(fill, .2))
  ))+
  geom_density(linewidth=.5)+
  facet_wrap(~var, scales = "free", nrow = 3)+
  scale_x_continuous(
    limits = ~ c(min(.x), ceiling(max(.x))),
    breaks = ~ seq(min(.x), max(.x), length.out = 5),
    expand = c(0, 0),
    labels = scales::number_format(scale = 1) 
  )+
  scale_color_manual(values = c("#0e387a","#008080"))+
  scale_fill_manual(values = c("#0e387a","#008080"))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

ggsave2(gg, file=paste0("../../../../../../Plots/rc/bc/alldensity.jpeg"), width = 7, height = 5)

df3<-df1 %>% 
  mutate(occupation=as.factor(occupation), 
         sector=as.factor(sector), 
         age_group=as.factor(age_group), 
         gender=as.factor(gender),
         person_muni_latest=as.factor(person_muni_latest)) %>% 
  pivot_longer(cols = c("occupation", "sector", "age_group", "gender", "provincia"))

df3 <- df3 %>%
  group_by(name,treatment) %>%
  mutate(totalworkers = n()) %>% 
  group_by(treatment,name,value) %>% 
  summarise(share=n()/mean(totalworkers))

g1<-
  df3 %>% 
  filter(!is.na(value), name!="sector") %>% 
  ggplot(aes(y=value,
             x=share,  
             color= stage(as.factor(treatment), after_scale = alpha(color, 1)),
             fill= stage(as.factor(treatment), after_scale= alpha(fill, .7))
  ))+
  geom_bar(position="dodge2", stat="identity")+
  facet_wrap(~name, scales = "free", nrow = 3)+
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(values = c("#0e387a","#008080"))+
  scale_fill_manual(values = c("#0e387a","#008080"))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

g2<-
  df3 %>% 
  filter(!is.na(value), name=="sector") %>% 
  ggplot(aes(y=value,
             x=share,  
             color= stage(as.factor(treatment), after_scale = alpha(color, 1)),
             fill= stage(as.factor(treatment), after_scale= alpha(fill, .7))
  ))+
  geom_bar(position="dodge2", stat="identity")+
  facet_wrap(~name, scales = "free", nrow = 3)+
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(values = c("#0e387a","#008080"))+
  scale_fill_manual(values = c("#0e387a","#008080"))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

gg<-plot_grid(g1+theme(legend.position = "none"), g2, nrow = 2, rel_widths = c(1,2), rel_heights = c(2,1))

ggsave2(gg, file=paste0("../../../../../../Plots/rc/bc/all2.jpeg"), width = 9, height = 15)


