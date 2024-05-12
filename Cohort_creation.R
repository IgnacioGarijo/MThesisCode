## COHORT CREATION ###



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
library(pROC)
library(ggpubr) #To grid the plots

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")







# load("finalpanel2019c.Rdata")
# 
# dfincome<-create_income_df(min_year=2019)
# 
# df<-create_manageable_df(df, min_year = 2019)
# 
# save(df, file = "manageable_df.Rdata")
# save(dfincome, file="manageable_dfincome.Rdata")

# load("manageable_df.Rdata")
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
# save(df,file="manageable_df.Rdata")


rm(list = ls())
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")


load("manageable_df.Rdata")
load("manageable_dfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)

create_cohort(df, name="fromanykind3")


############# BY CONTRACT ##########
rm(list=ls())
gc()
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6

load("manageable_df.Rdata")
load("manageable_dfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)

create_cohort(df, aggregation = "situation", name = "bysituation")

############# BY GENDER ##########


rm(list=ls())
gc()
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")


load("manageable_df.Rdata")
load("manageable_dfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)


create_cohort(df, name = "bygender", aggregation = "sex")


################ FOURTH: ANALYSIS BY PROVINCE ##############

rm(list=ls())
gc()
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")


load("manageable_df.Rdata")
df$person_muni_latest <- replace_province(df$person_muni_latest)


load("manageable_dfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)

create_cohort(df, aggregation="person_muni_latest", name="byregion")


################ FIFTH: BY OCCUPATION ###################
rm(list=ls())
gc()
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")


load("manageable_df.Rdata")
load("manageable_dfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)



create_cohort(df, aggregation = "occupation", name="byoccupation")


############### SIXTH: BY SECTOR2 #######################
rm(list=ls())
gc()
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")


load("manageable_df.Rdata")


df<-df[,-c("entry_date", "sex", "person_muni_latest", "birth_date")]

df$sector <- ifelse(df$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
                                     61, 62, 71,72, 81, 89, 91, 99),
                    paste0("0", df$sector),
                    df$sector)


df$sector<-substr(df$sector, 1,2)


df<-df %>%
  mutate(group3=as.numeric(sector),
         sector2=case_when(group3 %in% c(1:3) ~ "AGRICULTURE, FORESTRY AND FISHING",
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






load("manageable_dfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)

create_cohort(df, aggregation="sector2", name="bysector2", pbonly = F)


####################SEVENTH: BY AGE GROUP ##################
rm(list=ls()) 
gc()
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")


load("manageable_df.Rdata")

df$age<-df$yearmonth%/%100- df$birth_date %/% 100

df<-df %>% 
  select(-c( occupation, sector,birth_date,sex, person_muni_latest))


df$age_group[df$age<25]<- "<25"
df$age_group[df$age %in% c(25:34)]<- "25-34"
df$age_group[df$age %in% c(35:44)]<- "35-44"
df$age_group[df$age %in% c(45:54)]<- "45-54"
df$age_group[df$age %in% c(55:64)]<- "55-64"
df$age_group[df$age > 64]<- "65+"


load("manageable_dfincome.Rdata")
min_time<- min(df$time)
max_time<-max(df$time)



create_cohort(df, aggregation = "age_group", name = "byagegroup")

