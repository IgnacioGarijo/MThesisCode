library(haven)
library(data.table)
library(tidyverse)
library(lubridate) #to calculate differences between dates
library(zoo) 
library(cowplot) #To save plots
#library(didimputation)
library(fixest) #For feols
library(ggfixest) #To plot the DiD with ggiplot

theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")


#Loading dataframes

load("finalpanel2019b.Rdata")
setDT(df)



#Loading income dfs and cleaning so to have only the information needed

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


min_year <- 2019
max_year <- 2022
min_month <- 1
max_month <- 12


contr1<- loadRData("processed_contribution_1.Rdata")
contr1 <- contr1[contr1$year >= min_year , ]
contr1$month <- as.numeric(contr1$month)
contr1$time <- (contr1$year - min(contr1$year)) * 12 + (contr1$month - min(contr1$month) + 1)
contr1 <- contr1[, c("person_id", "time", "income")]

contr2<- loadRData("processed_contribution_2.Rdata")

contr2 <- contr2[contr2$year >= min_year, ]
contr2$month <- as.numeric(contr2$month)
contr2$time <- (contr2$year - min(contr2$year)) * 12 + (contr2$month - min(contr2$month) + 1)
contr2 <- contr2[, c("person_id", "time", "income")]

contr3<- loadRData("processed_contribution_3.Rdata")

contr3 <- contr3[contr3$year  >= min_year, ]
contr3$month <- as.numeric(contr3$month)
contr3$time <- (contr3$year - min(contr3$year)) * 12 + (contr3$month - min(contr3$month) + 1)
contr3 <- contr3[, c("person_id", "time", "income")]

contr4<- loadRData("processed_contribution_4.Rdata")

contr4 <- contr4[contr4$year  >= min_year, ]
contr4$month <- as.numeric(contr4$month)
contr4$time <- (contr4$year - min(contr4$year)) * 12 + (contr4$month - min(contr4$month) + 1)
contr4 <- contr4[, c("person_id", "time", "income")]


contr5<- loadRData("processed_contribution_5.Rdata")

contr5 <- contr5[contr5$year  >= min_year, ]
contr5$month <- as.numeric(contr5$month)
contr5$time <- (contr5$year - min(contr5$year)) * 12 + (contr5$month - min(contr5$month) + 1)
contr5 <- contr5[, c("person_id", "time", "income")]

contr6<- loadRData("processed_contribution_6.Rdata")

contr6 <- contr6[contr6$year  >= min_year, ]
contr6$month <- as.numeric(contr6$month)
contr6$time <- (contr6$year - min(contr6$year)) * 12 + (contr6$month - min(contr6$month) + 1)
contr6 <- contr6[, c("person_id", "time", "income")]

contr7<- loadRData("processed_contribution_7.Rdata")

contr7 <- contr7[contr7$year  >= min_year, ]
contr7$month <- as.numeric(contr7$month)
contr7$time <- (contr7$year - min(contr7$year)) * 12 + (contr7$month - min(contr7$month) + 1)
contr7 <- contr7[, c("person_id", "time", "income")]

contr8<- loadRData("processed_contribution_8.Rdata")

contr8 <- contr8[contr8$year  >= min_year, ]
contr8$month <- as.numeric(contr8$month)
contr8$time <- (contr8$year - min(contr8$year)) * 12 + (contr8$month - min(contr8$month) + 1)
contr8 <- contr8[, c("person_id", "time", "income")]

contr9<- loadRData("processed_contribution_9.Rdata")

contr9 <- contr9[contr9$year  >= min_year, ]
contr9$month <- as.numeric(contr9$month)
contr9$time <- (contr9$year - min(contr9$year)) * 12 + (contr9$month - min(contr9$month) + 1)
contr9 <- contr9[, c("person_id", "time", "income")]

contr10<- loadRData("processed_contribution_10.Rdata")

contr10 <- contr10[contr10$year  >= min_year, ]
contr10$month <- as.numeric(contr10$month)
contr10$time <- (contr10$year - min(contr10$year)) * 12 + (contr10$month - min(contr10$month) + 1)
contr10 <- contr10[, c("person_id", "time", "income")]

contr11<- loadRData("processed_contribution_11.Rdata")

contr11 <- contr11[contr11$year  >= min_year, ]
contr11$month <- as.numeric(contr11$month)
contr11$time <- (contr11$year - min(contr11$year)) * 12 + (contr11$month - min(contr11$month) + 1)
contr11 <- contr11[, c("person_id", "time", "income")]

contr12<- loadRData("processed_contribution_12.Rdata")

contr12 <- contr12[contr12$year  >= min_year, ]
contr12$month <- as.numeric(contr12$month)
contr12$time <- (contr12$year - min(contr12$year)) * 12 + (contr12$month - min(contr12$month) + 1)
contr12 <- contr12[, c("person_id", "time", "income")]

dfincome<-rbind(contr1, contr2, contr3, contr4, contr5, contr6, contr7, contr8, contr9, contr10, contr11, contr12)

rm(contr1, contr2, contr3, contr4, contr5, contr6, contr7, contr8, contr9, contr10, contr11, contr12)



#Now let's start


setDT(df)

#Reducing the df to make it easier to manage

df<-df[,c("person_id", "exit_date", "entry_date", "year", "month", "days_spell", "job_relationship", "regime", "contr_type", "ncontracts")]
df<-df[df$year>=min_year]

#First let's create the time variable for the cohorts



df[, c("time", "yearmonth", "exit_month"):= list((year - min_year) * 12 + (month - min_month + 1),
                                                 as.numeric(paste0(year, sprintf("%02d", month))),
                                                 exit_date %/% 100)]



df<-df %>% select(-c(exit_date, year, month))



aggregated_cohort<-function(df, group_variable){
  
min_time<- min(df$time)
max_time<-max(df$time)

#Loop from here
for (i in min_time:max_time){
  df1<-df %>% 
    group_by(person_id) %>% 
    mutate(treatment = ifelse(any(time==i & yearmonth==exit_month), 
                              1, 
                              NA)) %>% 
    filter(!is.na(treatment)) %>% 
    select(-treatment)
  
  #Now let's create a complete grid for the individuals so there is one observation per person per month
  
  complete_grid<-expand.grid(person_id=unique(df1$person_id),
                             time=i:(i+6)) 
  
  df1<-merge(complete_grid, df1, by=c("person_id", "time"), all.x = TRUE)
  
  
  df1$days_spell[is.na(df1$days_spell)]<-0
  df1$ncontracts[is.na(df1$ncontracts)]<-0
  
  
  setDT(df1)
  
  
  #Then let's define the situation of each person and eliminate unnecesary columns
  
  
  df1[, emp:= ifelse(!is.na(contr_type)| job_relationship %in% c(87,901,902,910,930,932,937,951),1,0)]
  
  df1[,  situation:= ifelse(!is.na(contr_type),
                            contr_type,
                            ifelse(regime %in% c(500:540, 721:740),
                                   "self-emp",ifelse(emp==1, "other",
                                                     "unemp")
                            )
  )
  ]
  
  df1$situation[is.na(df1$situation)]<-"unemp"
  
  df1$regime<-NULL
  df1$job_relationship<-NULL
  df1$contr_type<-NULL
  df1$yearmonth<-NULL
  df1$exit_month<-NULL
  
  #Let´s also merge with income data 
  
  df1<-merge(df1, dfincome, by=c("person_id", "time"), all.x = T )
  
  df1$income[is.na(df1$income)] <- 0
  
df1<-df1 %>% 
  group_by(person_id) %>%
  mutate(group= situation[time==i])
  
  
  df1<-df1 %>%
    group_by(time, group) %>%
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
              self_emp= ifelse(sum(situation!= "unemp", na.rm=T) ==0, 0, sum(situation=="self-emp", na.rm=T)/sum(situation!="unemp", na.rm=T)),
              cohort=i
              
    )
  group_variable_name <- deparse(substitute(group_variable))
  save(df1, file=paste0(group_variable_name,"_cohort_", i, ".Rdata"))
}


load(paste0(group_variable_name,"_cohort_1.Rdata"))
dff<-df1

min_timeplus<-min_time +1
for (i in min_timeplus:max_time){
  load(paste0(group_variable_name,"_cohort_", i, ".Rdata"))
  dff<-rbind(df1, dff)
  gc()
}

save(dff, file= paste0(group_variable_name,"_cohort_panel.Rdata"))

}


aggregated_cohort(df, situation)
