library(haven)
library(data.table)
library(tidyverse)

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")

##################CLEANING THE FILES FROM STATA AND PREPARING THEM TO CREATE COHORT PANELS################

#### 1. Cleaning individuals data and creating a df that maps individuals to their birthyear

# indiv<- read_dta("individuals.dta")
#
# indiv<-indiv %>% select(-death_year_month)
# indiv<-indiv %>%
#   filter(birth_date %in% c(195700:200612))
#
# dfbirth<-indiv[1:2]
# save(dfbirth, file = "dfbirth.Rdata")
#
# save(indiv, file = "indiv.Rdata")
#

#### 2. Cleaning affiliates data getting rid of observations and variables that are not needed


# aff1<- read_dta("../mcvl_affiliates_1.dta")
# aff1 <- aff1 %>%
#   mutate_at(vars(contract_type, occupation, entry_date, exit_date, reason_dismissal, firm_muni, sector, job_relationship, firm_ett, firm_jur_status),
#             ~ifelse(. == 0, NA, .)) %>%
#   select(-sector_cnae93) %>%
#   filter(!is.na(entry_date), !is.na(exit_date))


# save(aff1, file = "aff1.Rdata")

#
# aff2<-read_dta("../mcvl_affiliates_2.dta")
#
# aff2 <- aff2 %>%
#   mutate_at(vars(contract_type, occupation, entry_date, exit_date, reason_dismissal, firm_muni, sector, job_relationship, firm_ett, firm_jur_status),
#             ~ifelse(. == 0, NA, .)) %>%
#   select(-sector_cnae93)%>%
#   filter(!is.na(entry_date), !is.na(exit_date))
#
# save(aff2, file ="aff2.Rdata")

#
# aff3<- read_dta("../mcvl_affiliates_3.dta")

# aff3 <- aff3 %>%
#   mutate_at(vars(contract_type, occupation, entry_date, exit_date, reason_dismissal, firm_muni, sector, job_relationship, firm_ett, firm_jur_status),
#             ~ifelse(. == 0, NA, .)) %>%
#   select(-sector_cnae93)%>%
#   filter(!is.na(entry_date), !is.na(exit_date))
#
# save(aff3, file="aff3.Rdata")
#
# aff4<- read_dta("../mcvl_affiliates_4.dta")

# aff4 <- aff4 %>%
#   mutate_at(vars(contract_type, occupation, entry_date, exit_date, reason_dismissal, firm_muni, sector, job_relationship, firm_ett, firm_jur_status),
#             ~ifelse(. == 0, NA, .)) %>%
#   select(-sector_cnae93)%>%
#   filter(!is.na(entry_date), !is.na(exit_date))
#
#
# save(aff4, file="aff4.Rdata")


#### 3. Cleaning contributions data and turning it into 12 different panels with monthly income information since 2015.
#Then creating one df per cohort, which will facilitate the merging later to produce the final panel.


###### 3.1 Creating the 12 panels

# load("dfbirth.Rdata")
# contr13<-read_dta("contribution_13.dta")
#
# for (i in 1:12) {
#   file_name <- paste0("contribution_", i, ".dta")
#   dfcontr <- read_dta(file_name)
#   dfcontr <- rbind(dfcontr, contr13[contr13$person_id %in% dfcontr$person_id, ])
#   dfcontr<- left_join(dfcontr, dfbirth)
#   setDT(dfcontr)
#   dfcontr<-dfcontr[year>2015]
#   dfcontr<-dfcontr[birth_date %in% c(195700:200612)]
#   dfcontr<-dfcontr %>% pivot_longer(names_to = "month", cols = c(4:15))
# setDT(dfcontr)
# dfcontr[,month := case_when(month == "earnings_jan" ~ "01",
#                                               month == "earnings_feb" ~ "02",
#                                               month == "earnings_mar" ~ "03",
#                                               month == "earnings_apr" ~ "04",
#                                               month == "earnings_may" ~ "05",
#                                               month == "earnings_jun" ~ "06",
#                                               month == "earnings_jul" ~ "07",
#                                               month == "earnings_aug" ~ "08",
#                                               month == "earnings_sep" ~ "09",
#                                               month == "earnings_oct" ~ "10",
#                                               month == "earnings_nov" ~ "11",
#                                               month == "earnings_dec" ~ "12",
#                                               TRUE ~ "Unknown")]
# dfcontr[, date:= as.numeric(paste0(year, month))]
#
#
#   dfcontr<-dfcontr[value>0] #Eliminate residual negative numbers and 0 as they are not useful
#   setorder(dfcontr, person_id, date, value)
#   #Identifying repeated observations and eliminating them
#   dfcontr[, repeated:= ifelse(value==lag(value) & person_id ==lag(person_id) & date== lag(date), 1, 0)]
#   dfcontr<-dfcontr[repeated==0]
#
#   #Keeping the sum of the incomes received that month and eliminating the other observations so there is one observation per individual and date
#   dfcontr<-dfcontr[, c("income", "selection"):= list(sum(value),
#                                                    ifelse(value==max(value),1,0)), by=.(person_id, date)]
#
#   dfcontr<-dfcontr[selection==1]
#   dfcontr<-dfcontr %>% select(-c(selection, repeated, value))
#
#   save(dfcontr, file = paste0("processed_contribution_", i, ".rdata"))
#   rm(dfcontr, file_name)
# }
#
#
# #This functions allows loading data with the name we want for the dataframe
#
# loadRData <- function(fileName){
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
#
#
# ###### 3.3 Creating each cohort panel
#
# contr1<- loadRData("processed_contribution_1.Rdata")
# contr2<- loadRData("processed_contribution_2.Rdata")
# contr3<- loadRData("processed_contribution_3.Rdata")
# contr4<- loadRData("processed_contribution_4.Rdata")
# contr5<- loadRData("processed_contribution_5.Rdata")
# contr6<- loadRData("processed_contribution_6.Rdata")
# contr7<- loadRData("processed_contribution_7.Rdata")
# contr8<- loadRData("processed_contribution_8.Rdata")
# contr9<- loadRData("processed_contribution_9.Rdata")
# contr10<- loadRData("processed_contribution_10.Rdata")
# contr11<- loadRData("processed_contribution_11.Rdata")
# contr12<- loadRData("processed_contribution_12.Rdata")
#
#
# contr_cohorts<-function(year){
#   start<-year*100
#   end<-start+12
#   period<- c(start:end)
# cohortcontr1<-contr1[contr1$birth_date %in% period]
# cohortcontr2<-contr2[contr2$birth_date %in% period]
# cohortcontr3<-contr3[contr3$birth_date %in% period]
# cohortcontr4<-contr4[contr4$birth_date %in% period]
# cohortcontr5<-contr5[contr5$birth_date %in% period]
# cohortcontr6<-contr6[contr6$birth_date %in% period]
# cohortcontr7<-contr7[contr7$birth_date %in% period]
# cohortcontr8<-contr8[contr8$birth_date %in% period]
# cohortcontr9<-contr9[contr9$birth_date %in% period]
# cohortcontr10<-contr10[contr10$birth_date %in% period]
# cohortcontr11<-contr11[contr11$birth_date %in% period]
# cohortcontr12<-contr12[contr12$birth_date %in% period]
# cohort_contr<- rbind(cohortcontr1, cohortcontr2, cohortcontr3, cohortcontr4, cohortcontr5, cohortcontr6, cohortcontr7, cohortcontr8, cohortcontr9,
#       cohortcontr10, cohortcontr11, cohortcontr12)
# save(cohort_contr, file = paste0("contrcohort", year, ".Rdata"))
# }
#
# for (i in c(1957:2006)){
# contr_cohorts(i)
# }


######### CLEANING FISCAL DATA ###########

# dffiscal<- read.csv("../../mcvl/mcvl_cdf_2022/MCVL2022FISCAL_CDF.TXT", header=FALSE, sep=";")
# load("dfbirth.Rdata")
#
# dffiscal<-dffiscal %>%
#   transmute(person_id= V1,
#             province_resid=V4,
#             payment_type=V5,
#             payment_amount= V7,
#             marital_status =V19,
#             children_below3 =V27,
#             children=V37)
#
#
#
# dffiscal<-left_join(dfbirth, dffiscal)
#
# setDT(dffiscal1)
#
#
# dffiscal<-dffiscal %>%
#   group_by(person_id) %>%
#   mutate(selection= ifelse(payment_amount==max(payment_amount),1,0),
#          total_payment=sum(payment_amount),
#          pension= ifelse(payment_type=="B", payment_amount, 0),
#          salaries=ifelse(payment_type!= "B" & payment_type !="C" & payment_type != "D", payment_amount, 0),
#          unemp_benefits= ifelse(payment_type=="C" | payment_type=="D", payment_amount, 0),
#          pension= sum(pension),
#          salaries=sum(salaries),
#          unemp_benefits=sum(unemp_benefits)
#          ) %>%
#   select(-c(payment_type, payment_amount))
#
#
# dffiscal <- dffiscal %>%
#   distinct(person_id, .keep_all = TRUE)
#
# save(dffiscal, file = "fiscal_clean.Rdata")

########### LOAD DATA ##########

for (i in seq(1:4)){
  load(paste0("aff",i,".Rdata"))
}
load("fiscal_clean.Rdata")
load("indiv.Rdata")

########### VECTOR OF YEARS AND MONTHS ############

years <- 2015:2022
months <- 1:12

########## AFFILIATE DATA CLEANING ################


aff1$mergin=1

aff2$mergin=1

aff3$mergin=1

aff4$mergin=1

create_cohort<- function(year){
  
start<-year*100
end<-start+12

indiv2<- indiv %>% 
  filter(birth_date %in% c(start:end))

indivmerge1<- merge(indiv2, aff1, by="person_id", all.x = T)
indivmerge2<- merge(indiv2, aff2, by="person_id", all.x = T)
indivmerge3<- merge(indiv2, aff3, by="person_id", all.x = T)
indivmerge4<- merge(indiv2, aff4, by="person_id", all.x = T)

mergeall<-full_join(indivmerge1, indivmerge2)
mergeall<-full_join(mergeall, indivmerge3)
mergeall<- full_join(mergeall, indivmerge4) %>% filter(!is.na(mergin), exit_date>20160000) %>% select(-mergin)

rm(indivmerge1, indivmerge2, indivmerge3, indivmerge4)

mergeall<-mergeall %>% 
  mutate(entry_year= as.numeric(substr(entry_date, 1,4)),
         exit_year= as.numeric(substr(exit_date, 1,4)))


# Iterate over each year
for (x in years) {
  # Generate a new column with the year
  mergeall <- mergeall %>%
    mutate(!!paste0("year_", x) := ifelse(entry_year<=x & exit_year>=x, x, NA))
}


mergeall<-mergeall %>% 
  pivot_longer(cols = c(30:37), values_to = "year") %>% 
  select(-name) %>% 
  filter(!is.na(year))



setDT(mergeall)


mergeall[, c("entry_month", "exit_month") := .(
  as.numeric(substr(entry_date, 5, 6)),
  as.numeric(substr(exit_date, 5, 6))
)]



for (x in months) {
  # Update by reference
  mergeall[, paste("month_", x, sep = "") := ifelse((year > entry_year & year < exit_year) |  
                                                      (year==entry_year & year<exit_year & x >=entry_month) |
                                                      (year>entry_year & year==exit_year & x<= exit_month) |
                                                      (year==entry_year & year==exit_year & x >= entry_month & x <=exit_month),
                                                    x, NA)]
}


mergeall<-mergeall %>% 
  pivot_longer(cols = c(33:44), values_to = "month") %>% 
  filter(!is.na(month)) %>% 
  select(-c(name, entry_month, exit_month, entry_year, exit_year))

setDT(mergeall)

mergeall$date2 <- as.numeric(sprintf("%d%02d", mergeall$year, mergeall$month))
mergeall$monthstart <- as.numeric(substr(mergeall$entry_date, 1, 6))
mergeall$monthend <- as.numeric(substr(mergeall$exit_date, 1, 6))
mergeall$daystart <- as.numeric(substr(mergeall$entry_date,7,8))
mergeall$dayend <- as.numeric(substr(mergeall$exit_date, 7,8))

mergeall[, c("msd", "med", "unemp") := list(case_when(monthstart<date2 ~ 1,
                                             monthstart==date2 ~ daystart),
                                   case_when(monthend>date2 ~ ifelse(month==2, 28,30), 
                                             monthend== date2 ~ ifelse(dayend==31, 30, dayend)),
                                   ifelse(is.na(contract_type) & job_relationship %in% c(400, 751:756, 986,999),1,0))]

for (x in 1:30) {
  # Create a new column indicating if the day falls within any contract period
  mergeall[, paste0("day_", x) := +(any(x >= msd & x <= med & unemp==0)), 
           by = .(person_id, year, month)]
}

mergeall$days_spell <- rowSums(select(mergeall, starts_with("day_")))

table(mergeall$total_days_worked)

mergeall<-mergeall %>% select(-starts_with("day_"))

mergeall<-mergeall %>% select(-c(med, msd, daystart, dayend, monthstart, monthend, date2))

setorder(mergeall, person_id, year, month, -days_spell)

mergeall[, spells_number:=.N, by=.(person_id, year, month)]
mergeall[, spells_numberseq:=1:.N, by=.(person_id, year, month)]


mergeall<-mergeall[!(contract_type %in% c(1:98,101:102,131,141,151:186,231,241,251:257,331,351:357,431,451:500,531,551,557,990))]

mergeall<- mergeall[, contr_type := case_when(contract_type %in% c(100, 109, 130, 139, 150, 189:230, 239, 250, 289) ~ "permanent",
                                                contract_type %in% c(300,309, 330, 339, 350, 389) ~ "open-ended",
                                                contract_type %in% c(403:418,430,450:500,503:518,530,552:557) ~ "other temporary", 
                                              contract_type %in% c(401,501) ~ "project-based",
                                                contract_type %in% c(402,502) ~ "production circumstances", 
                                                contract_type %in% c(420,421,520) ~ "Internship or training",
                                                contract_type %in% c(441,541) ~ "Replacement",
                                                contract_type == 540 ~ "Pre-retirement",
                                              contract_type==990 ~"other")]



mergeall[, c("permanent", "open_ended", "other_temp", "project_based", "prod_circ", "internship", "replacement", "other"):= list(
  sum(contr_type=="permanent", na.rm = T),
  sum(contr_type=="open-ended", na.rm = T),
  sum(contr_type=="other temporary", na.rm = T),
  sum(contr_type=="project-based", na.rm = T),
  sum(contr_type=="production circumstances", na.rm = T),
  sum(contr_type=="Internship or training", na.rm = T),
  sum(contr_type=="Replacement", na.rm = T),
  sum(contr_type=="other", na.rm = T)),
                    by= .(person_id, year, month)]

mergeall<- mergeall[spells_numberseq==1]

mergeall$ncontracts<- mergeall$permanent+ mergeall$open_ended+ mergeall$other_temp+ mergeall$project_based+ mergeall$prod_circ+ mergeall$internship+ mergeall$replacement+ mergeall$other

mergeall<-mergeall %>% select(-spells_numberseq)
mergeall<-left_join(mergeall, dffiscal)

load(paste0("contrcohort", year, ".Rdata"))

cohort_contr<-cohort_contr %>% select(-date)
mergeall$month<-as.numeric(mergeall$month)
cohort_contr$month <- as.numeric(cohort_contr$month)
mergeall$year <- as.numeric(mergeall$year)
cohort_contr$year <- as.numeric(cohort_contr$year)

finalcohort<-left_join(mergeall, cohort_contr, by=c("person_id","year", "month"))
save(finalcohort, file = paste0("finalcohort", year, ".Rdata"))
gc()
}


for (i in c(1957:2006)){
create_cohort(i)
}




#Reduce the size of the panels taking only observations since 2018
# 
# for (i in 1957:2006){
# load(paste0("finalcohort", i, ".Rdata"))
# finalcohort<-finalcohort[finalcohort$year>=2018]
# finalcohort$birth_date= finalcohort$birth_date.x
# finalcohort$firm_id_sec<-finalcohort$firm_id_sec.x
# finalcohort<-finalcohort %>% select(-c(firm_id_sec.y, firm_id_sec.x, birth_date.x, birth_date.y, province_resid, selection, spells_number, firm_main_prov,
#                                       contract_type))
# save(finalcohort, file=paste0("reducedcohort18", i, ".Rdata"))
# gc()
# }
# 
# 
# load("reducedcohort181957.Rdata")
# df<-finalcohort
# rm(finalcohort)
# 
# for (i in 1979:2006){
#   load(paste0("reducedcohort18", i, ".Rdata"))
#   df<-rbind(df, finalcohort)
#   gc()
# }
# 
# 
# count<-df %>%
#   group_by(person_id, year) %>%
#   summarize(cont=n())
# table(count$cont)
# 
# save(df, file = "finalpanel2018.Rdata")



#Reduce the size of the panels taking only observations since 2019

for (i in 1957:2006){
  load(paste0("finalcohort", i, ".Rdata"))
  finalcohort<-finalcohort[finalcohort$year>=2019]
  finalcohort$birth_date= finalcohort$birth_date.x
  finalcohort$firm_id_sec<-finalcohort$firm_id_sec.x
  finalcohort<-finalcohort %>% select(-c(firm_id_sec.y, firm_id_sec.x, birth_date.x, birth_date.y, province_resid, selection, spells_number, firm_main_prov,
                                         contract_type))
  save(finalcohort, file=paste0("try/reducedcohort", i, ".Rdata"))
  gc()
}


#Final panel for main analysis

load("finalcohort1957.Rdata")
finalcohort<-finalcohort[,c("person_id", "exit_date", "entry_date", "year", "month", "days_spell", "job_relationship", "regime", 
                   "contr_type", "ncontracts", "occupation", "sector", "birth_date", "sex","person_muni_latest")]

df<-finalcohort
rm(finalcohort)

for (i in 1958:2006){
  load(paste0("reducedcohort", i, ".Rdata"))
  finalcohort<-finalcohort[,c("person_id", "exit_date", "entry_date", "year", "month", "days_spell", "job_relationship", "regime", 
            "contr_type", "ncontracts", "occupation", "sector", "birth_date", "sex","person_muni_latest")]
  df<-rbind(df, finalcohort)
  gc()
}

hist(df$birth_date %/% 100)

save(df, file = "finalpanel2019c.Rdata")

#Final panel for exploration figures

df<-data.frame()

for (i in 1957:2006){
  load(paste0("reducedcohort", i, ".Rdata"))
  finalcohort<-finalcohort[, c("person_id", "entry_date", "exit_date", "firm_ett", "year", "month", "unemp", "days_spell", "contr_type", "permanent", "open_ended", "other_temp", "project_based", "prod_circ", "internship", "replacement", "other","ncontracts", "salaries", "income")]
df <-rbind(df, finalcohort)
}

save(df,file= "descriptivepanel.Rdata")

