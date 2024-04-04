library(haven)
library(data.table)
library(tidyverse)
library(lubridate)

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")

for (i in seq(1:4)){
  load(paste0("aff",i,".Rdata"))
}

load("aff1.Rdata")
aff1<-aff1 %>% 
  select(person_id, contract_type, entry_date, exit_date, reason_dismissal) %>% 
  filter(exit_date>20190000)
aff2<-aff2 %>% 
  select(person_id, contract_type, entry_date, exit_date, reason_dismissal)%>% 
  filter(exit_date>20190000)
aff3<-aff3 %>% 
  select(person_id, contract_type, entry_date, exit_date, reason_dismissal)%>% 
  filter(exit_date>20190000)
aff4<-aff4 %>% 
  select(person_id, contract_type, entry_date, exit_date, reason_dismissal)%>% 
  filter(exit_date>20190000)

dfcontracts<- rbind(aff1, aff2, aff3, aff4)

rm(aff1, aff2, aff3, aff4)

## What happened to duration of contracts

setDT(dfcontracts)
dfcontracts[, contract_type := case_when(contract_type %in% c(100, 109, 130, 139, 150, 189:230, 239, 250, 289) ~ "permanent",
                                      contract_type %in% c(300,309, 330, 339, 350, 389) ~ "open-ended",
                                      contract_type %in% c(403:418,430,450:500,503:518,530,552:557) ~ "other temporary", 
                                      contract_type %in% c(401,501) ~ "project-based",
                                      contract_type %in% c(402,502) ~ "production circumstances", 
                                      contract_type %in% c(420,421,520) ~ "Internship or training",
                                      contract_type %in% c(441,541) ~ "Replacement",
                                      contract_type == 540 ~ "Pre-retirement",
                                      contract_type==990 ~"other")]

save(dfcontracts, file = "dfcontracts.Rdata")
