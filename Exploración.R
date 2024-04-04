#Descriptive

library(haven)
library(data.table)
library(tidyverse)
library(lubridate) #to calculate differences between dates
library(zoo) 
library(cowplot) #To save plots

theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")
load("finalpanel2019b.Rdata")
setDT(df)


df1<-df %>% 
  group_by(year, month) %>% 
  summarize(ncontracts= sum(ncontracts),
            npermanent= sum(permanent),
            nopen_ended=sum(open_ended),
            nother_temp= sum(other_temp), 
            nprod_circ= sum(prod_circ),
            ninternship= sum(internship),
            nreplacement= sum(replacement),
            nproject=sum(project_based),
            nother=sum(other)) %>% 
  mutate(rpermanent= npermanent/ncontracts,
         ropen_ended= nopen_ended/ncontracts,
         rother_temp=nother_temp/ncontracts,
         rprod_circ= nprod_circ/ncontracts,
         rinternship= ninternship/ncontracts,
         rreplacement= nreplacement/ncontracts,
         rproject=nproject/ncontracts,
         rother=nother/ncontracts,
         total= rpermanent+ropen_ended+rother_temp+rprod_circ+rinternship+rreplacement+rproject+rother)

df2 <-df1 %>% 
  pivot_longer(cols = c(12:18), names_to = "type", values_to = "value")

#ggncontracts<-
df2 %>% 
  mutate(transp= as.factor(ifelse(type %in% c("rpermanent", "ropen_ended", "rproject"),0,1))) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-")) ) %>%  
  ggplot(aes(x=date, y=value, group=type, color=type, alpha=transp))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  scale_alpha_manual(values = c(1,0.2))+
  guides(alpha="none")

ggsave2(ggncontracts, file="../../../../../../Plots/descriptive_mcvl/ratio_of_total_contracts.jpeg", width = 5)

#Did people go to self-employed?

dfautonomo<- df[regime<800, self_employed := case_when(regime%in% c(0:180,611:650) ~"employee",
                                                           regime %in% c(500:540, 721:740) ~"self-employed")]

dfautonomo1<-dfautonomo %>% 
  group_by(year, month) %>% 
  summarize(self_emp= sum(self_employed== "self-employed", na.rm = T),
            employee= sum(self_employed=="employee", na.rm = T)) %>% 
  ungroup()
  
dfautonomo1<-dfautonomo1 %>% 
  mutate(date= as.numeric(paste0(year, sprintf("%02d", month))),
         self_empp= self_emp/first(self_emp)*100,
         employeep= employee/first(employee)*100)

dfautonomo1<-dfautonomo1 %>% select(-c(3,4))

dfautonomo1<-dfautonomo1 %>% pivot_longer(cols = c(4,5), names_to = "type", values_to = "value")



#ggself_emp<-
dfautonomo1 %>%
  mutate(date=as.Date(paste(year, month, "01", sep="-"))) %>% 
  ggplot(aes(x=date, y=value, group=type, color=type))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))

ggsave2(ggself_emp, file="../../../../../../Plots/descriptive_mcvl/self_emp.jpeg", width = 5)

##Number of contracts

dfncontracts<- df %>% 
  group_by(year, month, contr_type) %>% 
  summarize(ncontracts=mean(ncontracts, na.rm = T)) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-")))
  
dfncontracts %>% 
  mutate(transp= as.factor(ifelse(contr_type %in% c("permanent", "open-ended", "project-based"),0,1))) %>% 
  filter(!is.na(contr_type), contr_type!="Pre-retirement") %>% 
  ggplot(aes(x=date, y=ncontracts, group=contr_type, color=contr_type, alpha=transp))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  scale_alpha_manual(values = c(1,0.2))+
  guides(alpha="none")

##days worked

dfdaysworked<-df %>% 
  group_by(year, month, contr_type) %>% 
  summarise(daysworked=mean(days_spell)) %>% 
  mutate(daysworked= ifelse(month==2, daysworked*30/28 , daysworked ),
         date=as.Date(paste(year, month, "01", sep="-")))


#ggdaysworked<-
dfdaysworked %>% 
  mutate(transp= as.factor(ifelse(contr_type %in% c("permanent", "open-ended", "project-based"),0,1))) %>% 
  filter(!is.na(contr_type)) %>% 
  ggplot(aes(x=date, y=daysworked, group=contr_type, color=contr_type, alpha=transp))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_alpha_manual(values = c(1,0.2))+
  guides(alpha="none")

ggsave2(ggdaysworked, file="../../../../../../Plots/descriptive_mcvl/daysworked.jpeg", width = 6)


## permanent jobs weaker? (probably better with the other df)


dfperm<-df %>% 
  select(person_id, year, month, contr_type, entry_date, exit_date)

dfperm<-dfperm %>% distinct(person_id, entry_date, exit_date, .keep_all=T) #Important otherwise each contract is repeated every month it exists


dfperm<-dfperm %>% 
    mutate(bin_end=ifelse(exit_date>20221200,1,0),
         start=lubridate::ymd(entry_date),
         end=lubridate::ymd(exit_date),
         duration=as.numeric(end-start))

dfperm1<-dfperm %>% 
  group_by(year, month, contr_type) %>% 
  summarize(meaan=mean(duration),
            ending=mean(bin_end))

dfperm1<-dfperm1 %>% 
  mutate(dates=as.Date(paste(year, month, "01", sep="-")))
  
  
dfperm1 %>% 
  mutate(transp= as.factor(ifelse(contr_type %in% c("permanent", "open-ended", "project-based"),0,1))) %>% 
  filter(!is.na(contr_type)) %>% 
  ggplot(aes(x=dates, y=meaan, group=contr_type, color=contr_type, alpha=transp)) +
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  scale_alpha_manual(values=c(1,.2))+
  guides(alpha="none")

dfperm1 %>% 
  filter(!is.na(contr_type)) %>% 
  ggplot(aes(x=dates, y=ending, group=contr_type, color=contr_type)) +
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))


##Permanent jobs less income?

df1<-df %>% 
  group_by(contr_type, year, month) %>% 
  summarise(income=mean(income, na.rm = T)) %>% 
  mutate(date= as.Date(paste(year, month, "01", sep = "-")))

ggincome_contracts<-df1 %>% 
  filter(!is.na(contr_type)) %>% 
  mutate(transp= as.factor(ifelse(contr_type %in% c("permanent", "open-ended", "project-based"),0,1))) %>% 
  ggplot(aes(x=date, y=income, color=contr_type, alpha=as.factor(transp))) +
  geom_line()+
  scale_alpha_manual(values = c(.2,1))+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  guides(alpha="none")+
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave2(ggincome_contracts,file="../../../../../../Plots/descriptive_mcvl/income_bycontract.jpeg", width = 6)

#### People with temporary jobs, what happened to them ##



df1<-df %>% 
  mutate(dummy_project_based= ifelse(project_based>0,1,0
         )) %>% 
  group_by(person_id, year) %>% 
  mutate(sumpb=sum(dummy_project_based)) %>% 
  ungroup() %>% 
  group_by(person_id) %>% 
  mutate(selected=ifelse(any(year==2021 & sumpb>10),1,0)) %>% 
  filter(selected==1) %>% 
  ungroup()

#Look at contracts

df2<-df1 %>% 
  group_by(year, month) %>% 
  summarize(permanent= sum(permanent)/sum(ncontracts),
            open_ended= sum(open_ended)/sum(ncontracts),
            other_temp=sum(other_temp)/sum(ncontracts),
            prod_circ= sum(prod_circ)/sum(ncontracts),
            internship= sum(internship)/sum(ncontracts),
            replacement= sum(replacement)/sum(ncontracts),
            project=sum(project_based)/sum(ncontracts),
            other=sum(other)/sum(ncontracts),
            total= permanent+open_ended+other_temp+prod_circ+internship+replacement+project+other)


df2 <-df2 %>% 
  pivot_longer(cols = c(3:9), names_to = "type", values_to = "value")


ggcontractstreatment<- df2 %>% 
  mutate(transp= as.factor(ifelse(type %in% c("permanent", "open_ended", "project"),0,1))) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-")) ) %>%  
  ggplot(aes(x=date, y=value, group=type, color=type, alpha=transp))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  scale_alpha_manual(values = c(1,0.2))+
  guides(alpha="none")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave2(ggcontractstreatment, file="../../../../../../Plots/descriptive_mcvl/ggcontractstreatment.jpeg", width = 6)


## Days worked ##

dfdaysworked<-df1 %>% 
  group_by(year, month, contr_type) %>% 
  summarise(daysworked=mean(days_spell)) %>% 
  mutate(daysworked= ifelse(month==2, daysworked*30/28 , daysworked ),
         date=as.Date(paste(year, month, "01", sep="-")))


#ggdaysworked<-
dfdaysworked %>% 
  mutate(transp= as.factor(ifelse(contr_type %in% c("permanent", "open-ended", "project-based"),0,1))) %>% 
  filter(!is.na(contr_type)) %>% 
  ggplot(aes(x=date, y=daysworked, group=contr_type, color=contr_type, alpha=transp))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_alpha_manual(values = c(1,0.2))+
  guides(alpha="none")

## Look at self-employed



setDT(df1)

dfautonomo<- df1[regime<800, self_employed := case_when(regime%in% c(0:180,611:650) ~"employee",
                                                       regime %in% c(500:540, 721:740) ~"self-employed")]

dfautonomo1<-dfautonomo %>% 
  group_by(year, month) %>% 
  summarize(self_emp= sum(self_employed== "self-employed", na.rm = T),
            employee= sum(self_employed=="employee", na.rm = T)) %>% 
  ungroup()

dfautonomo1<-dfautonomo1 %>% 
  mutate(date= as.numeric(paste0(year, sprintf("%02d", month))),
         self_empp= self_emp/first(self_emp)*100,
         employeep= employee/first(employee)*100)

dfautonomo2<-dfautonomo1 %>% select(-c(6,7))
dfautonomo1<-dfautonomo1 %>% select(-c(3,4))

dfautonomo1<-dfautonomo1 %>% pivot_longer(cols = c(4,5), names_to = "type", values_to = "value") %>% mutate(graph= "base_100")
dfautonomo2<-dfautonomo2 %>% pivot_longer(cols = c(3,4), names_to = "type", values_to = "value") %>% mutate(graph= "total")

dfaut<-rbind(dfautonomo1, dfautonomo2) %>% mutate(type= case_when(type=="employeep"~ "employee",
                                                                 type=="self_empp" ~ "self_emp",
                                                                 TRUE ~type
                                                                  ))

ggauttreatment<-dfaut %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-"))) %>% 
  ggplot(aes(x=date, y=value, group=type, color=type))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  facet_wrap(~graph, scales = "free", nrow = 2)

ggsave(ggauttreatment, file="../../../../../../Plots/descriptive_mcvl/ggselfemployed_treatment.jpeg", width = 6)

##income

df2<-df1 %>% 
  group_by(year, month) %>% 
  summarise(income=mean(income, na.rm = T)) %>% 
  mutate(date= as.Date(paste(year, month, "01", sep = "-")))

df2 %>% 
  ggplot(aes(x=date, y=income)) +
  geom_line()



#----------------------------------------------------------------------


#### Looking at duration of contracts ####


load("dfcontracts.Rdata")

dfcontracts1<-dfcontracts %>% 
  filter(entry_date>20190000)

dfcontracts1<-dfcontracts1 %>% 
  mutate(start=ymd(entry_date),
       end=ymd(exit_date),
       duration=as.numeric(end-start))

dfcontracts1<-dfcontracts1 %>% 
  mutate(dates=as.Date(as.character(entry_date), format="%Y%m%d")
         # week=format(dates, "%W"),
         # year=format(dates, "%Y"),
         #yearweek= format(dates, "%Y%W")
         )

dfcontracts1<-dfcontracts1%>%
  group_by(contract_type, dates) %>% 
  summarize(meaan=mean(duration)) %>% 
  ungroup() %>% 
  arrange(contract_type, dates) %>%
  group_by(contract_type) %>%
  mutate(duration_avg = rollmean(meaan, k = 30, align = "right", fill = NA)) %>% 
  ungroup()


ggdurationcontracts<-
  dfcontracts1 %>% 
  mutate(transp= as.factor(ifelse(contract_type %in% c("permanent", "open-ended", "project-based"),0,1))) %>% 
  filter(!is.na(contract_type), contract_type != "Replacement", contract_type!="Pre-retirement") %>% 
  ggplot(aes(x=dates, y=duration_avg, group=contract_type, color=contract_type, alpha=transp)) +
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_alpha_manual(values = c(1,.2))+
  guides(alpha="none")

ggsave2(ggdurationcontracts, file="../../../../../../Plots/descriptive_mcvl/ggdurationcontracts.jpeg", width = 6)

##Number of contracts destroyed by day


dfcontracts2<-dfcontracts %>% 
  filter(entry_date>20190000, exit_date<20221231)  

dfcontracts6<-dfcontracts2 %>% 
  group_by(contract_type, exit_date) %>% 
  summarise(number=n()) %>% 
  ungroup() %>% 
  arrange(contract_type, exit_date) %>%
  group_by(contract_type) %>%
  mutate(exits = rollmean(number, k = 30, align = "right", fill = NA),
         dates=as.Date(as.character(exit_date), format="%Y%m%d"))%>% 
  select(-c(exit_date, number))

dfcontracts7<-dfcontracts2 %>% 
  group_by(contract_type, entry_date) %>% 
  summarise(number=n()) %>% 
  ungroup() %>% 
  arrange(contract_type, entry_date) %>%
  group_by(contract_type) %>%
  mutate(entries = rollmean(number, k = 30, align = "right", fill = NA),
         dates=as.Date(as.character(entry_date), format="%Y%m%d"))%>% 
  select(-c(entry_date, number))

dfcontractsplot1<- left_join(dfcontracts6, dfcontracts7, by=c("dates","contract_type")) %>% mutate(diff=exits-entries)

dfcontractsplot1<- dfcontractsplot1 %>% pivot_longer(cols = c(2,4,5), names_to = "movement", values_to = "value")

dfcontractsplot1<-dfcontractsplot1 %>% mutate(graph=ifelse(movement=="diff", "net_exits", "exits and entries"))

ggentries<-dfcontractsplot1 %>% 
  filter(!is.na(contract_type), contract_type %in% c("inactive-open-ended", "permanent", "open-ended", "project-based")) %>% 
  ggplot(aes(x=dates, y=value, linetype=movement, color=contract_type))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  facet_wrap(~graph, scales = "free", nrow = 2)+
  scale_linetype_manual(values = c("solid", "solid", "dashed"))+
  theme(legend.title = element_blank(), 
        legend.position = "bottom")

ggsave2(ggentries, file="../../../../../../Plots/descriptive_mcvl/ggexits.jpeg", width = 7, height = 6)

##Same as before but differentiating those who go to inactivity

dfcontracts3<-dfcontracts %>% 
  filter(entry_date>20190000, exit_date<20221231)  

dfcontracts3<-dfcontracts3 %>% 
  mutate(type= ifelse(contract_type=="open-ended" & reason_dismissal==94, "inactive-open-ended", contract_type ))
 
dfcontracts4<- dfcontracts3 %>% 
  group_by(type, entry_date) %>% 
  summarise(exitn=n()) %>% 
  ungroup() %>% 
  arrange(type, entry_date) %>%
  group_by(type) %>%
  mutate(exits = rollmean(exitn, k = 30, align = "right", fill = NA),
         dates=as.Date(as.character(entry_date), format="%Y%m%d"))%>% 
  select(-c(entry_date, exitn))


dfcontractsplot<- dfcontracts4 %>% 
  filter(type %in% c("inactive-open-ended", "open-ended")) %>% 
  group_by(dates) %>% 
  mutate(total=sum(exits),
         exitsrel= exits/total) %>% 
  ungroup()

dfcontractsplot<- dfcontractsplot %>%
  pivot_longer(cols = c(2,5), names_to = "graph", values_to = "value") 

dfcontractsplot %>% 
  ggplot(aes(x=dates, y=value, color=type))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  theme(legend.title = element_blank())+
  facet_wrap(~graph, scales = "free", nrow = 2)


##NUMBER OF CONTRACTS ENDING BEFORE 3 MONTHS


dfcontracts8<-dfcontracts %>% 
  filter(entry_date>20190000)

dfcontracts8<-dfcontracts8 %>% 
  mutate(start=ymd(entry_date),
         end=ymd(exit_date),
         duration=as.numeric(end-start))

dfcontracts8<-dfcontracts8 %>% 
  mutate(dates=as.Date(as.character(entry_date), format="%Y%m%d")
  )

dfcontracts8<- dfcontracts8 %>% 
  mutate(less_than_7=ifelse(duration<7, 1,0),
         less_than_15=ifelse(duration<15, 1,0),
         less_than_30=ifelse(duration<30, 1, 0),
         less_than_90=ifelse(duration<90, 1, 0))



dfcontracts9<-dfcontracts8 %>% 
  group_by(contract_type, dates) %>% 
  summarize(below7=mean(less_than_7, na.rm = T),
            below15=mean(less_than_15, na.rm=T),
            below30=mean(less_than_30, na.rm=T),
            below90=mean(less_than_90, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(contract_type, dates) %>%
  group_by(contract_type) %>% 
  mutate(below7= rollmean(below7, k=30, align = "right", fill=NA),
         below15= rollmean(below15, k=30, align = "right", fill=NA),
         below30= rollmean(below30, k=30, align = "right", fill=NA),
         below90=rollmean(below90, k=30, align = "right", fill=NA))

# dfcontracts9<-dfcontracts8 %>% 
#   group_by(dates) %>% 
#   summarize(below7=mean(less_than_7, na.rm = T),
#             below15=mean(less_than_15, na.rm=T),
#             below30=mean(less_than_30, na.rm=T),
#             below90=mean(less_than_90, na.rm = T)) %>% 
#   ungroup() 


dfcontracts9<-dfcontracts9 %>% pivot_longer(cols = c(3:6), names_to = "duration", values_to = "value")
# dfcontracts9<-dfcontracts9 %>% pivot_longer(cols = c(2:5), names_to = "duration", values_to = "value")


dfcontracts9$duration <- factor(dfcontracts9$duration, levels = c("below7", "below15", "below30", "below90"))

#ggduration<-
  dfcontracts9 %>% 
  arrange(duration) %>% 
  filter(!is.na(contract_type), contract_type %in% c("open-ended", "permanent", "project-based")) %>% 
  ggplot(aes(x=dates, y=value, color= contract_type))+
  geom_line()+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  facet_wrap(~duration)+
  theme(legend.position = "bottom",
        legend.title = element_blank())
  
# dfcontracts9 %>% 
#     arrange(duration) %>% 
#    # filter(!is.na(contract_type), contract_type %in% c("open-ended", "permanent", "project-based")) %>% 
#     ggplot(aes(x=dates, y=value))+
#     geom_line()+
#     geom_vline(xintercept = as.Date("2022-01-01"))+
#   scale_x_date(limits = as.Date(c("2021-11-01", "2022-02-01")))+
#     facet_wrap(~duration)+
#     theme(legend.position = "bottom",
#           legend.title = element_blank()) 

ggsave2(ggduration, file="../../../../../../Plots/descriptive_mcvl/ggduration.jpeg", width = 7, height = 6)


###What happened to ETT
df1<-df %>% 
  filter(firm_ett==5081, !is.na(contr_type)) %>% 
  group_by(year, month, contr_type) %>% 
  summarise(count=as.numeric(n())) %>% 
  ungroup() %>% 
  group_by(month, year) %>% 
  mutate(total=as.numeric(sum(count, na.rm = T))) %>% 
  ungroup() %>% 
  mutate(ratio=as.numeric(count/total))

df1<-pivot_longer(df1,cols = c(4,6))

df1 <-df1 %>% mutate(date= as.Date(paste(year, month, "01", sep="-")))

ggett<-df1 %>% 
  ggplot(aes(x=date, y=value, color=contr_type)) +
  geom_line()+
  facet_wrap(~name, scales = "free", nrow=2)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave2(ggett, file="../../../../../../Plots/descriptive_mcvl/ett.jpeg", width = 7, height = 6)
 
#####Number of people per sector


##Follow days worked of these people --> Not a good idea cause it changes a lot depending on how the treatment has been chosen
##Follow income of treated people ---> same
##ETT
#Number of people per sector, occupation, provincia, town

df1<-df %>% select(year, month, person_muni_latest, sector, occupation, regime)

dfcount<-df1 %>% 
  group_by(year, month, person_muni_latest) %>% 
  summarize(count=n()) #From 200 to 32000


df1 <- df1 %>%
  filter(person_muni_latest!=0) %>% 
  mutate(prov = person_muni_latest %/% 1000)


dfcount<-df1 %>% 
  group_by(year, month, prov) %>% 
  summarize(count=n()) ##400 to 63000

dfcount<-df1 %>% 
  group_by(year, month, sector) %>% 
  summarize(count=n())

sectors_without_zero <- c(
  11, 111, 1110, 112, 1121, 1122, 113, 1131, 1132, 1133, 1134,
  12, 121, 1210, 122, 1221, 1222, 123, 1231, 1232, 124, 1240, 125,
  1250, 13, 130, 1300, 14, 141, 1410, 142, 1420, 15, 150, 1501,
  1502, 1503, 2, 20, 201, 2011, 2012, 202, 2020, 5, 50,
  501, 5010, 502, 5021, 5022
)


df1$sector<- as.integer(df1$sector)

df1$sector <- ifelse(as.numeric(df1$sector) %in% sectors_without_zero, paste0("0", df1$sector), df1$sector)

df1$sector1<- substr(df1$sector, 1, 2)

dfcount<-df1 %>% 
  group_by(year, month, sector1) %>% 
  summarize(count=n())



############# TRANSITIONS ##############

df<-df %>% select(person_id, exit_date, contr_type, year, month, unemp)


df<-df %>% 
  mutate(situation= ifelse(!is.na(contr_type), contr_type, ifelse(unemp==1, "unemployment", NA)
                           )
         ) %>% 
  filter(!is.na(situation))
  


complete_data <- expand.grid(person_id = unique(df$person_id),
                             year = unique(df$year),
                             month = 1:12)

# Merge with the original data to retain existing observations
complete_data <- merge(complete_data, df, by = c("person_id", "year", "month"), all.x = TRUE)


# Since we already eliminated all NAs from situation, the new ones must come from the creation of new observations
complete_data$new_observation <- ifelse(is.na(complete_data$situation), 1, 0)

# Fill missing 'situation' with "unemployment"
complete_data$situation <- ifelse(is.na(complete_data$situation), "unemployment", complete_data$situation)


# Reorder columns
complete_data <- complete_data[, c("person_id", "exit_date", "year", "month", "situation", "new_observation")]

complete_data<-complete_data %>% 
  mutate(exit_month= exit_date %/% 100,
         yearmonth= as.numeric(paste0(year, sprintf("%02d", month))))

complete_data<-complete_data %>% 
  arrange(person_id, -yearmonth) %>% 
  group_by(person_id) %>% 
  mutate(next_situation= lag(situation),
         next2_situation= lag(situation,2),
         next3_situation=lag(situation,3)) %>% 
  filter(yearmonth==exit_month) %>% 
  ungroup()

df3<-complete_data[1:10000,]

complete_data<-complete_data[complete_data$situation %in% c("permanent", "project-based", "production circumstances"),]



  
dff<-complete_data %>% 
  group_by(situation, year, month, next_situation) %>% 
  summarize(transition=n()) %>% 
  group_by(situation, year, month) %>% 
  mutate(total=sum(transition),
         ratio=transition/total,
         date= as.Date(paste(year, month, "01", sep="-"))) %>% 
  arrange(year, month) %>% 
  group_by(next_situation, situation) %>% 
  mutate(base100=ratio*100/first(ratio))


dff2<-complete_data %>% 
  group_by(situation, year, month, next2_situation) %>% 
  summarize(transition=n()) %>% 
  group_by(situation, year, month) %>% 
  mutate(total=sum(transition),
         ratio=transition/total,
         date= as.Date(paste(year, month, "01", sep="-"))) %>% 
  arrange(year, month) %>% 
  group_by(next2_situation, situation) %>% 
  mutate(base100=ratio*100/first(ratio))

dff3<-complete_data %>% 
  group_by(situation, year, month, next3_situation) %>% 
  summarize(transition=n()) %>% 
  group_by(situation, year, month) %>% 
  mutate(total=sum(transition),
         ratio=transition/total,
         date= as.Date(paste(year, month, "01", sep="-"))) %>% 
  arrange(year, month) %>% 
  group_by(next3_situation, situation) %>% 
  mutate(base100=ratio*100/first(ratio))

dff3<-dff3 %>% 
  filter(situation %in% c("project-based", "production circumstances"), next3_situation=="permanent")

dff3<-dff3 %>% 
  mutate(treatment=ifelse(situation=="project-based", 1, 0)) %>% 
  arrange(treatment, year, month) %>% 
  mutate(time=seq(1:45))

mod_1 <- feols(ratio ~ i(time, treatment, ref = +37),
               data = dff3)
ggiplot(mod_1)

dff %>%
    filter(situation %in% c("project-based", "permanent")) %>%
    ggplot(aes(x = date, y = base100, color = situation)) +
    geom_line() +
   facet_wrap(~ next_situation, scales = "free") 

dff2 %>%
  filter(situation %in% c("project-based", "permanent")) %>%
  ggplot(aes(x = date, y = base100, color = situation)) +
  geom_line() +
  facet_wrap(~ next2_situation, scales = "free") 

dff3 %>%
  filter(situation %in% c("project-based", "permanent")) %>%
  ggplot(aes(x = date, y = base100, color = situation)) +
  geom_line() +
  facet_wrap(~ next3_situation, scales = "free") 




# dff %>%
#   filter(situation %in% c("project-based", "Internship or training")) %>%
#   ggplot(aes(x = date, y = base100, color = situation)) +
#   geom_line() +
#   facet_wrap(~ next_situation, scales = "free") 

# dff %>%
#   filter(situation %in% c("project-based", "other temporary")) %>%
#   ggplot(aes(x = date, y = base100, color = situation)) +
#   geom_line() +
#   facet_wrap(~ next_situation, scales = "free") 


#gg1month<-
  dff %>%
  filter(situation %in% c("project-based", "production circumstances"), next_situation != "Pre-retirement", !is.na(next_situation)) %>%
  ggplot(aes(x = date, y = base100, color = situation)) +
  geom_line() +
  facet_wrap(~ next_situation, scales = "free") +
  theme(legend.position = "bottom",
        legend.title=element_blank())+
  ggtitle("N laid-off workers from pc and pb contracts to each situation after 1 month. 01/2019=100%")

ggsave2(gg1month, file="../../../../../../Plots/descriptive_mcvl/1month.jpeg", width = 8, height = 6)

gg2month<-
  dff2 %>%
  filter(situation %in% c("project-based", "production circumstances"), next2_situation != "Pre-retirement", !is.na(next2_situation)) %>%
  ggplot(aes(x = date, y = base100, color = situation)) +
  geom_line() +
  facet_wrap(~ next2_situation, scales = "free") +
  theme(legend.position = "bottom")+
  ggtitle("N laid-off workers from pc and pb contracts to each situation after 2 months. 01/2019=100%")

ggsave2(gg2month, file="../../../../../../Plots/descriptive_mcvl/2months.jpeg", width = 8, height = 6)



gg3month<-
dff3 %>%
  filter(situation %in% c("project-based", "production circumstances"), next3_situation != "Pre-retirement", !is.na(next3_situation)) %>%
  ggplot(aes(x = date, y = base100, color = situation)) +
  geom_line() +
  facet_wrap(~ next3_situation, scales = "free") +
  theme(legend.position = "bottom")+
  ggtitle("N laid-off workers from pc and pb contracts to each situation after 3 months. 01/2019=100%")

ggsave2(gg3month, file="../../../../../../Plots/descriptive_mcvl/3months.jpeg", width = 8, height = 6)


### take the people who lost their job in 2021 2022 and see how that goes


# rm(df)
# newdf <- expand.grid(person_id = unique(complete_data$person_id),
#                              year = unique(complete_data$year),
#                              month = 1:12)
# load("finalpanel2019b.Rdata")
# setDT(df)
# 
# df<-df %>% select(person_id, sex, person_muni_latest, edu_code, exit_date, year, month, unemp, days_spell, contr_type, ncontracts, marital_status, children)
# 
# newdf <- merge(newdf, df, by = c("person_id", "year", "month"), all.x = TRUE)
# 
# rm(df)
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
# contr1 <- contr1 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr2 <- contr2 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr3 <- contr3 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr4 <- contr4 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr5 <- contr5 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr6 <- contr6 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr7 <- contr7 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr8 <- contr8 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr9 <- contr9 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr10 <- contr10 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr11 <- contr11 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# contr12 <- contr12 %>%
#   filter(person_id %in% newdf$person_id, year %in% newdf$year)
# 
# fullcontr<-rbind(contr1, contr2, contr3, contr4, contr5, contr6, contr7, contr8, contr9, contr10, contr11, contr12)
# 
# newdf <- merge(newdf, fullcontr, by = c("person_id", "year", "month"), all.x = TRUE)
# 
# newdf1<-newdf[1:10000,]
# 
# setDT(newdf)
# 
# newdf[, situation:= ifelse(!is.na(contr_type), contr_type, ifelse(unemp==1, "unemployment", NA))]
# 
# min_year <- min(newdf1$year)
# max_year <- max(newdf1$year)
# min_month <- min(newdf1$month)
# max_month <- max(newdf1$month)
# 
# 
# newdf[, time:= (year - min_year) * 12 + (month - min_month + 1)]
# 
# 
# newdf <- newdf %>%
#   group_by(person_id) %>%
#   mutate(group = ifelse(any(situation=="project-based"), max(time[situation == "project-based"], na.rm = T),NA)) %>%
#   ungroup()
# 
# save(newdf, file = "newdf.Rdata")
# 

################# Employment, unemployment, permanent contracts, people going to unemployment... ########



