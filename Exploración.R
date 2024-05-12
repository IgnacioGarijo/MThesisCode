#Descriptive

library(haven)
library(data.table)
library(tidyverse)
library(lubridate) #to calculate differences between dates
library(zoo) 
library(cowplot) #To save plots
library(broom)   #Para los mapas
library(rgdal) #Mapas

theme_set(theme_bw())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")
load("descriptivepanel.Rdata")
setDT(df)

##Number of contracts

df$ncontracts[df$ncontracts==0]<-NA
df<-df[!is.na(df$ncontracts),]

dfncontracts<- df %>% 
  group_by(year, month, contr_type) %>% 
  summarize(ncontracts=mean(ncontracts, na.rm = T)) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-")))

dfncontracts1<-df[df$contr_type=="permanent"]
rm(df)

dfncontracts1$contract_date <- as.Date(as.character(dfncontracts1$entry_date), format = "%Y%m%d")

dfncontracts1$current_date <- as.Date(paste(dfncontracts1$year, dfncontracts1$month, "01", sep = "-"))

dfncontracts1$years_difference <- as.numeric(difftime(dfncontracts1$current_date, dfncontracts1$contract_date, units = "days") / 365)

dfncontracts1 <- dfncontracts1[dfncontracts1$years_difference >= 1, ]


dfncontracts1<-dfncontracts1 %>% 
  group_by(year, month) %>%
  summarize(ncontracts=mean(ncontracts, na.rm = T)) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-")),
         contr_type="old-permanent")
  
dfncontracts<-rbind(dfncontracts, dfncontracts1)

gg<-
  dfncontracts %>% 
  filter(contr_type %in% c("permanent", "old-permanent","open-ended", "project-based")) %>% 
  filter(!is.na(contr_type), contr_type!="Pre-retirement") %>% 
  ggplot(aes(x=date, y=ncontracts, group=contr_type, color=contr_type, alpha=contr_type))+
  geom_line()+
  geom_point(size=1)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed", alpha=.4)+
  geom_vline(xintercept = as.Date("2020-01-01"), linetype="dashed", alpha=.4)+
  geom_vline(xintercept = as.Date("2019-01-01"), linetype="dashed", alpha=.4)+
  geom_hline(yintercept = 1, alpha=.3)+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.x = element_blank())+
  ylab("Average number of contracts")+
  scale_color_manual(name="h",values = c("permanent"="#065465","old-permanent"="#065465", "open-ended"= "#008080","project-based"= "#b67182"))+
  scale_alpha_manual(values = c("permanent"=.7, "open-ended"=.7, "old-permanent"=.4, "project-based"=.7), name="h")
  

ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/YESnofcontracts.jpeg", width = 5, height = 3)


dfncontracts1<-dfncontracts %>% 
  filter(contr_type %in% c("permanent", "old-permanent")) %>% 
  pivot_wider(names_from = contr_type, values_from = ncontracts) %>% 
  mutate(ratio= (permanent - `old-permanent`)/`old-permanent`,
         var="ncontracts") 

gg<-dfncontracts1 %>% 
    ggplot(aes(x=date, y=dif))+
  geom_line( color="#065465", alpha=.7)+
  geom_point(color="#065465", alpha=.8)+
  scale_y_continuous(labels = scales::percent)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  geom_vline(xintercept = c(as.Date("2021-01-01"), as.Date("2020-01-01"), as.Date("2019-01-01")), linetype="dashed")+
  theme(axis.title = element_blank())

ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/difnofcontracts.jpeg", width = 5, height = 3)



##days worked

load("descriptivepanel.Rdata")

dfdaysworked<-df %>% 
  group_by(year, month, contr_type) %>% 
  summarise(daysworked=mean(days_spell)) %>% 
  mutate(daysworked= ifelse(month==2, daysworked*30/28 , daysworked ),
         date=as.Date(paste(year, month, "01", sep="-")))

df1<-df[df$contr_type=="permanent"]
rm(df)

df1$contract_date <- as.Date(as.character(df1$entry_date), format = "%Y%m%d")

df1$current_date <- as.Date(paste(df1$year, df1$month, "01", sep = "-"))

df1$years_difference <- as.numeric(difftime(df1$current_date, df1$contract_date, units = "days") / 365)

df1 <- df1[df1$years_difference >= 1, ]

dfdaysworked2<-df1 %>% 
  group_by(year, month) %>% 
  summarise(daysworked=mean(days_spell)) %>% 
  mutate(daysworked= ifelse(month==2, daysworked*30/28 , daysworked ),
         date=as.Date(paste(year, month, "01", sep="-")),
         contr_type="old-permanent")

dfdaysworked<-rbind(dfdaysworked, dfdaysworked2)

ggdaysworked<-
dfdaysworked %>% 
  filter(contr_type %in% c("permanent", "old-permanent", "open-ended", "project-based")) %>% 
  ggplot(aes(x=date, y=daysworked, group=contr_type, color=contr_type, alpha=contr_type))+
  geom_line()+
  geom_point( size=1)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed", alpha=.4)+
  geom_vline(xintercept = as.Date("2020-01-01"), linetype="dashed", alpha=.4)+
  geom_vline(xintercept = as.Date("2019-01-01"), linetype="dashed", alpha=.4)+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.x = element_blank())+
  ylab("Average days under spell")+
  scale_alpha_manual(values = c("permanent"=.7, "open-ended"=.7, "old-permanent"=.4, "project-based"=.7), name="h")+
  scale_color_manual(values = c("permanent"="#065465", "old-permanent"="#065465", "open-ended"= "#008080","project-based"= "#b67182"),
                     name="h")

ggsave2(ggdaysworked, file="../../../../../../Plots/descriptive_mcvl/YESdaysworked.jpeg", width = 5, height = 3)



dfdif<-
  dfdaysworked %>% 
  filter(contr_type %in% c("permanent", "old-permanent")) %>% 
  pivot_wider(names_from = contr_type, values_from = daysworked) %>% 
  mutate(ratio= (permanent - `old-permanent`)/`old-permanent`,
         var= "days_worked") 

gg<-
dfdif %>% 
  ggplot(aes(x=date, y=dif))+
  geom_line( color="#065465", alpha=.7)+
  geom_point(color="#065465", alpha=.8)+
  scale_y_continuous(labels = scales::percent)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  geom_vline(xintercept = c(as.Date("2021-01-01"), as.Date("2020-01-01"), as.Date("2019-01-01")), linetype="dashed")+
  theme(axis.title = element_blank())

ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/difdaysworked.jpeg", width = 5, height = 3)


##Less income?


dfincome<-df %>% 
  group_by(year, month, contr_type) %>% 
  summarise(income=mean(income, na.rm=T)) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-")))

dfincome2<-df1 %>% 
  group_by(year, month) %>% 
  summarise(income=mean(income, na.rm = T)) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep="-")),
         contr_type="old-permanent")

dfincome<-rbind(dfincome, dfincome2)

ggincome<-
  dfincome %>% 
  filter(contr_type %in% c("permanent", "old-permanent", "open-ended", "project-based")) %>% 
  ggplot(aes(x=date, y=income/100, group=contr_type, color=contr_type, alpha=contr_type))+
  geom_line()+
  geom_point( size=1)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed", alpha=.4)+
  geom_vline(xintercept = as.Date("2020-01-01"), linetype="dashed", alpha=.4)+
  geom_vline(xintercept = as.Date("2019-01-01"), linetype="dashed", alpha=.4)+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.x = element_blank())+
  ylab("Average monthly income")+
  scale_alpha_manual(values = c("permanent"=.7, "open-ended"=.7, "old-permanent"=.4, "project-based"=.7), name="h")+
  scale_color_manual(values = c("permanent"="#065465", "old-permanent"="#065465", "open-ended"= "#008080","project-based"= "#b67182"),
                     name="h")

ggsave2(ggincome, file="../../../../../../Plots/descriptive_mcvl/YESincome.jpeg", width = 5, height = 3)

dfdif2<-
  dfincome %>% 
  filter(contr_type %in% c("permanent", "old-permanent")) %>% 
  pivot_wider(names_from = contr_type, values_from = income) %>% 
  mutate(ratio= ( permanent-`old-permanent`)/`old-permanent`,
         var="income")

gg<-
  dfdif2 %>% 
  ggplot(aes(x=date, y=ratio))+
  geom_line( color="#065465", alpha=.7)+
  geom_point(color="#065465", alpha=.8)+
  scale_y_continuous(labels = scales::percent)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  geom_vline(xintercept = c(as.Date("2021-01-01"), as.Date("2020-01-01"), as.Date("2019-01-01")), linetype="dashed")+
  theme(axis.title = element_blank())

  
  
ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/difincome.jpeg", width = 5, height = 3)


dfdiff<-rbind(dfdif2, dfdif, dfncontracts1)


gg<-
  dfdiff %>% 
  ggplot(aes(x=date, y=ratio, color=var,group=var))+
  geom_line(alpha=.7)+
  geom_point( alpha=.8)+
  scale_y_continuous(breaks = seq(-0.075, 0.01, by=0.01),labels = scales::percent)+
  scale_color_manual(labels=c("days_worked"= "N. of days worked", "income"="Income", "ncontracts"="N. of contracts"),values = c("#065465", "#008080", "#b67182"))+
  geom_vline(xintercept = as.Date("2022-01-01"))+
  geom_vline(xintercept = c(as.Date("2021-01-01"), as.Date("2020-01-01"), as.Date("2019-01-01")), linetype="dashed")+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  ylab("Ratio of permanent/old-permanent")

ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/YESdif.jpeg", width = 5, height = 3.5)


########### DURATION OF CONTRACTS ##################333

load("dfcontracts.Rdata")

##NUMBER OF CONTRACTS ENDING BEFORE 7,15,30, 90 DAYS


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


dfcontracts9<-dfcontracts9 %>% pivot_longer(cols = c(3:6), names_to = "duration", values_to = "value")


dfcontracts9$duration <- factor(dfcontracts9$duration, levels = c("below7", "below15", "below30", "below90"))

ggduration<-
  dfcontracts9 %>% 
  arrange(duration) %>% 
  filter(!is.na(contract_type), contract_type %in% c("open-ended", "permanent", "project-based")) %>% 
  ggplot(aes(x=dates, y=value, color= contract_type))+
  geom_line(alpha=.8)+
  geom_vline(xintercept = as.Date("2022-01-01"))+
    geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed" , alpha=.4)+
    geom_vline(xintercept = as.Date("2020-01-01"), linetype="dashed", alpha=.4)+
    geom_vline(xintercept = as.Date("2019-01-01"), linetype="dashed", alpha=.4)+
    theme(axis.title.x = element_blank())+
  ylab("Share of contracts")+
  scale_color_manual(values = c("permanent"="#065465", "open-ended"= "#008080","project-based"= "#b67182"))+
  facet_wrap(~duration)+
  theme(legend.position = "bottom",
        legend.title = element_blank())
  

ggsave2(ggduration, file="../../../../../../Plots/descriptive_mcvl/YESggduration.jpeg", width = 7, height = 6)


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
 
################### FIGURES BEFORE TREATMENT ###################

load("manageable_df.Rdata")


df1<-df[yearmonth==202112,]


######### BY GENDER ##########

df1 %>% 
  group_by(sex) %>% 
  summarize(var=sum(contr_type=="project-based", na.rm = T)/ sum(!is.na(contr_type), na.rm = T)*100) %>% 
  mutate(sex=ifelse(sex==1, "man", "woman"))


######### BY AGE GROUP #######


df1$age<-df1$yearmonth%/%100- df1$birth_date %/% 100


df1$age_group[df1$age<25]<- "<25"
df1$age_group[df1$age %in% c(25:34)]<- "25-34"
df1$age_group[df1$age %in% c(35:44)]<- "35-44"
df1$age_group[df1$age %in% c(45:54)]<- "45-54"
df1$age_group[df1$age %in% c(55:64)]<- "55-64"
df1$age_group[df1$age > 64]<- "65+"

gg<-
  df1 %>% 
  group_by(age_group) %>% 
  summarize(var=sum(contr_type=="project-based", na.rm = T)/ sum(!is.na(contr_type), na.rm = T)) %>% 
    mutate(label=paste0(round(var*100,2), "%")) %>% 
  ggplot(aes(x=age_group, y=var, fill=age_group))+
    geom_text(aes(label=label),
              nudge_y = 0.01,
              color="grey40")+
  geom_col(alpha=.8)+
  scale_fill_manual(values = c("#8eb1c2", "#51aff7","#006884",  "#446879", "#044766"))+
  theme(legend.title = element_blank(),
        axis.title = element_blank())+
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")


ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/prebyagegroup.jpeg", width = 5, height = 3)
##### BY SECTOR ########

df1$sector <- ifelse(df1$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
                                     61, 62, 71,72, 81, 89, 91, 99),
                    paste0("0", df1$sector),
                    df1$sector)


df1$sector<-substr(df1$sector, 1,2)


df1<-df1 %>%
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


df1$sector2 <- factor(df1$sector2, levels = rev(c(
  "AGRICULTURE, FORESTRY AND FISHING",
  "MINING AND QUARRYING",
  "MANUFACTURING",
  "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
  "WATER SUPPLY AND WASTE MANAGEMENT",
  "CONSTRUCTION",
  "WHOLESALE & REPAIR OF MOTOR VEHICLES",
  "TRANSPORTATION AND STORAGE",
  "ACCOMMODATION AND FOOD SERVICE",
  "INFORMATION AND COMMUNICATION",
  "FINANCIAL AND INSURANCE ACTIVITIES",
  "REAL ESTATE",
  "PROFESSIONAL, SCIENTIFIC AND TECHNICAL",
  "ADMINISTRATIVE & SUPPORT SERVICE ACTIVITIES",
  "PUBLIC ADMINISTRATION AND DEFENCE",
  "EDUCATION",
  "HUMAN HEALTH AND SOCIAL WORK",
  "ARTS, ENTERTAINMENT AND RECREATION",
  "OTHER SERVICES",
  "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS",
  "EXTRATERRITORIAL ORGANIZATIONS ACTIVITIES"
)))


cc<-scales::seq_gradient_pal("#9bc9be", "#0e387a", "Lab")(seq(0,1,length.out=(length(unique(df1$sector2)
                                                                                    )-1)
                                                              )
                                                          )
gg<-
  df1 %>% 
  filter(!is.na(sector2)) %>% 
  group_by(sector2) %>% 
  summarize(var=sum(contr_type=="project-based", na.rm = T)/ sum(!is.na(contr_type), na.rm = T))%>% 
    mutate(label=paste0(round(var*100,2), "%")) %>% 
  ggplot(aes(y=sector2, x=var, fill=sector2))+
  geom_col(alpha=.9)+
    geom_text(aes(label=label),
              nudge_x = 0.03,
              color="grey40",
              size=3)+
    theme(legend.title = element_blank(),
        axis.title = element_blank())+
  scale_x_continuous(labels = scales::percent)+
  scale_fill_manual(values= cc)+
  guides(fill="none")

ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/prebysector.jpeg", width = 7, height = 3)

###BY OCCUPATION #######

df1<-df1 %>% 
  mutate(group=occupation,
         group2= case_when(
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

cc<-scales::seq_gradient_pal("#9bc9be", "#0e387a", "Lab")(seq(0,1,length.out=(length(unique(df1$group2)
)-1)
)
)


df1$group2 <- factor(df1$group2, levels = rev(c("Engineers/Top Management", "Technical Engineers/Experts", "Managers",
                                                        "UntitledAssistants", "Administrative Officers", "Subordinates",
                                                        "Administrative Assistants", "1st 2nd Grade Officers","3rd Grade Officers/Specialists",
                                                        "Unqualified +18","<18 years old")))

gg<-
  df1 %>% 
  filter(!is.na(group2)) %>% 
  group_by(group2) %>% 
  summarize(var=sum(contr_type=="project-based", na.rm = T)/ sum(!is.na(contr_type), na.rm = T)) %>% 
    mutate(label=paste0(round(var*100,2), "%")) %>% 
  ggplot(aes(y=group2, x=var, fill=group2))+
  geom_col(alpha=.8)+
    geom_text(aes(label=label),
              nudge_x = 0.015,
              color="grey40",
              size=3)+
  theme(legend.title = element_blank(),
        axis.title = element_blank())+
  scale_x_continuous(labels = scales::percent)+
  scale_fill_manual(values= cc)+
  guides(fill="none")

ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/prebyoccupation.jpeg", width = 5.5, height = 3)

########### BY REGION ##############


source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Cohorts.R")

df1$person_muni_latest <- replace_province(df1$person_muni_latest)


shapefile_provincias <- readOGR("provincias/Provincias_ETRS89_30N.shp")

data_provincias <- tidy(shapefile_provincias)
nombres_provincias <- tibble(shapefile_provincias$Texto) %>% 
  mutate(id = as.character(seq(0, nrow(.)-1)))
data_provincias_mapa <- data_provincias %>% 
  left_join(nombres_provincias, by = "id")

regional_plot2 <- data_provincias_mapa%>%
  mutate(lat_c = ifelse(lat <35e5, lat + 75e4, lat),
         long_c = ifelse(long <(-5e5), (long + 65e4), long),
         id2= as.numeric(id)+1)

#Exploring min and max values to know where to draw the line
regional_plot2%>%
  filter(`shapefile_provincias$Texto` %in% c("Las Palmas", "Santa Cruz de Tenerife"))%>%
  summarize(a = min(lat_c),
            b = max(lat_c),
            c = min(long_c),
            d = max(long_c))


#Creating separate df
canaries_line <- data.frame(long = c(-354502, 134136, 134136),
                            lat = c(4036484, 4036484, 3882137))

map_df<-df1 %>% 
  group_by(person_muni_latest) %>% 
  summarize(var=sum(contr_type=="project-based", na.rm = T)/ sum(!is.na(contr_type), na.rm = T)) %>% 
  mutate(id2=as.numeric(person_muni_latest))


map_df<-left_join(regional_plot2, map_df)

gg<-
  map_df %>%
    group_by(`shapefile_provincias$Texto`) %>% 
    mutate(count=1:n(),
           label=ifelse(count==1,first(round(var*100,2)),NA),
           long_label=mean(long_c),
           lat_label=mean(lat_c)) %>% 
  ggplot() +
  geom_polygon(aes( x= long_c, 
                    y = lat_c, 
                    group = group,
                    fill=var),
               color="white",
               linewidth = 0.05 ) +
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
  geom_text(aes(label=label,
                x= long_label, 
                y = lat_label),
            color="grey20",
            size=2)+
  theme_void() +
  theme(panel.background = element_rect(linewidth= 1, color = "white", fill = "white")) +
  scale_fill_gradient(low="#9bc9be", 
                      high = "#19547b",
                      label=scales::percent)+
  scale_alpha_manual(values = c("non-relevant"=0.5, "relevant"=1))+
  guides(color="none",
         alpha="none")+
  theme(legend.title=element_blank())
ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/prebyprovince.jpeg", width = 5, height = 3)


df %>%
  filter(yearmonth %/% 100== 2021) %>% 
  group_by(time) %>% 
  summarise(temp=sum(contr_type %in% c("other temporary","project-based","Internship or training","production circumstances","Replacement")),
            pb=sum(contr_type == "project-based", na.rm = T),
            pb/temp
            )

df1 %>% 
  filter(group2=="Unqualified +18", person_muni_latest==21, sector2 == "AGRICULTURE, FORESTRY AND FISHING", sex==1, age_group=="<25")%>% 
  summarise(temp=sum(contr_type %in% c("other temporary","project-based","Internship or training","production circumstances","Replacement")),
            pb=sum(contr_type == "project-based", na.rm = T),
            pb/temp,
            n()
  )


df$person_muni_latest <- replace_province(df$person_muni_latest)
df$age<-df$yearmonth%/%100- df$birth_date %/% 100

df$sector <- ifelse(df$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
                                       61, 62, 71,72, 81, 89, 91, 99),
                     paste0("0", df$sector),
                     df$sector)


df$sector<-substr(df$sector, 1,2)




df %>% 
  filter(occupation==10, person_muni_latest==21, as.numeric(sector) %in% c(1:3), sex==1, age<25)%>% 
  summarise(temp=sum(contr_type %in% c("other temporary","project-based","Internship or training","production circumstances","Replacement")),
            pb=sum(contr_type == "project-based", na.rm = T),
            pb/temp,
            n()
  )



Sys.setlocale("LC_TIME", "English")

# Create the dataframe
df <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month"),
  time=1:36,
  var= c(rep(2, 12),rep(3,12), 3:14)
) %>% 
  mutate(month= format(date, "%B"),
         control= ifelse(time%in% c(1:24), "control", NA),
         treatment= ifelse(time %in% c(13:36), "treatment", NA))


df1<-df %>% 
  pivot_longer(names_to = "treatment", cols = c("treatment", "control")) %>% 
  filter(!is.na(value)) %>% 
  arrange(value, time) %>% 
  mutate(time2= ifelse(treatment=="treatment", time-12, time),
         month2=ifelse(time2<13, paste0(1, month), paste0(2, month)),
         var2=ifelse(value=="control", var+1, var))

df1$month2 <- factor(df1$month2, levels = c("1January", "1February", "1March", "1April", "1May", "1June", "1July", "1August", "1September", "1October", "1November", "1December",
                                            "2January", "2February", "2March", "2April", "2May", "2June", "2July", "2August", "2September", "2October", "2November", "2December"))


gg<-
  df1 %>% 
  ggplot(aes(x=month2, y=var, color=value, group=value)) +
  geom_line()+
  geom_point(alpha=.7)+
  geom_text(aes(label=format(date, "%m/%y")),
            nudge_y=.6,
            nudge_x = -.1,
            size=2,check_overlap = T)+
  geom_vline(xintercept = "2January")+
  scale_color_manual(values = c("#065465", "#008080"))+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 70, vjust = .5),
        legend.position = c(.1,.8),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave2(gg, file="../../../../../../Plots/descriptive_mcvl/example.jpeg", width = 6, height = 3)

