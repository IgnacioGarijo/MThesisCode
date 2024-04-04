#Merging all datasets

library(tidyverse)
library(fixest)
library(TwoWayFEWeights)
library(car)
library(ggfixest)
library(gsynth)

theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM")


quarters <- c("2020T1", "2020T2", "2020T3", "2020T4",
              "2021T1", "2021T2", "2021T3", "2021T4",
              "2022T1", "2022T2", "2022T3", "2022T4",
              "2023T1", "2023T2", "2023T3")


for (quarter in quarters) {
  load(paste0("Datos/df_", quarter, ".Rdata"))
}

process_dataframe2 <- function(input_df) {
  input_df<-input_df %>%
    mutate(EDAD5=EDAD1)
  return(input_df)
}



df_2021T1 <- process_dataframe2(df_2021T1)
df_2021T2 <- process_dataframe2(df_2021T2)
df_2021T3 <- process_dataframe2(df_2021T3)
df_2021T4 <- process_dataframe2(df_2021T4)

df_2022T1 <- process_dataframe2(df_2022T1)
df_2022T2 <- process_dataframe2(df_2022T2)
df_2022T3 <- process_dataframe2(df_2022T3)
df_2022T4 <- process_dataframe2(df_2022T4)

df_2023T1 <- process_dataframe2(df_2023T1)
df_2023T2 <- process_dataframe2(df_2023T2)
df_2023T3 <- process_dataframe2(df_2023T3)

df_vector <- list(df_2020T1, df_2020T2, df_2020T3, df_2020T4, df_2021T1, df_2021T2, df_2021T3, df_2022T1, df_2022T2, df_2022T3, df_2022T4, df_2023T1, df_2023T2, df_2023T3)


# NEED TO ADD YEARS

df <- df6 %>%
  filter(EDAD5 %in% 16:65) %>%
  mutate(young= as.factor(ifelse(EDAD5<25, 1, 0)),
         sector=  case_when(
           ACT1 == 0 ~ "Agriculture, Livestock, Forestry, and Fishing",
           ACT1 == 1 ~ "Food, Textile, Leather, Wood, and Paper Industry",
           ACT1 == 2 ~ "Extractive Industries, Petroleum Refining, Chemical Industry, Pharmaceutical Industry, Rubber and Plastic Materials Industry, Electricity, Gas, Steam, and Air Conditioning Supply, Water Supply, Waste Management, Metallurgy",
           ACT1 == 3 ~ "Machinery Construction, Electrical Equipment, and Transportation Material Construction, Industrial Installation and Repair",
           ACT1 == 4 ~ "Construction",
           ACT1 == 5 ~ "Wholesale and Retail Trade, Repair of Automobiles, Hospitality",
           ACT1 == 6 ~ "Transportation and Storage, Information and Communications",
           ACT1 == 7 ~ "Financial Intermediation, Insurance, Real Estate Activities, Professional, Scientific, Administrative, and Other Services",
           ACT1 == 8 ~ "Public Administration, Education, and Health Activities",
           ACT1 == 9 ~ "Other Services"
         ),
         profession= case_when(
           OCUP1 == 0 ~ "Military Occupations",
           OCUP1 == 1 ~ "Directors and Managers",
           OCUP1 == 2 ~ "Technical and Professional Scientists and Intellectuals",
           OCUP1 == 3 ~ "Technical and Support Professionals",
           OCUP1 == 4 ~ "Accounting, Administrative, and Other Office Employees",
           OCUP1 == 5 ~ "Workers in Catering, Personal Services, Protection, and Sales",
           OCUP1 == 6 ~ "Skilled Workers in Agriculture, Livestock, Forestry, and Fishing",
           OCUP1 == 7 ~ "Artisans and Skilled Workers in Manufacturing Industries and Construction",
           OCUP1 == 8 ~ "Machine Operators and Assemblers",
           OCUP1 == 9 ~ "Elementary Occupations"
         ),
         oys = ifelse(DUCON1 == 6 & DUCON3 %in% c(3, 6), 1, 0),
         asalariado= ifelse(SITU%in%c(7,8), 1,0),
         tempr= ifelse(DUCON1==6, 1, 0),
         permr=ifelse(DUCON1==1 & DUCON2==1, 1, 0),
         openended= ifelse(DUCON1==1 & DUCON2==6,1,0),
         jcomplet= ifelse(PARCO1==1, 1, 0),
         horaspa= ifelse(HORASP==9999, NA, HORASP/100),
         horasha= ifelse(HORASH==9999, NA, HORASH/100),
         tramas= ifelse(TRAPLU==1, 1, 0),
         male= ifelse(SEXO1==1, 1, 0),
         cony=ifelse(NCONY!=0, 1, 0),
         educsup=ifelse(NFORMA=="SU", 1, 0),
         empnohours= ifelse((TRAREM==1 | AUSENT==1)& HORASE==0, 1, 0),
         freelance= ifelse(SITU==3, 1, 0),
         ocupado= ifelse(TRAREM==1 | AUSENT==1, 1, 0),
         workingage= 1,
         employed= ifelse(TRAREM==1 | AUSENT==1,1, 0),
         parado= ifelse(employed==0 & (BUSCA==1| NUEVEM==1), 1, 0),
         activo= ifelse(employed==1 |parado==1, 1, 0),
         worked= ifelse(HORASE>0,1,0),
         nwe=ifelse(employed==1 & worked==1, 1,0)) %>%
  group_by(profession) %>%
  summarize(
    rate = sum(oys, na.rm = T) / sum(asalariado, na.rm = T),
    ratesum=sum(oys, na.rm = T),
    temprate= sum(tempr, na.rm = T)/sum(asalariado, na.rm = T),
    permrate= sum(permr, na.rm = T)/sum(asalariado, na.rm = T),
    openendr=sum(openended, na.rm = T)/sum(asalariado, na.rm = T),
    openendsum= sum(openended, na.rm = T),
    permsum= sum(permr, na.rm = T),
    jcompleta = sum(jcomplet, na.rm = T) / sum(asalariado, na.rm = T),
    jcompletasum= sum(jcomplet, na.rm = T),
    hcontrato = mean(horaspa, na.rm = TRUE),
    htrab = mean(horasha, na.rm = TRUE),
    ezhsum= sum(empnohours, na.rm = T),
    ezhr= sum(empnohours, na.rm=T)/ sum(asalariado, na.rm = T),
    freelsum= sum(freelance,na.rm = T),
    freer=sum(freelance, na.rm = T)/sum(ocupado, na.rm=T),
    otro = sum(tramas, na.rm = T) / sum(asalariado, na.rm = T),
    otrosum= sum(tramas, na.rm = T),
    edad= mean(EDAD5, na.rm=T),
    male= sum(male, na.rm = T)/sum(!is.na(male)),
    ncony=sum(cony, na.rm = T)/sum(!is.na(cony)),
    educsup=mean(educsup, na.rm = T),
    employedr = sum(employed) / sum(workingage),
    employedsum=sum(employed),
    ewdsum= sum(nwe, na.rm=T),
    ewdr=sum(nwe, na.rm = T)/sum(asalariado, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(!is.na(profession))




