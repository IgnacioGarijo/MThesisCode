#By sector

library(tidyverse)
library(fixest)
library(TwoWayFEWeights)
library(car)
library(ggfixest)

theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM")

df_2020T1 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2020T1.csv")

df_2020T2 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2020T2.csv")

df_2020T3 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2020T3.csv")

df_2020T4 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2020T4.csv")

df_2021T1 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2021T1.csv")

df_2021T2 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2021T2.csv")

df_2021T3 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2021T3.csv")

df_2021T4 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2021T4.csv")

df_2022T1 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2022T1.csv")

df_2022T2 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2022T2.csv")

df_2022T3 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2022T3.csv")

df_2022T4 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2022T4.csv")

df_2023T1 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2023T1.csv")

df_2023T2 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2023T2.csv")

df_2023T3 <- read.delim("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/EPA_2023T3.csv")


#filtrar por edad menor 65 crear variable edad con media y calcular edad media, porcentaje educ terciaria, sexo, ncony!=0

#FUNCTION


process_dataframe1 <- function(input_df) {
  # Extract year and quarter from the dataframe name
  name_parts <- strsplit(deparse(substitute(input_df)), "_")[[1]]
  year <- as.numeric(substring(name_parts[2], 1, 4))
  quarter <- as.numeric(substring(name_parts[2], 6, 6))
  
  # Subset for df_2020T1t
  df_result <- input_df %>%
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
           activo= ifelse(employed==1 |parado==1, 1, 0)) %>%
    group_by(sector) %>%
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
      year = first(year),
      quart = first(quarter),
      edad= mean(EDAD5, na.rm=T),
      male= sum(male, na.rm = T)/sum(!is.na(male)),
      ncony=sum(cony, na.rm = T)/sum(!is.na(cony)),
      educsup=mean(educsup, na.rm = T),
      employedr = sum(employed) / sum(workingage),
      employedsum=sum(employed)
    ) %>%
    ungroup() %>% 
    mutate(yearq= paste(year, quarter, sep = "_"),
           dd= ifelse(rate>median(rate, na.rm=T),1,0)) %>% 
    filter(!is.na(sector))

  return(df_result)
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


df_2020T1 <- process_dataframe1(df_2020T1)
df_2020T2 <- process_dataframe1(df_2020T2)
df_2020T3 <- process_dataframe1(df_2020T3)
df_2020T4 <- process_dataframe1(df_2020T4)

df_2021T1 <- process_dataframe1(df_2021T1)
df_2021T2 <- process_dataframe1(df_2021T2)
df_2021T3 <- process_dataframe1(df_2021T3)
df_2021T4 <- process_dataframe1(df_2021T4)

df_2022T1 <- process_dataframe1(df_2022T1)
df_2022T2 <- process_dataframe1(df_2022T2)
df_2022T3 <- process_dataframe1(df_2022T3)
df_2022T4 <- process_dataframe1(df_2022T4)

df_2023T1 <- process_dataframe1(df_2023T1)
df_2023T2 <- process_dataframe1(df_2023T2)
df_2023T3 <- process_dataframe1(df_2023T3)


df4 <- bind_rows(
  df_2020T1, df_2020T2, df_2020T3, df_2020T4,
  df_2021T1, df_2021T2, df_2021T3, df_2021T4,
  df_2022T1, df_2022T2, df_2022T3, df_2022T4,
  df_2023T1, df_2023T2, df_2023T3
) %>% 
  group_by(sector) %>%
  mutate(da= ifelse(any(yearq=="2020_1" & dd==1), 1, 0),
         db= ifelse(any(yearq=="2021_4" & dd==1), 1, 0)
  ) %>%
  ungroup() %>%
  mutate(tra= ifelse(year>2021 & da==1, 1, 0),
         trb= ifelse(year>2021 & db==1, 1, 0)) %>% 
  arrange(sector) %>% 
  mutate(time=rep(1:15, 10),
         timeto=time-8,
         timeto2=time-7,
         timedummy= ifelse(timeto>0,1,0),
         indiv= sector)

rm(df_2020T1, df_2020T2, df_2020T3, df_2020T4,
   df_2021T1, df_2021T2, df_2021T3, df_2021T4,
   df_2022T1, df_2022T2, df_2022T3, df_2022T4,
   df_2023T1, df_2023T2, df_2023T3)


save(df4, file = "Datos/df4.Rdata")
load("Datos/df4.Rdata")



df1<- df4 %>% 
  group_by(da, yearq) %>% 
  summarise(employed= mean(employedr),
            employed2= mean(employedsum),
            rate= mean(rate),
            temprate= mean(temprate),
            jcompleta= mean(jcompleta),
            hcontrato= mean(hcontrato), 
            htrab= mean(htrab), 
            otro= mean(otro),
            ratesum=sum(ratesum)
  ) %>% 
  pivot_longer(cols = -c(da, yearq), names_to = "var", values_to = "value") %>% 
  arrange(yearq) %>% 
  group_by(var, da) %>% 
  mutate(timep= seq(1:15),
         timeto= timep-8) %>% 
  ungroup()

#gg1<- 
df1 %>% 
#  filter(var %in% c("ratesum", "rate")) %>% 
  ggplot(aes(x=timeto, y= value, group=as.character(da), color=as.character(da))) +
  geom_line(alpha=.85)+
  geom_point(shape=18, alpha=0.85)+
  geom_vline(xintercept = 1)+
  facet_grid(var~., scales = "free_y")+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("#49919d", "#202e53"), labels= c("Not-Treated", "Treated"))+
  theme(axis.title = element_blank(),
        legend.title = element_blank())+
  guides()




##################TREATMENT A #############################33

#EFFECTS ON employed

#First we check if we can compute a twowayfe regression without negative weights.

twowayfeweights(df4, "employedr", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df4, "employedsum", "indiv", "time", "tra", type = "feTR", summary_measures = T)


mod_employed = feols(employedr~ i(timeto, da, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df4)
mod_employedsum = feols(employedsum ~ i(timeto, da, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                          indiv+ yearq,                             ## FEs
                        cluster = ~indiv,                          ## Clustered SEs
                        data = df4)


gu<-iplot(mod_employed)
gus<-iplot(mod_employedsum)


linearHypothesis(mod_employed, c("timeto::-7:da=0", 
                                 "timeto::-6:da=0", 
                                 "timeto::-5:da=0", 
                                 "timeto::-4:da=0", 
                                 "timeto::-3:da=0", 
                                 "timeto::-2:da=0", 
                                 "timeto::-1:da=0"))

#EFFECT ON BANNED TEMPORALITY

twowayfeweights(df4, "rate", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df4, "ratesum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_temp = feols(rate ~ i(timeto, da, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df4)
mod_tempsum = feols(ratesum ~ i(timeto, da, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                      indiv+ yearq,                             ## FEs
                    cluster = ~indiv,                          ## Clustered SEs
                    data = df4)

gtemp<-iplot(mod_temp)
gtempsum<-iplot(mod_tempsum)

linearHypothesis(mod_temp, c("timeto::-7:da=0", 
                             "timeto::-6:da=0", 
                             "timeto::-5:da=0", 
                             "timeto::-4:da=0", 
                             "timeto::-3:da=0", 
                             "timeto::-2:da=0", 
                             "timeto::-1:da=0"))

linearHypothesis(mod_tempsum, c("timeto::-7:da=0", 
                                "timeto::-6:da=0", 
                                "timeto::-5:da=0", 
                                "timeto::-4:da=0", 
                                "timeto::-3:da=0", 
                                "timeto::-2:da=0", 
                                "timeto::-1:da=0"))

#EFFECT ON OVERALL TEMPORALITY

twowayfeweights(df4, "temprate", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_temprate = feols(temprate ~ i(timeto, da, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df4)

gtemprate<-iplot(mod_temprate)

linearHypothesis(mod_temprate, c("timeto::-7:da=0", 
                                 "timeto::-6:da=0", 
                                 "timeto::-5:da=0", 
                                 "timeto::-4:da=0", 
                                 "timeto::-3:da=0", 
                                 "timeto::-2:da=0", 
                                 "timeto::-1:da=0"))





#EFFECT ON PERMANENT JOBS

twowayfeweights(df4, "permrate", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_permrate = feols(permrate ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df4)
mod_permsum = feols(permsum ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                      indiv+ yearq,                             ## FEs
                    cluster = ~indiv,                          ## Clustered SEs
                    data = df4)


gpermrate<-iplot(mod_permrate)
gpermsum<-iplot(mod_permsum)


linearHypothesis(mod_permrate, c("timeto::-7:da=0", 
                                 "timeto::-6:da=0", 
                                 "timeto::-5:da=0", 
                                 "timeto::-4:da=0", 
                                 "timeto::-3:da=0", 
                                 "timeto::-2:da=0", 
                                 "timeto::-1:da=0"))

#EFFECT ON INTERMITTENT OPEN-ENDED

twowayfeweights(df4, "openendr", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df4, "openendsum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_openendr = feols(openendr ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df4)
mod_openendsum = feols(openendsum ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                         indiv+ yearq,                             ## FEs
                       cluster = ~indiv,                          ## Clustered SEs
                       data = df4)


gopenendr<-iplot(mod_openendr)
gopenendsum<-iplot(mod_openendsum)


linearHypothesis(mod_openendr, c("timeto::-7:da=0", 
                                 "timeto::-6:da=0", 
                                 "timeto::-5:da=0", 
                                 "timeto::-4:da=0", 
                                 "timeto::-3:da=0", 
                                 "timeto::-2:da=0", 
                                 "timeto::-1:da=0"))



#EFFECT ON FULLTIME

twowayfeweights(df4, "jcompleta", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df4, "jcompletasum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_full = feols(jcompleta ~ i(timeto, da, ref=+0) + employedr + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df4)
mod_fulls = feols(jcompletasum ~ i(timeto, da, ref=+0) + employedr + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                    indiv+ yearq,                             ## FEs
                  cluster = ~indiv,                          ## Clustered SEs
                  data = df4)


gfull<-iplot(mod_full)
gfulls<-iplot(mod_fulls)

linearHypothesis(mod_full, c("timeto::-7:da=0", 
                             "timeto::-6:da=0", 
                             "timeto::-5:da=0", 
                             "timeto::-4:da=0", 
                             "timeto::-3:da=0", 
                             "timeto::-2:da=0", 
                             "timeto::-1:da=0"))
linearHypothesis(mod_fulls, c("timeto::-7:da=0", 
                              "timeto::-6:da=0", 
                              "timeto::-5:da=0", 
                              "timeto::-4:da=0", 
                              "timeto::-3:da=0", 
                              "timeto::-2:da=0", 
                              "timeto::-1:da=0"))

#EFFECT ON HOURS WORKED

twowayfeweights(df4, "htrab", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df4, "hcontrato", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_hours = feols(htrab ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                    indiv+ yearq,                             ## FEs
                  cluster = ~indiv,                          ## Clustered SEs
                  data = df4)
mod_hourscontr = feols(hcontrato ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                         indiv+ yearq,                             ## FEs
                       cluster = ~indiv,                          ## Clustered SEs
                       data = df4)


ghours<-iplot(mod_hours)
hourscontr<- iplot(mod_hourscontr)

linearHypothesis(mod_hours, c("timeto::-7:da=0", 
                              "timeto::-6:da=0", 
                              "timeto::-5:da=0", 
                              "timeto::-4:da=0", 
                              "timeto::-3:da=0", 
                              "timeto::-2:da=0", 
                              "timeto::-1:da=0"))

#EFFECT ON HOLDING OTHER JOBS

twowayfeweights(df4, "otro", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df4, "otrosum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_otro = feols(otro ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df4)
mod_otrosum = feols(otrosum ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                      indiv+ yearq,                             ## FEs
                    cluster = ~indiv,                          ## Clustered SEs
                    data = df4)


gother<-iplot(mod_otro, main="Effect on holding a second job")
gothersum<-iplot(mod_otrosum, main="Effect on holding a second job")


linearHypothesis(mod_otro, c("timeto::-7:da=0", 
                             "timeto::-6:da=0", 
                             "timeto::-5:da=0", 
                             "timeto::-4:da=0", 
                             "timeto::-3:da=0", 
                             "timeto::-2:da=0", 
                             "timeto::-1:da=0"))

#FINAL GRAPH

paleta<- c("#ce6a6c", "#ebada2", "#5fb3b3", "#49919d", "#202e53", "#92b481", "#b0a1c1","#b9c3c7") 


#gg3<-
  ggiplot(list("unemployed"=mod_employed, 
               "Banned temp. rate"=mod_temp,
               "Overall temp. rate"= mod_temprate,
               "Permanent rate"= mod_permrate,
               "Intermittent open-ended"=mod_openendr,
               "Second job"=mod_otro, 
               "Full time"=mod_full,
               "Hours worked"=mod_hours), 
          pt.join=TRUE, 
          pt.pch=19,
          multi_style = 'facet',
          facet_args = list(ncol=2, 
                            scales= "free_y",
                            labeller= labeller(category="")
          ),
          main= "Policy treatment effects"
  )+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  scale_color_manual(values = paleta, aesthetics = c("color", "fill"))+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

#cowplot::ggsave2(filename = "gg3.jpg", plot = gg3, width = 7, height= 5)


  
  
  ############################## SYNTHETHIC CONTROL METHOD ################################
  library(gsynth)
  
  #Employment
  out1 <- gsynth(employedsum ~ trb + rate+ jcompleta + educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out1, raw="band", main = "employed")
  
  
  #Employment only with previous week workers
  out2 <- gsynth(ezhsum ~ trb +employedr+ rate+ jcompleta + educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out2, raw="band", main = "0 hour employed")
  
  #Rate of project-based contracts
  
  out3 <- gsynth(rate ~ trb +employedr+ jcompleta + educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out3, raw = "band")
  
  # Overall temporary rate
  
  out4 <- gsynth(temprate ~ trb +employedr+ jcompleta + educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out4, raw="band")
  
  #Permanent rate
  
  out5 <- gsynth(permrate ~ trb +employedr+ jcompleta + educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out5)
  
  #Open ended
  
  out6 <- gsynth(openendr ~ trb +employedr+ jcompleta + educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out6)
  
  # Full time
  
  out7 <- gsynth(jcompletasum ~ trb +employedr+rate+ educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out7, type="counterfactual")
  
  # Hours worked
  
  out8 <- gsynth(htrab ~ trb +employedr+ jcompleta+ rate+ educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out8)
  
  
  # Hours contract
  
  out8 <- gsynth(hcontrato ~ trb +employedr+ jcompleta+ rate+ educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out8)
  
  #other job
  
  out9 <- gsynth(otrosum ~ trb +employedr+ jcompleta+ rate+ educsup, data = df4, 
                 index = c("indiv","time"), force = "two-way", 
                 CV = TRUE, se = TRUE, 
                 inference = "parametric", nboots = 1000, 
                 parallel = FALSE)
  
  plot(out9)
  
