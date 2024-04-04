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
  df_2020T1t <- input_df %>%
    filter(EDAD5 %in% 16:65) %>%
    group_by(PROV) %>%
    mutate(oys = ifelse(DUCON1 == 6 & DUCON3 %in% c(3, 6), 1, 0),
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
           ocupado= ifelse(TRAREM==1 | AUSENT==1, 1, 0)
           ) %>%
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
      otro = sum(tramas, na.rm = T) / sum(asalariado, na.rm = T),
      ezhsum= sum(empnohours, na.rm = T),
      ezhr= sum(empnohours, na.rm=T)/ sum(asalariado, na.rm = T),
      freelsum= sum(freelance,na.rm = T),
      freer=sum(freelance, na.rm = T)/sum(ocupado, na.rm=T),
      otrosum= sum(tramas, na.rm = T),
      year = first(year),
      quart = first(quarter),
      edad= mean(EDAD5, na.rm=T),
      male= sum(male, na.rm = T)/sum(!is.na(male)),
      ncony=sum(cony, na.rm = T)/sum(!is.na(cony)),
      educsup=mean(educsup, na.rm = T)
    ) %>%
    ungroup()

  # Subset for df_2020T1p
  df_2020T1p <- input_df %>%
    filter(!is.na(TRAREM), EDAD5 %in% 16:65) %>%
    mutate(workingage= 1,
           employed= ifelse(TRAREM==1 | AUSENT==1,1, 0),
           parado= ifelse(employed==0 & (BUSCA==1| NUEVEM==1), 1, 0),
           activo= ifelse(employed==1 |parado==1, 1, 0)) %>%
    group_by(PROV) %>%
    summarize(year = first(year),
              quart = first(quarter),
              employedr = sum(employed) / sum(workingage),
              unr= sum(parado, na.rm = T) / sum(activo, na.rm = T),
              employedsum=sum(employed)
    )

  # Perform left join
  df_result <- left_join(df_2020T1p, df_2020T1t) %>%
    mutate(yearq= paste(year, quarter, sep = "_"),
           hightemp= ifelse(rate>median(rate),1,0))

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





df <- bind_rows(
  df_2020T1, df_2020T2, df_2020T3, df_2020T4,
  df_2021T1, df_2021T2, df_2021T3, df_2021T4,
  df_2022T1, df_2022T2, df_2022T3, df_2022T4,
  df_2023T1, df_2023T2, df_2023T3
)%>%
  group_by(PROV) %>%
  mutate(hightemp= ifelse(any(yearq=="2020_1" & hightemp==1), 1, 0)) %>%
  ungroup() %>%
  mutate(treatment= ifelse(year>2021 & hightemp==1, 1, 0)) %>%
  arrange(PROV) %>%
  mutate(time=rep(1:15, 52),
         timeto=time-8,
         timedummy= ifelse(timeto>0,1,0))

rm(df_2020T1, df_2020T2, df_2020T3, df_2020T4,
   df_2021T1, df_2021T2, df_2021T3, df_2021T4,
   df_2022T1, df_2022T2, df_2022T3, df_2022T4,
   df_2023T1, df_2023T2, df_2023T3)


save(df, file = "Datos/df.Rdata")
load("Datos/df.Rdata")


df1<- df %>% 
  group_by(hightemp, yearq) %>% 
  summarise(rate= mean(rate),
            temprate= mean(temprate),
            jcompleta= mean(jcompleta),
            hcontrato= mean(hcontrato), 
            htrab= mean(htrab), 
            otro= mean(otro),
            ezhsum= mean(ezhsum),
            ezhrate=mean(ezhr),
            freelsum= mean(freelsum),
            freer=mean(freer)
            ) %>% 
  pivot_longer(cols = -c(hightemp, yearq), names_to = "var", values_to = "value") %>% 
  arrange(yearq) %>% 
  group_by(var, hightemp) %>% 
  mutate(timep= seq(1:15),
         timeto= timep-8) %>% 
  ungroup()
  
#gg1<- 
  df1 %>% 
    filter(var %in% c("ezhrate","ezhsum", "freer","freelsum")) %>% 
ggplot(aes(x=timeto, y= value, group=as.character(hightemp), color=as.character(hightemp))) +
  geom_line(alpha=.85)+
  geom_point(shape=18, alpha=0.85)+
  geom_vline(xintercept = 1)+
  facet_grid(var~., scales = "free_y")+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("#49919d", "#202e53"), labels= c("Not-Treated", "Treated"))+
  theme(axis.title = element_blank(),
        legend.title = element_blank())+
  guides()

#cowplot::ggsave2(filename = "gg1.jpg", plot = gg1, width = 6, height = 7)

  #EFFECTS ON UNEMPLOYMENT

 #First we check if we can compute a twowayfe regression without negative weights.

  twowayfeweights(df, "employedr", "PROV", "time", "treatment", type = "feTR", summary_measures = T)
  twowayfeweights(df, "employedsum", "PROV", "time", "treatment", type = "feTR", summary_measures = T)
  twowayfeweights(df, "unr", "PROV", "time", "treatment", type = "feTR", summary_measures = T)
  
  
  mod_employed = feols(employedr~ i(timeto, hightemp, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                         PROV+ yearq,                             ## FEs
                       cluster = ~PROV,                          ## Clustered SEs
                       data = df)
  mod_employedsum = feols(employedsum ~ i(timeto, hightemp, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                            PROV+ yearq,                             ## FEs
                          cluster = ~PROV,                          ## Clustered SEs
                          data = df)
  mod_unr = feols(unr ~ i(timeto, hightemp, ref = +0) + employedsum+ rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                    PROV+ yearq,                             ## FEs
                  cluster = ~PROV,                          ## Clustered SEs
                  data = df[df$yearq!="2020_1",])
  
  gu<-iplot(mod_employed)
  gus<-iplot(mod_employedsum)
  gun<- iplot(mod_unr)

linearHypothesis(mod_employed, c("timeto::-7:hightemp=0", 
                             "timeto::-6:hightemp=0", 
                             "timeto::-5:hightemp=0", 
                             "timeto::-4:hightemp=0", 
                             "timeto::-3:hightemp=0", 
                             "timeto::-2:hightemp=0", 
                             "timeto::-1:hightemp=0"))

linearHypothesis(mod_unr, c("timeto::-6:hightemp=0", 
                                 "timeto::-5:hightemp=0", 
                                 "timeto::-4:hightemp=0", 
                                 "timeto::-3:hightemp=0", 
                                 "timeto::-2:hightemp=0", 
                                 "timeto::-1:hightemp=0"))

#EFFECT ON BANNED TEMPORALITY

twowayfeweights(df, "rate", "PROV", "time", "treatment", type = "feTR", summary_measures = T)



mod_temp = feols(rate ~ i(timeto, hightemp, ref=+0) + employedsum + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)
mod_tempsum = feols(ratesum ~ i(timeto, hightemp, ref=+0) + employedsum + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)


gtemp<-iplot(mod_temp)
gtempsum<-iplot(mod_tempsum)


linearHypothesis(mod_tempsum, c("timeto::-7:hightemp=0", 
                             "timeto::-6:hightemp=0", 
                             "timeto::-5:hightemp=0", 
                             "timeto::-4:hightemp=0", 
                             "timeto::-3:hightemp=0", 
                             "timeto::-2:hightemp=0", 
                             "timeto::-1:hightemp=0"))


#EFFECT ON OVERALL TEMPORALITY


twowayfeweights(df, "temprate", "PROV", "time", "treatment", type = "feTR", summary_measures = T)



mod_temprate = feols(temprate ~ i(timeto, hightemp, ref=+0) + employedsum + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)



gtemprate<-iplot(mod_temprate)


linearHypothesis(mod_temprate, c("timeto::-7:hightemp=0", 
                                "timeto::-6:hightemp=0", 
                                "timeto::-5:hightemp=0", 
                                "timeto::-4:hightemp=0", 
                                "timeto::-3:hightemp=0", 
                                "timeto::-2:hightemp=0",
                                "timeto::-1:hightemp=0"))

#EFFECT ON PERMANENT JOBS

twowayfeweights(df, "permrate", "PROV", "time", "treatment", type = "feTR", summary_measures = T)



mod_permrate = feols(permrate ~ i(timeto, hightemp, ref=+0) + employedsum + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)
mod_permsum = feols(permsum ~ i(timeto, hightemp, ref=+0) + employedsum + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       PROV+ yearq,                             ## FEs
                     cluster = ~PROV,                          ## Clustered SEs
                     data = df)


gpermrate<-iplot(mod_permrate)
gpermsum<-iplot(mod_permsum)


linearHypothesis(mod_permsum, c("timeto::-7:hightemp=0", 
                             "timeto::-6:hightemp=0", 
                             "timeto::-5:hightemp=0", 
                             "timeto::-4:hightemp=0", 
                             "timeto::-3:hightemp=0", 
                             "timeto::-2:hightemp=0", 
                             "timeto::-1:hightemp=0"))




#EFFECT ON INTERMITTENT OPEN ENDED

twowayfeweights(df, "openendr", "PROV", "time", "treatment", type = "feTR", summary_measures = T)
twowayfeweights(df, "openendsum", "PROV", "time", "treatment", type = "feTR", summary_measures = T)



mod_openendr = feols(openendr ~ i(timeto, hightemp, ref=+0) + employedsum + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       PROV+ yearq,                             ## FEs
                     cluster = ~PROV,                          ## Clustered SEs
                     data = df)
mod_openendsum = feols(openendsum ~ i(timeto, hightemp, ref=+0) + employedsum + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                      PROV+ yearq,                             ## FEs
                    cluster = ~PROV,                          ## Clustered SEs
                    data = df)


gopenendr<-iplot(mod_openendr)
gopenendsum<-iplot(mod_openendsum)


linearHypothesis(mod_permsum, c("timeto::-7:hightemp=0", 
                                "timeto::-6:hightemp=0", 
                                "timeto::-5:hightemp=0", 
                                "timeto::-4:hightemp=0", 
                                "timeto::-3:hightemp=0", 
                                "timeto::-2:hightemp=0", 
                                "timeto::-1:hightemp=0"))



#EFFECT ON FULLTIME

twowayfeweights(df, "jcompleta", "PROV", "time", "treatment", type = "feTR", summary_measures = T)
twowayfeweights(df, "jcompletasum", "PROV", "time", "treatment", type = "feTR", summary_measures = T)



mod_full = feols(jcompleta ~ i(timeto, hightemp, ref=+0) + employedsum + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)
mod_fullsum = feols(jcompletasum ~ i(timeto, hightemp, ref=+0) + employedsum + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)


gfull<-iplot(mod_full)
gfullsum<-iplot(mod_fullsum)


linearHypothesis(mod_fullsum, c("timeto::-7:hightemp=0", 
                             "timeto::-6:hightemp=0", 
                             "timeto::-5:hightemp=0", 
                             "timeto::-4:hightemp=0", 
                             "timeto::-3:hightemp=0", 
                             "timeto::-2:hightemp=0", 
                             "timeto::-1:hightemp=0"))

#EFFECT ON HOURS WORKED

twowayfeweights(df, "htrab", "PROV", "time", "treatment", type = "feTR", summary_measures = T)



mod_hours = feols(htrab ~ i(timeto, hightemp, ref=+0) + employedsum + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)


ghours<-iplot(mod_hours)

linearHypothesis(mod_hours, c("timeto::-7:hightemp=0", 
                             "timeto::-6:hightemp=0", 
                             "timeto::-5:hightemp=0", 
                             "timeto::-4:hightemp=0", 
                             "timeto::-3:hightemp=0", 
                             "timeto::-2:hightemp=0", 
                             "timeto::-1:hightemp=0"))

#EFFECT ON HOLDING OTHER JOBS

twowayfeweights(df, "otro", "PROV", "time", "treatment", type = "feTR", summary_measures = T)
twowayfeweights(df, "otrosum", "PROV", "time", "treatment", type = "feTR", summary_measures = T)



mod_otro = feols(otro ~ i(timeto, hightemp, ref=+0) + employedsum + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                    PROV+ yearq,                             ## FEs
                  cluster = ~PROV,                          ## Clustered SEs
                  data = df)
mod_otrosum = feols(otrosum ~ i(timeto, hightemp, ref=+0) + employedsum + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)


gother<-iplot(mod_otro, main="Effect on holding a second job")
gothersum<-iplot(mod_otrosum, main="Effect on holding a second job")


linearHypothesis(mod_otro, c("timeto::-7:hightemp=0", 
                              "timeto::-6:hightemp=0", 
                              "timeto::-5:hightemp=0", 
                              "timeto::-4:hightemp=0", 
                              "timeto::-3:hightemp=0", 
                              "timeto::-2:hightemp=0", 
                              "timeto::-1:hightemp=0"))


##EFFECT ON UNEMPLOYMENT WITHOUT ZERO HOURS

mod_ezhr = feols(ezhr~ i(timeto, hightemp, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       PROV+ yearq,                             ## FEs
                     cluster = ~PROV,                          ## Clustered SEs
                     data = df)
mod_ezhsum = feols(ezhsum~ i(timeto, hightemp, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       PROV+ yearq,                             ## FEs
                     cluster = ~PROV,                          ## Clustered SEs
                     data = df)

gezhr<-iplot(mod_ezhr)
gezhsum<-iplot(mod_ezhsum)


##EFFECT ON FREELANCERS


mod_freer = feols(freer~ i(timeto, hightemp, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df)
mod_freelsum = feols(freelsum~ i(timeto, hightemp, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                     PROV+ yearq,                             ## FEs
                   cluster = ~PROV,                          ## Clustered SEs
                   data = df)

gefreer<-iplot(mod_freer)
gefreelsum<-iplot(mod_freelsum)

df$hightemp<- as.numeric(df$hightemp)
df<- df %>% 
  mutate(hightemp1=as.numeric(hightemp))
dataprep_out <-
  dataprep(
    foo = df,
    predictors    = c("rate","jcompleta","htrab", "otro", "edad", "male", "ncony", "educsup"),
    dependent     = "freer",
    unit.variable = "hightemp1",
    time.variable = "time",
    treatment.identifier = 1,
    controls.identifier = 0,
    time.predictors.prior = 1:8,
    time.optimize.ssr = 1:8,
    #unit.names.variable = "country",
    time.plot = 1:15
  )


#FINAL GRAPH

paletoncia<- c("#ce6a6c", "#ebada2", "#5fb3b3", "#49919d", "#202e53", "#92b481", "#b0a1c1","#b9c3c7") 


gg2<-
  ggiplot(list("Employment rate"=mod_employed,
              "Banned temp. contracts"=mod_tempsum,
              "Overall temporality rate"=mod_temprate,
              "Permanent contracts"= mod_permsum,
              "Intermittent open-ended"=mod_openendsum,
              "Second job share"=mod_otro, 
             "Full time"=mod_fullsum,
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
  scale_color_manual(values = paletoncia, aesthetics = c("color", "fill"))+
  geom_point(size=2, alpha=.7)+
  geom_line(linewidth=1, alpha=.7)

cowplot::ggsave2(filename = "gg2.jpg", plot = gg2, width = 7, height= 5)

#TRY AGAIN
  
df2<-df %>%
  group_by(yearq) %>% 
  mutate(treatmentt= case_when(rate>=1.2*median(rate)~ 1,
                               rate<=.8*median(rate) ~0)) %>% 
  ungroup() %>% 
  group_by(PROV) %>%
  mutate(hightemp2 = treatmentt[yearq == "2020_1"]) %>% 
  filter(!is.na(hightemp2))

table(is.na(df2$treatmentt), is.na(df2$treatment2))  


#terminar de ver el nuevo tratamiento
#Efecto en temporalidad, no en rate porque temporalidad engloba mas gente y tipos de contrato




############################## SYNTHETHIC CONTROL METHOD ################################

#Employment
out1 <- gsynth(employedr ~ treatment + rate+ jcompleta + educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out1, raw="band")


#Employment only with previous week workers
out2 <- gsynth(ewdr ~ treatment +employedr+ rate+ jcompleta + educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out2, raw="band")

#Rate of project-based contracts

out3 <- gsynth(rate ~ treatment +employedr+ jcompleta + educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out3)

# Overall temporary rate

out4 <- gsynth(temprate ~ treatment +employedr+ jcompleta + educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out4, raw="ci")

#Permanent rate

out5 <- gsynth(permrate ~ treatment +employedr+ jcompleta + educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out5)

#Open ended

out6 <- gsynth(openendr ~ treatment +employedr+ jcompleta + educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out6)

# Full time

out7 <- gsynth(jcompletasum ~ treatment +employedr+rate+ educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out7, type="counterfactual")

# Hours worked

out8 <- gsynth(htrab ~ treatment +employedr+ jcompleta+ rate+ educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out8)


# Hours contract

out8 <- gsynth(hcontrato ~ treatment +employedr+ jcompleta+ rate+ educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out8)

#other job

out9 <- gsynth(otrosum ~ treatment +employedr+ jcompleta+ rate+ educsup, data = df, 
               index = c("PROV","time"), force = "two-way", 
               CV = TRUE, se = TRUE, 
               inference = "parametric", nboots = 1000, 
               parallel = FALSE)

plot(out9)


