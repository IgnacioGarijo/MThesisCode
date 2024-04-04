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
    mutate(young= as.factor(ifelse(EDAD5<25, 1, 0)),
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
           educsup=ifelse(NFORMA=="SU", 1, 0)) %>%
    group_by(PROV, young) %>%
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
    mutate(young= as.factor(ifelse(EDAD5<25, 1, 0)),
           workingage= 1,
           employed= ifelse(TRAREM==1 | AUSENT==1,1, 0),
           parado= ifelse(employed==0 & (BUSCA==1| NUEVEM==1), 1, 0),
           activo= ifelse(employed==1 |parado==1, 1, 0)) %>%
    group_by(PROV, young) %>%
    summarize(year = first(year),
              quart = first(quarter),
              employedr = sum(employed) / sum(workingage),
              unr= sum(parado, na.rm = T) / sum(activo, na.rm = T),
              employedsum=sum(employed)
    ) %>% ungroup()
  
  # Perform left join
  df_result <- left_join(df_2020T1p, df_2020T1t) %>%
    mutate(yearq= paste(year, quarter, sep = "_"),
           dd= ifelse(rate>median(rate),1,0))
  
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


df2 <- bind_rows(
  df_2020T1, df_2020T2, df_2020T3, df_2020T4,
  df_2021T1, df_2021T2, df_2021T3, df_2021T4,
  df_2022T1, df_2022T2, df_2022T3, df_2022T4,
  df_2023T1, df_2023T2, df_2023T3
) %>% 
  group_by(PROV, young) %>%
  mutate(da= ifelse(any(yearq=="2020_1" & dd==1), 1, 0),
         db= ifelse(any(yearq=="2021_4" & dd==1), 1, 0)
         ) %>%
  ungroup() %>%
  mutate(tra= ifelse(year>2021 & da==1, 1, 0),
         trb= ifelse(year>2021 & db==1, 1, 0)) %>% 
  arrange(PROV, young) %>%
  mutate(time=rep(1:15, 104),
         timeto=time-8,
         timeto2=time-7,
         timedummy= ifelse(timeto>0,1,0),
         indiv= paste(PROV, young, sep = "_"))

rm(df_2020T1, df_2020T2, df_2020T3, df_2020T4,
df_2021T1, df_2021T2, df_2021T3, df_2021T4,
df_2022T1, df_2022T2, df_2022T3, df_2022T4,
df_2023T1, df_2023T2, df_2023T3)


save(df2, file = "Datos/df2.Rdata")
load("Datos/df2.Rdata")



df1<- df2 %>% 
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
  filter(var %in% c("ratesum", "rate")) %>% 
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

twowayfeweights(df2, "employedr", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df2, "employedsum", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df2, "unr", "indiv", "time", "tra", type = "feTR", summary_measures = T)


mod_employed = feols(employedr~ i(timeto, da, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)
mod_employedsum = feols(employedsum ~ i(timeto, da, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)
mod_unr = feols(unr ~ i(timeto, da, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)

gu<-iplot(mod_employed)
gus<-iplot(mod_employedsum)
gun<- iplot(mod_unr)

linearHypothesis(mod_employed, c("timeto::-7:da=0", 
                             "timeto::-6:da=0", 
                             "timeto::-5:da=0", 
                             "timeto::-4:da=0", 
                             "timeto::-3:da=0", 
                             "timeto::-2:da=0", 
                             "timeto::-1:da=0"))

#EFFECT ON BANNED TEMPORALITY

twowayfeweights(df2, "rate", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df2, "ratesum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_temp = feols(rate ~ i(timeto, da, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)
mod_tempsum = feols(ratesum ~ i(timeto, da, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)

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

twowayfeweights(df2, "temprate", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_temprate = feols(temprate ~ i(timeto, da, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)

gtemprate<-iplot(mod_temprate)

linearHypothesis(mod_temprate, c("timeto::-7:da=0", 
                             "timeto::-6:da=0", 
                             "timeto::-5:da=0", 
                             "timeto::-4:da=0", 
                             "timeto::-3:da=0", 
                             "timeto::-2:da=0", 
                             "timeto::-1:da=0"))





#EFFECT ON PERMANENT JOBS

twowayfeweights(df2, "permrate", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_permrate = feols(permrate ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)
mod_permsum = feols(permsum ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)


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

twowayfeweights(df2, "openendr", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df2, "openendsum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_openendr = feols(openendr ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)
mod_openendsum = feols(openendsum ~ i(timeto, da, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                      indiv+ yearq,                             ## FEs
                    cluster = ~indiv,                          ## Clustered SEs
                    data = df2)


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

twowayfeweights(df2, "jcompleta", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df2, "jcompletasum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_full = feols(jcompleta ~ i(timeto, da, ref=+0) + employedr + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)
mod_fulls = feols(jcompletasum ~ i(timeto, da, ref=+0) + employedr + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)


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

twowayfeweights(df2, "htrab", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df2, "hcontrato", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_hours = feols(htrab ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                    indiv+ yearq,                             ## FEs
                  cluster = ~indiv,                          ## Clustered SEs
                  data = df2)
mod_hourscontr = feols(hcontrato ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                    indiv+ yearq,                             ## FEs
                  cluster = ~indiv,                          ## Clustered SEs
                  data = df2)


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

twowayfeweights(df2, "otro", "indiv", "time", "tra", type = "feTR", summary_measures = T)
twowayfeweights(df2, "otrosum", "indiv", "time", "tra", type = "feTR", summary_measures = T)



mod_otro = feols(otro ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)
mod_otrosum = feols(otrosum ~ i(timeto, da, ref=+0) + employedr + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)


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


gg3<-
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

cowplot::ggsave2(filename = "gg3.jpg", plot = gg3, width = 7, height= 5)


