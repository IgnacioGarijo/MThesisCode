#This code aggregates by age group and region and uses treatment b (who had a high temporary rate right before the reform)

library(tidyverse)
library(fixest)
library(TwoWayFEWeights)
library(car)
library(ggfixest)

theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM")
load("Datos/df2.Rdata")


twowayfeweights(df2, "employedr", "indiv", "time", "trb", type = "feTR", summary_measures = T)
twowayfeweights(df2, "employedsum", "indiv", "time", "trb", type = "feTR", summary_measures = T)
twowayfeweights(df2, "unr", "indiv", "time", "trb", type = "feTR", summary_measures = T)


mod_employed = feols(employedr~ i(timeto, db, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)
mod_employedsum = feols(employedsum ~ i(timeto, db, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                          indiv+ yearq,                             ## FEs
                        cluster = ~indiv,                          ## Clustered SEs
                        data = df2)
mod_unr = feols(unr ~ i(timeto, db, ref = +0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                  indiv+ yearq,                             ## FEs
                cluster = ~indiv,                          ## Clustered SEs
                data = df2)

gu<-iplot(mod_employed)
gus<-iplot(mod_employedsum)
gun<- iplot(mod_unr)

linearHypothesis(mod_employed, c("timeto::-7:db=0", 
                                 "timeto::-6:db=0", 
                                 "timeto::-5:db=0", 
                                 "timeto::-4:db=0", 
                                 "timeto::-3:db=0", 
                                 "timeto::-2:db=0", 
                                 "timeto::-1:db=0"))

#EFFECT ON BANNED TEMPORALITY

twowayfeweights(df2, "rate", "indiv", "time", "trb", type = "feTR", summary_measures = T)
twowayfeweights(df2, "ratesum", "indiv", "time", "trb", type = "feTR", summary_measures = T)



mod_temp = feols(rate ~ i(timeto, db, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)
mod_tempsum = feols(ratesum ~ i(timeto, db, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                      indiv+ yearq,                             ## FEs
                    cluster = ~indiv,                          ## Clustered SEs
                    data = df2)

gtemp<-iplot(mod_temp)
gtempsum<-iplot(mod_tempsum)

linearHypothesis(mod_temp, c("timeto::-7:db=0", 
                             "timeto::-6:db=0", 
                             "timeto::-5:db=0", 
                             "timeto::-4:db=0", 
                             "timeto::-3:db=0", 
                             "timeto::-2:db=0", 
                             "timeto::-1:db=0"))

linearHypothesis(mod_tempsum, c("timeto::-7:db=0", 
                                "timeto::-6:db=0", 
                                "timeto::-5:db=0", 
                                "timeto::-4:db=0", 
                                "timeto::-3:db=0", 
                                "timeto::-2:db=0", 
                                "timeto::-1:db=0"))

#EFFECT ON OVERALL TEMPORALITY

twowayfeweights(df2, "temprate", "indiv", "time", "trb", type = "feTR", summary_measures = T)



mod_temprate = feols(temprate ~ i(timeto, db, ref=+0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)

gtemprate<-iplot(mod_temprate)

linearHypothesis(mod_temprate, c("timeto::-7:db=0", 
                                 "timeto::-6:db=0", 
                                 "timeto::-5:db=0", 
                                 "timeto::-4:db=0", 
                                 "timeto::-3:db=0", 
                                 "timeto::-2:db=0", 
                                 "timeto::-1:db=0"))





#EFFECT ON PERMANENT JOBS

twowayfeweights(df2, "permrate", "indiv", "time", "trb", type = "feTR", summary_measures = T)



mod_permrate = feols(permrate ~ i(timeto, db, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)
mod_permsum = feols(permsum ~ i(timeto, db, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                      indiv+ yearq,                             ## FEs
                    cluster = ~indiv,                          ## Clustered SEs
                    data = df2)


gpermrate<-iplot(mod_permrate)
gpermsum<-iplot(mod_permsum)


linearHypothesis(mod_permrate, c("timeto::-7:db=0", 
                                 "timeto::-6:db=0", 
                                 "timeto::-5:db=0", 
                                 "timeto::-4:db=0", 
                                 "timeto::-3:db=0", 
                                 "timeto::-2:db=0", 
                                 "timeto::-1:db=0"))

#EFFECT ON INTERMITTENT OPEN-ENDED

twowayfeweights(df2, "openendr", "indiv", "time", "trb", type = "feTR", summary_measures = T)
twowayfeweights(df2, "openendsum", "indiv", "time", "trb", type = "feTR", summary_measures = T)



mod_openendr = feols(openendr ~ i(timeto, db, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                       indiv+ yearq,                             ## FEs
                     cluster = ~indiv,                          ## Clustered SEs
                     data = df2)
mod_openendsum = feols(openendsum ~ i(timeto, db, ref = +0) + employedr + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                         indiv+ yearq,                             ## FEs
                       cluster = ~indiv,                          ## Clustered SEs
                       data = df2)


gopenendr<-iplot(mod_openendr)
gopenendsum<-iplot(mod_openendsum)


linearHypothesis(mod_openendr, c("timeto::-7:db=0", 
                                 "timeto::-6:db=0", 
                                 "timeto::-5:db=0", 
                                 "timeto::-4:db=0", 
                                 "timeto::-3:db=0", 
                                 "timeto::-2:db=0", 
                                 "timeto::-1:db=0"))



#EFFECT ON FULLTIME

twowayfeweights(df2, "jcompleta", "indiv", "time", "trb", type = "feTR", summary_measures = T)
twowayfeweights(df2, "jcompletasum", "indiv", "time", "trb", type = "feTR", summary_measures = T)



mod_full = feols(jcompleta ~ i(timeto, db, ref=+0) + employedr + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)
mod_fulls = feols(jcompletasum ~ i(timeto, db, ref=+0) + employedr + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                    indiv+ yearq,                             ## FEs
                  cluster = ~indiv,                          ## Clustered SEs
                  data = df2)


gfull<-iplot(mod_full)
gfulls<-iplot(mod_fulls)

linearHypothesis(mod_full, c("timeto::-7:db=0", 
                             "timeto::-6:db=0", 
                             "timeto::-5:db=0", 
                             "timeto::-4:db=0", 
                             "timeto::-3:db=0", 
                             "timeto::-2:db=0", 
                             "timeto::-1:db=0"))
linearHypothesis(mod_fulls, c("timeto::-7:db=0", 
                              "timeto::-6:db=0", 
                              "timeto::-5:db=0", 
                              "timeto::-4:db=0", 
                              "timeto::-3:db=0", 
                              "timeto::-2:db=0", 
                              "timeto::-1:db=0"))

#EFFECT ON HOURS WORKED

twowayfeweights(df2, "htrab", "indiv", "time", "trb", type = "feTR", summary_measures = T)
twowayfeweights(df2, "hcontrato", "indiv", "time", "trb", type = "feTR", summary_measures = T)



mod_hours = feols(htrab ~ i(timeto, db, ref=+0) + employedr + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                    indiv+ yearq,                             ## FEs
                  cluster = ~indiv,                          ## Clustered SEs
                  data = df2)
mod_hourscontr = feols(hcontrato ~ i(timeto, db, ref=+0) + employedr + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                         indiv+ yearq,                             ## FEs
                       cluster = ~indiv,                          ## Clustered SEs
                       data = df2)


ghours<-iplot(mod_hours)
hourscontr<- iplot(mod_hourscontr)

linearHypothesis(mod_hours, c("timeto::-7:db=0", 
                              "timeto::-6:db=0", 
                              "timeto::-5:db=0", 
                              "timeto::-4:db=0", 
                              "timeto::-3:db=0", 
                              "timeto::-2:db=0", 
                              "timeto::-1:db=0"))

#EFFECT ON HOLDING OTHER JOBS

twowayfeweights(df2, "otro", "indiv", "time", "trb", type = "feTR", summary_measures = T)
twowayfeweights(df2, "otrosum", "indiv", "time", "trb", type = "feTR", summary_measures = T)



mod_otro = feols(otro ~ i(timeto, db, ref=+0) + employedr + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                   indiv+ yearq,                             ## FEs
                 cluster = ~indiv,                          ## Clustered SEs
                 data = df2)
mod_otrosum = feols(otrosum ~ i(timeto, db, ref=+0) + employedr + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                      indiv+ yearq,                             ## FEs
                    cluster = ~indiv,                          ## Clustered SEs
                    data = df2)


gother<-iplot(mod_otro, main="Effect on holding a second job")
gothersum<-iplot(mod_otrosum, main="Effect on holding a second job")


linearHypothesis(mod_otro, c("timeto::-7:db=0", 
                             "timeto::-6:db=0", 
                             "timeto::-5:db=0", 
                             "timeto::-4:db=0", 
                             "timeto::-3:db=0", 
                             "timeto::-2:db=0", 
                             "timeto::-1:db=0"))

#FINAL GRAPH

paleta<- c("#ce6a6c", "#ebada2", "#5fb3b3", "#49919d", "#202e53", "#92b481", "#b0a1c1","#b9c3c7") 

gg4<-
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

cowplot::ggsave2(filename = "gg4.jpg", plot = gg4, width = 7, height= 5)
