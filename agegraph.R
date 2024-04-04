library(tidyverse)
library(cowplot)
theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM")

df <- read.delim2("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Datos/Grafico edad/42381.csv")

df1 <- df %>% 
  mutate(type= Tipo.de.contrato.o.relación.laboral,
         type= case_when(type== "Total" ~ "total",
                         type== "De duración indefinida: Permanente a lo largo del tiempo" ~ "Permanent",
                         type== "De duración indefinida: Discontinuo" ~ "Intermittent Open-Ended",
                         type == "Temporal: Total" ~ "Temporary: total",
                         type == "Temporal: Para obra o servicio determinado" ~ "Temp: Work and Service",
                         type == "Temporal: Eventual por circunstancias de la producción" ~ "Temp: Production circumstances",
                         type== "Temporal: De aprendizaje, formación o práctica" ~ "Temp: Internships",
                         type=="Temporal: Estacional o de temporada" ~ "Temp: Seasonal",
                         type== "Temporal: En periodo de prueba" ~ "Temp: Testing period",
                         type == "Temporal: Cubre la ausencia total o parcial de otro trabajador" ~ "Temp: Substitution", 
                         type == "Temporal: Verbal, no incluido en las opciones anteriores" ~ "Temp: Verbal", 
                         type == "Temporal: Otro tipo" ~"Temp: Other",
                         type == "Temporal: No sabe" ~ "Temp: Does not know"),
         date=Periodo,
         value= as.numeric(gsub("\\.", "", gsub(",", ".", Total))),
         age= Edad,
         young= as.factor(ifelse(age %in% c("De 16 a 19 años", "De 20 a 24 años"), "<25", "25+"))) %>% 
  filter(!is.na(type))  %>% 
  select(-c(Sexo, Tipo.de.contrato.o.relación.laboral, Edad, Periodo, Total)) %>% 
  group_by(date, young, type) %>% 
  summarize(value=sum(value)) %>% 
  group_by(young, date) %>% 
  mutate(total_val= value[type=="total"],
         rate= value/total_val) %>% 
  ungroup() %>% 
  group_by(type, young) %>% 
  mutate(totalp=value[date=="2019T1"], 
         ratep= value*100/totalp)


paletoncia<- c("#ce6a6c", "#ebada2", "#5fb3b3", "#49919d", "#202e53", "#92b481")



#agegraph<- 
  df1 %>% 
  filter(type !="Temporary", type!= "total") %>% 
  ggplot(aes(x=date, y=rate, color= type, linetype=young, group= interaction(young, type)))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept ="2022T1")
  coord_cartesian(ylim=0.2)
  scale_color_manual(values = c("#ce6a6c", "#49919d", "#202e53"))+
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = -90),
        legend.box = "vertical")
  
agegraph2<-  
  df1 %>% 
    filter(type!="total") %>% 
    ggplot(aes(x=date, y= rate, linetype= young, color=young, group= young))+
    geom_point()+
    geom_line()+
    geom_vline(xintercept = "2021T4")+
    scale_color_manual(values = c("#ce6a6c", "#49919d"))+
    theme(axis.text.x = element_text(angle = -90),
          legend.title = element_blank(),
          axis.title = element_blank())+
    facet_wrap(~type, scales="free")
    
ggsave2(plot = agegraph2, filename = "agegraph2.jpg", width = 10, height = 6)
#cowplot::ggsave2(plot = agegraph, filename = "agegraph.jpg", width = 5)

  df1 %>% 
    filter(type !="Temporary", type!= "total") %>% 
    ggplot(aes(x=date, y=ratep, color= type, linetype=young, group= interaction(young, type)))+
    geom_point()+
    geom_line()+
    geom_vline(xintercept ="2022T1")+
    scale_color_manual(values = c("#ce6a6c", "#49919d", "#202e53"))+
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = -90),
          legend.box = "vertical")
