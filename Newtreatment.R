df3<- df2 %>% 
  filter(!is.na(hightemp2)) %>% 
  group_by(hightemp2, yearq) %>% 
  summarise(paro= mean(paro), 
            rate= mean(rate),
            jcompleta= mean(jcompleta),
            hcontrato= mean(hcontrato), 
            htrab= mean(htrab), 
            otro= mean(otro)
  ) %>% 
  pivot_longer(cols = -c(hightemp2, yearq), names_to = "var", values_to = "value") %>% 
  arrange(yearq) %>% 
  group_by(var, hightemp2) %>% 
  mutate(timep= seq(1:15),
         timeto= timep-8) %>% 
  ungroup()

df3 %>% 
  ggplot(aes(x=timeto, y= value, group=as.character(hightemp2), color=as.character(hightemp2))) +
  geom_line(alpha=.85)+
  geom_point(shape=18, alpha=0.85)+
  geom_vline(xintercept = 1)+
  facet_grid(var~., scales = "free_y")+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("#49919d", "#202e53"), labels= c("Not-Treated", "Treated"))+
  theme(axis.title = element_blank(),
        legend.title = element_blank())+
  guides()

#cowplot::ggsave2(filename = "gg1.jpg", plot = gg1, width = 5)
#EFFECTS ON UNEMPLOYMENT

#First we check if we can compute a twowayfe regression without negative weights.

df2<-df2 %>% 
  mutate(treatment2=ifelse(hightemp2==1 & year>2021, 1, 0))

twowayfeweights(df2, "paro", "PROV", "time", "treatment2", type = "feTR", summary_measures = T)


mod_paro = feols(paro ~ i(timeto, hightemp2, ref=+0) + rate + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df2)

gu<-iplot(mod_paro)

linearHypothesis(mod_paro, c("timeto::-7:hightemp2=0", 
                             "timeto::-6:hightemp2=0", 
                             "timeto::-5:hightemp2=0", 
                             "timeto::-4:hightemp2=0", 
                             "timeto::-3:hightemp2=0", 
                             "timeto::-2:hightemp2=0", 
                             "timeto::-1:hightemp2=0"))

#EFFECT ON TEMPORALITY

twowayfeweights(df2, "rate", "PROV", "time", "treatment2", type = "feTR", summary_measures = T)



mod_temp = feols(rate ~ i(timeto, hightemp2, ref=+0) + paro + jcompleta + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df2)


gtemp<-iplot(mod_temp)

linearHypothesis(mod_temp, c("timeto::-7:hightemp2=0", 
                             "timeto::-6:hightemp2=0", 
                             "timeto::-5:hightemp2=0", 
                             "timeto::-4:hightemp2=0", 
                             "timeto::-3:hightemp2=0", 
                             "timeto::-2:hightemp2=0", 
                             "timeto::-1:hightemp2=0"))
#EFFECT ON FULLTIME

twowayfeweights(df2, "jcompleta", "PROV", "time", "treatment2", type = "feTR", summary_measures = T)



mod_full = feols(jcompleta ~ i(timeto, hightemp2, ref=+0) + paro + rate + htrab+otro+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df2)


gfull<-iplot(mod_full)

linearHypothesis(mod_full, c("timeto::-7:hightemp2=0", 
                             "timeto::-6:hightemp2=0", 
                             "timeto::-5:hightemp2=0", 
                             "timeto::-4:hightemp2=0", 
                             "timeto::-3:hightemp2=0", 
                             "timeto::-2:hightemp2=0", 
                             "timeto::-1:hightemp2=0"))

#EFFECT ON HOURS WORKED

twowayfeweights(df2, "htrab", "PROV", "time", "treatment2", type = "feTR", summary_measures = T)



mod_hours = feols(htrab ~ i(timeto, hightemp2, ref=+0) + paro + rate + jcompleta+otro+ edad+male+ncony+educsup|                    ## Other controls
                    PROV+ yearq,                             ## FEs
                  cluster = ~PROV,                          ## Clustered SEs
                  data = df2)


ghours<-iplot(mod_hours)

linearHypothesis(mod_hours, c("timeto::-7:hightemp2=0", 
                              "timeto::-6:hightemp2=0", 
                              "timeto::-5:hightemp2=0", 
                              "timeto::-4:hightemp2=0", 
                              "timeto::-3:hightemp2=0", 
                              "timeto::-2:hightemp2=0", 
                              "timeto::-1:hightemp2=0"))

#EFFECT ON HOLDING OTHER JOBS

twowayfeweights(df2, "otro", "PROV", "time", "treatment2", type = "feTR", summary_measures = T)



mod_otro = feols(otro ~ i(timeto, hightemp2, ref=+0) + paro + rate + jcompleta+htrab+ edad+male+ncony+educsup|                    ## Other controls
                   PROV+ yearq,                             ## FEs
                 cluster = ~PROV,                          ## Clustered SEs
                 data = df2)


gother<-iplot(mod_otro, main="Effect on holding a second job")

linearHypothesis(mod_otro, c("timeto::-7:hightemp2=0", 
                             "timeto::-6:hightemp2=0", 
                             "timeto::-5:hightemp2=0", 
                             "timeto::-4:hightemp2=0", 
                             "timeto::-3:hightemp2=0", 
                             "timeto::-2:hightemp2=0", 
                             "timeto::-1:hightemp2=0"))

paletoncia<- c("#ce6a6c", "#ebada2", "#5fb3b3", "#49919d", "#202e53")

new_labels <- c("Hours worked" = "cat.1", "mod_temp" = "cat.2", "mod_otro" = "cat.3", "mod_full" = "cat.4", "mod_hours"="cat.5")

#gg2<-
ggiplot(list("unemployment"=mod_paro, 
             "Temporary contract"=mod_temp,
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
        main= "Policy treatment2 effects"
)+ 
  theme_minimal()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        strip.background = element_blank())+
  scale_color_manual(values = paletoncia, aesthetics = c("color", "fill"))+
  geom_point(size=2, alpha=.7)+
  geom_line(size=1, alpha=.7)
