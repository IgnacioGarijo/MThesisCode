
library(haven)
library(data.table)
library(tidyverse)
library(lubridate) #to calculate differences between dates
library(zoo) 
library(cowplot) #To save plots
library(fixest) #For feols
library(ggfixest) #To plot the DiD with ggiplot
library(broom)
library(modelsummary)
library(texreg) #for the tables
library(broom)   #Para los mapas
library(sf) #Mapas
library(pROC)
library(TwoWayFEWeights)
library(ggpubr) #To grid the plots


theme_set(theme_minimal())

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")
source("C:/Users/ignac/OneDrive/Documentos/GitHub/MThesisCode/Functions.R")

variable_names<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
labels2<- c("1 month later", "2 months later", "3 months later", "4 months later", "5 months later", "6 months later")
names(labels2)<-1:6
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
           self_emp="Self-employment", unemployed= "Unemployment")





############### FIRST ANALYSIS AGGREGATED ################


load("fromanykind3_panel.Rdata")
dff$salaries<- dff$salaries/100

dftwfe<-create_twfe_dataframe(dff,
                              first_variable = 2, 
                              last_variable = 15, 
                              time_dif = 12,
                              treatment_time = 37,
                              months = 30)

dfsrdd<-create_twfe_dataframe(dff,
                              first_variable = 2, 
                              last_variable = 15,
                              time_dif = 12,
                              treatment_time = 37,
                              months = 30, 
                              rdd = T)
df1<-dfsrdd$df1
df2<-dfsrdd$df2
dfnames<-dfsrdd$dfnames

create_rdd_figures(df1 = df1, df2 = df2, dffnames = dfnames, treatment_time = 37, new_directory = "RDD")



result_models<-create_results(dftwfe = dftwfe, new_directory = "verylongtwfe3", figures = F)



#dir.create("../../../../../../Tables/DID_aggregated/")


for (variable in variable_names) {
  cat(texreg(list(result_models$did_coefs[[paste("mod", variable, 1, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 2, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 3, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 4, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 5, sep = "_")]],
                  result_models$did_coefs[[paste("mod",variable, 6, sep = "_")]]),
             custom.model.names = labels2,
             custom.coef.map = list("ATT"=variable),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE), file = paste0("../../../../../../Tables/DID_aggregated/", variable,".tex")
  )
}



results<-data.frame()

  for (variable in variable_names){
    for(number in 1:6){
      pretrends<-result_models$pre_trends[[paste("mod", variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models$models[[paste("mod", variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(variable= variable, 
               months_later=number,
               pre_trends=pretrends)
      results<-rbind(results, results1)
    }
  }


results<-results %>% 
  filter(rowname!="(Intercept)") %>% 
  transmute(time=as.numeric(gsub(".*:(-?\\d+):.*", "\\1", rowname)),
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends)



plots<-list()
labs<-list(days_worked="N. days worked", salaries="Income", ncontracts="N. contracts",
           open_ended= "Open ended", permanent="Permanent", project_based="Project-based", 
            self_emp="Self-employment", unemployed= "Unemployment")

for (x in variable_names){
  label<-labs[[x]]
  gg<-
    results %>% 
    filter(variable==x, months_later %in% c(1,3,5)) %>%  
    ggplot(aes(x=time, color=as.factor(months_later)))+
    geom_point(aes(y=coefficient), alpha=.9)+
    geom_line(aes(y=coefficient), alpha=.7)+
    geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("#065465", "#008080", "#b67182"), labels= labels2)+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          title = element_text(hjust=.5))+
    ggtitle(label)
  plots[[paste(x)]]<-gg
  ggsave2(gg, file=paste0("../../../../../../Plots/verylongtwfe3/", x,"2.jpeg"), width = 7, height = 5)
  
}


plots<-ggarrange(plotlist = plots, common.legend = T, legend="bottom",
                  ncol=2, nrow=4)
ggsave2(plots, file="../../../../../../Plots/verylongtwfe3/grid.jpeg", width = 7, height = 9)

################# SECOND ANALYSIS, BY SITUATION ###################


load("bysituation_panel.Rdata")

dff$salaries<- dff$salaries/100
result_models<- list()

contracts<-c("permanent", "open-ended", "project-based")


for (g in contracts){
dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                              first_variable = 3, 
                              last_variable = 16, 
                              time_dif = 12,
                              treatment_time = 37,
                              months = 30)
result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE, figures = F )
}

for (contract in contracts){
  for (variable in variable_names){
    for(number in 1:6){
      print(result_models[[contract]]$weights[[paste("mod", contract, variable, number, sep = "_")]])$nrminus
      
    }
  }
  }

results<-data.frame()

for (contract in contracts){
  for (variable in variable_names){
    for(number in 1:6){
pretrends<-result_models[[contract]]$pre_trends[[paste("mod", contract, variable, number, sep = "_")]]$p
results1<- rownames_to_column(as.data.frame(result_models[[contract]]$models[[paste("mod", contract, variable, number, sep = "_")]]$coeftable)) %>% 
  mutate(contract=contract, 
         variable= variable, 
         months_later=number,
         pre_trends=pretrends)
results<-rbind(results, results1)
  }
}
}

results<-results %>% 
  filter(rowname!="(Intercept)") %>% 
  transmute(time=as.numeric(gsub(".*:(-?\\d+):.*", "\\1", rowname)),
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            contract=contract,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends)



plots<-list()
for (x in variable_names){
  label<-labs[[x]]
gg<-
  results %>% 
  filter(variable==x, months_later %in% c(1,3,5)) %>%  
  ggplot(aes(x=time, group=contract, color=contract))+
  geom_point(aes(y=coefficient), alpha=.7)+
  geom_line(aes(y=coefficient), alpha=.8)+
  geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
  facet_wrap(~months_later, labeller = labeller(months_later=labels2))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c("permanent"="#065465", "open-ended"= "#008080","project-based"= "#b67182"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        title = element_text(hjust=.5))+
  ggtitle(label)
plots[[paste(x)]]<-gg
ggsave2(gg, file=paste0("../../../../../../Plots/DID_bycontract/", x,".jpeg"), width = 7, height = 4)

}

plots1<-ggarrange(plotlist = plots[c("unemployed", "salaries", "days_worked" , "ncontracts")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)
plots2<-ggarrange(plotlist = plots[c("project_based", "open_ended", "permanent" , "self_emp")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)
ggsave2(plots1, file="../../../../../../Plots/DID_bycontract/grid1.jpeg", width = 12, height = 10)
ggsave2(plots2, file="../../../../../../Plots/DID_bycontract/grid2.jpeg", width = 12, height = 10)

# dir.create("../../../../../../Tables")
# dir.create("../../../../../../Tables/did_bycontract")

for (contract in contracts) {
  for (variable in variable_names) {
    cat(texreg(list(result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 1, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 2, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 3, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 4, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 5, sep = "_")]],
                    result_models[[contract]]$did_coefs[[paste("mod", contract, variable, 6, sep = "_")]]),
               custom.model.names = labels2,
               custom.coef.map = list("ATT"=variable),
               stars=c("*"=.1, "**"=.05, "***"=.01),
               include.rsquared = FALSE,
               include.adjrs = FALSE,
               include.nobs = FALSE,
               include.rmse = FALSE), file = paste0("../../../../../../Tables/DID_bycontract/", contract,"_", variable,".tex")
    )
  }
}



###############THIRD ANALYSIS BY GENDER ####################


load("bygender_panel.Rdata")
dff$salaries<- dff$salaries/100

dff<-dff %>% 
  mutate(group=ifelse(group==1, "men", "women"))


result_models<- list()

groups<-unique(dff$group)


for (g in groups){
  dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                                first_variable = 3, 
                                last_variable = 16, 
                                time_dif = 12,
                                treatment_time = 37,
                                months = 30)
  result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE, figures = F )
}



results<-data.frame()

for (g in groups){
  for (variable in variable_names){
    for(number in 1:6){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[g]]$models[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(group=g, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
      results<-rbind(results, results1)
    }
  }
}

results<-results %>% 
  filter(rowname!="(Intercept)") %>% 
  transmute(time=as.numeric(gsub(".*:(-?\\d+):.*", "\\1", rowname)),
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            group=group,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends,
            pretest=ifelse(pretrends<=.05, "significant", "Not significant"))



plots<-list()
for (x in variable_names){
  label<-labs[[x]]
  gg<-
    results %>% 
    filter(variable==x, months_later %in% c(1,3,5)) %>%  
    ggplot(aes(x=time, group=group, color=group))+
    geom_point(aes(y=coefficient), alpha=.7)+
    geom_line(aes(y=coefficient), alpha=.9)+
    geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
    facet_wrap(~months_later, labeller = labeller(months_later=labels2))+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("#008080","#b67182",  "grey20"))+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          title = element_text(hjust=.5))+
    ggtitle(label)
  plots[[paste(x)]]<- gg
  ggsave2(gg, file=paste0("../../../../../../Plots/DID_bygender/", x,".jpeg"), width = 7, height = 4)
  
}

plots1<-ggarrange(plotlist = plots[c("unemployed", "salaries", "days_worked" , "ncontracts")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)
plots2<-ggarrange(plotlist = plots[c("project_based", "open_ended", "permanent" , "self_emp")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)

ggsave2(plots1, file="../../../../../../Plots/DID_bygender/grid1.jpeg", width = 12, height = 10)
ggsave2(plots2, file="../../../../../../Plots/DID_bygender/grid2.jpeg", width = 12, height = 10)



for (g in groups) {
  for (variable in variable_names) {
    cat(texreg(list(result_models[[g]]$did_coefs[[paste("mod", g, variable, 1, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 2, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 3, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 4, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 5, sep = "_")]],
                    result_models[[g]]$did_coefs[[paste("mod", g, variable, 6, sep = "_")]]),
               custom.model.names = labels2,
               custom.coef.map = list("ATT"="Days worked"),
               stars=c("*"=.1, "**"=.05, "***"=.01),
               include.rsquared = FALSE,
               include.adjrs = FALSE,
               include.nobs = FALSE,
               include.rmse = FALSE), file = paste0("../../../../../../Tables/did_bygender/", g,"_", variable,".tex")
    )
  }
  
}




################ FOURTH: ANALYSIS BY PROVINCE ##############

 shapefile_provincias <- st_read("provincias/Provincias_ETRS89_30N.shp")

 shapefile_provincias<-as_Spatial(shapefile_provincias)
 
 data_provincias <- tidy(shapefile_provincias) %>% 
   mutate(id=as.numeric(id))
 
 nombres_provincias <- tibble(nombre=shapefile_provincias$Texto,
                              id=as.numeric(shapefile_provincias$Codigo))
 
 data_provincias_mapa <- data_provincias %>% 
   left_join(nombres_provincias, by = "id")
 
 regional_plot2 <- data_provincias_mapa%>%
   mutate(lat_c = ifelse(lat <35e5, lat + 75e4, lat),
          long_c = ifelse(long <(-5e5), (long + 65e4), long))
 
 #Exploring min and max values to know where to draw the line
 regional_plot2%>%
   filter(nombre %in% c("Las Palmas", "Santa Cruz de Tenerife"))%>%
   summarize(a = min(lat_c),
             b = max(lat_c),
             c = min(long_c),
             d = max(long_c))
 
 
 #Creating separate df
 canaries_line <- data.frame(long = c(-354502, 134136, 134136),
                             lat = c(4036484, 4036484, 3882137))
 
 regional_plot2 %>%
   group_by(nombre) %>% 
   mutate(count=1:n(),
          labell=ifelse(count==1,nombre,NA),
          long_label=mean(long_c),
          lat_label=mean(lat_c)) %>%
   ggplot() +
   geom_polygon(aes( x= long_c,
                     y = lat_c,
                     group = group),fill="black",
                color="white",
                linewidth = 0.05) +
   geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
   geom_text(aes(x= long_label,
                 y = lat_label,label=labell), color="white", size=4)+
   theme_void() +
   theme(panel.background = element_rect(linewidth= 1, color = "white", fill = "white")) +
   scale_fill_gradient(low="#d0a5ae",
                       high = "#19547b")+
   theme(legend.title = element_blank())

load("byregion_panel.Rdata")


dff$salaries<- dff$salaries/100


result_models<- list()

groups<-unique(dff$group)


for (g in groups){
  tryCatch({
  dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                                first_variable = 3, 
                                last_variable = 16, 
                                time_dif = 12,
                                treatment_time = 37,
                                months = 30)
  result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE, figures = F )
  }, error = function(e) {
    cat("Error occurred for group:", g, "\n")
  })
  }



results<-data.frame()

for (g in groups){
  for (variable in variable_names){
    for(number in c(1,3,5)){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[g]]$did_coefs[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(group=g, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
      results<-rbind(results, results1)
    }
  }
}


results<-results %>% 
  filter(rowname=="ATT") %>% 
  transmute(rowname=rowname,
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            group=group,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            id=as.numeric(group),
            stars=ifelse(pvalue<=0.1, "significant", "Not significant"),
            pretrends=pre_trends,
            pretest= ifelse(pretrends<=0.05, "significant", "Not significant"),
            relevant= ifelse(stars=="significant" & pretest =="Not significant", "relevant", "non-relevant"))


for (x in variable_names){
results1<-results %>% 
  filter(variable==x)

map_df<-left_join(regional_plot2, results1, by="id") 

gg<-map_df %>%
  ggplot() +
  geom_polygon(aes( x= long_c, 
                    y = lat_c, 
                    group = group.x,
                    fill=coefficient,
                    color=as.factor(relevant), 
                    alpha=as.factor(relevant)),
               linewidth = 0.05 ) +
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
  theme_void() +
  theme(panel.background = element_rect(linewidth= 1, color = "white", fill = "white")) +
  scale_fill_gradient(low="#9bc9be", 
                      high = "#19547b")+
  scale_alpha_manual(values = c("non-relevant"=0.5, "relevant"=1))+
  guides(color="none",
         alpha="none")+
  theme(legend.title=element_blank())+
  scale_color_manual(values = c( "non-relevant"="white", "relevant"="black"))+
  facet_wrap(~months_later, labeller = labeller(months_later=labels2), ncol = 3 )


ggsave2(gg, file=paste0("../../../../../../Plots/DID_byprovince/", x,".jpeg"), width = 18, height = 6)

}





################ FIFTH: BY OCCUPATION ###################

load("byoccupation_panel.Rdata")
dff$salaries<- dff$salaries/100


result_models<- list()

dff<-dff %>% filter(!is.na(group), group!=13)
groups<-unique(dff$group)


for (g in groups){
  dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                                first_variable = 3, 
                                last_variable = 16, 
                                time_dif = 12,
                                treatment_time = 37,
                                months = 30)
  result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE, figures = F )
}



results<-data.frame()

for (g in groups){
  for (variable in variable_names){
    for(number in c(1,3,5)){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[g]]$did_coefs[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(group=g, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
      results<-rbind(results, results1)
    }
  }
}


results<-results %>% 
  filter(rowname=="ATT") %>% 
  transmute(rowname=rowname,
            coefficient=  Estimate,
            pvalue= `Pr(>|t|)`,
            group=group,
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
            ),
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends,
            stars=ifelse(pvalue<=0.1, "significant", "Not significant"),
            pretest=ifelse(pretrends<=.05, "significant", "Not significant"))


results$group2 <- factor(results$group2, levels = rev(c("Engineers/Top Management", "Technical Engineers/Experts", "Managers",
                                                    "UntitledAssistants", "Administrative Officers", "Subordinates",
                                                    "Administrative Assistants", "1st 2nd Grade Officers","3rd Grade Officers/Specialists",
                                                    "Unqualified +18","<18 years old")))

plots<-list()

for (x in variable_names){
  results1<-results %>% 
    filter(variable==x)
  label<-labs[[x]]
  if(x %in% c("unemployed", "days_worked", "project_based", "permanent")) {
gg<-
  results1 %>% 
  ggplot(aes(y=group2, x=coefficient))+
  geom_point(aes(color=pretest), fill="#008080",size=3,shape=21, alpha=.7)+
  geom_errorbarh(aes(xmax=plus95, xmin=minus95), color="#008080", alpha=.5)+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = "none")+
  geom_vline(xintercept = 0)+
  scale_color_manual(values = c("Not significant"="#008080", "significant"="red"))+
  guides(color="none")+
  ggtitle(label)+
  facet_wrap(~months_later, labeller = labeller(months_later=labels2) )
  } else {
    gg<-
      results1 %>% 
      ggplot(aes(y=group2, x=coefficient))+
      geom_point(aes(color=pretest), fill="#008080",size=3,shape=21, alpha=.7)+
      geom_errorbarh(aes(xmax=plus95, xmin=minus95), color="#008080", alpha=.5)+
      theme_bw()+
      theme(axis.title = element_blank(),
            axis.text.x = element_text(angle = 90),
            legend.position = "none",
            axis.text.y = element_blank())+
      geom_vline(xintercept = 0)+
      scale_color_manual(values = c("Not significant"="#008080", "significant"="red"))+
      guides(color="none")+
      ggtitle(label)+
      facet_wrap(~months_later, labeller = labeller(months_later=labels2) )
}

plots[[paste(x)]]<- gg
  
ggsave2(gg, file=paste0("../../../../../../Plots/DID_byoccupation/", x,".jpeg"), width = 10, height = 6)

}
 
plots1<-ggarrange(plotlist = plots[c("unemployed", "salaries", "days_worked" , "ncontracts")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2,widths = c(1.3,1))

plots2<-ggarrange(plotlist = plots[c("project_based", "open_ended", "permanent" , "self_emp")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2,widths = c(1.3,1))

ggsave2(plots1, file="../../../../../../Plots/DID_byoccupation/grid1.jpeg", width = 10, height = 10)
ggsave2(plots2, file="../../../../../../Plots/DID_byoccupation/grid2.jpeg", width = 10, height = 10)



############### SIXTH: BY SECTOR #######################

#  df$sector <- ifelse(df$sector %in% c(11, 12,13, 14, 15, 16,17,21, 22, 23, 24, 31, 32, 51, 52,
#                                          61, 62, 71,72, 81, 89, 91, 99),
#                                  paste0("0", df$sector),
#                                  df$sector) 
#  
# 
# df$sector<-substr(df$sector, 1,2)
# 
# create_cohort(df, aggregation="sector", name="bysector")
# 
# 
# load("bysector_panel.Rdata")
# 
# 
# 
# result_models<- list()
# 
# dff<-dff %>% 
#   filter(!is.na(group)) %>% 
#   ungroup() %>% 
#   group_by(group) %>% 
#   mutate(nn=n()) %>% 
#   ungroup() %>% 
#   filter(nn==max(nn))
# 
# groups<-unique(dff$group)
# 
# 
# for (g in groups) {
#   tryCatch({
#     dftwfe <- create_twfe_dataframe(dff[dff$group == paste0(g), ],
#                                     first_variable = 3,
#                                     last_variable = 16,
#                                     time_dif = 12,
#                                     treatment_time = 37,
#                                     months = 30)
#     result_models[[paste0(g)]] <- create_results(dftwfe, paste0("DID_disaggregated", g), disaggregation = TRUE, figures = F)
#   }, error = function(e) {
#     cat("Error occurred for group:", g, "\n")
#   })
# }
# 
# 
# results<-data.frame()
# 
# for (g in groups){
#   for (variable in variable_names){
#     for(number in c(1,3,5)){
#       pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
#       results1<- rownames_to_column(as.data.frame(result_models[[g]]$did_coefs[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
#         mutate(group=g, 
#                variable= variable, 
#                months_later=number,
#                pre_trends=pretrends)
#       results<-rbind(results, results1)
#     }
#   }
# }
# 
# 
# results<-results %>% 
#   filter(rowname=="ATT") %>% 
#   transmute(rowname=rowname,
#             coefficient= ifelse(grepl("^salar", variable), Estimate/100, Estimate),
#             pvalue= `Pr(>|t|)`,
#             group=group,
#             group2 = case_when(
#               group == "01" ~ "Crop and animal production",
#               group == "02" ~ "Forestry and logging",
#               group == "03" ~ "Fishing and aquaculture",
#               group == "05" ~ "Mining of coal and lignite",
#               group == "06" ~ "Extraction of crude petroleum and natural gas",
#               group == "07" ~ "Mining of metal ores",
#               group == "08" ~ "Other mining and quarrying",
#               group == "09" ~ "Mining support service activities",
#               group == "10" ~ "M of Food Products",
#               group == "11" ~ "M of Beverages",
#               group == "12" ~ "M of Tobacco Products",
#               group == "13" ~ "M of Textiles",
#               group == "14" ~ "M of Wearing Apparel",
#               group == "15" ~ "M of Leather",
#               group == "16" ~ "M of Wood Products",
#               group == "17" ~ "M of Paper & Paper Products",
#               group == "18" ~ "Printing & Reproduction of Recorded Media",
#               group == "19" ~ "M Refined Petroleum Products",
#               group == "20" ~ "M of Chemical Products",
#               group == "21" ~ "M of Basic Pharmaceutical Products",
#               group == "22" ~ "M of Rubber and Plastic Products",
#               group == "23" ~ "M of Other Non-Metallic Mineral Products",
#               group == "24" ~ "M of Basic Metals",
#               group == "25" ~ "M of Metal Products",
#               group == "26" ~ "M of Electronic Products",
#               group == "27" ~ "M of Electrical Equipment",
#               group == "28" ~ "M of Machinery & Equipment",
#               group == "29" ~ "M of Motor Vehicles",
#               group == "30" ~ "M of Other Transport Equipment",
#               group == "31" ~ "M of Furniture",
#               group == "32" ~ "Other manufacturing",
#               group == "33" ~ "Machinery Repair & Installation",
#               group == "35" ~ "Energy Supply & Conditioning",
#               group == "36" ~ "Water Management",
#               group == "37" ~ "Sewerage Services",
#               group == "38" ~ "Waste Management & Remediation",
#               group == "39" ~ "Waste Remediation & Management",
#               group == "41" ~ "Building Construction",
#               group == "42" ~ "Civil Engineering",
#               group == "43" ~ "Specialized Construction",
#               group == "45" ~ "Motor Vehicle Trade & Repair",
#               group == "46" ~ "Wholesale Trade",
#               group == "47" ~ "Retail Trade",
#               group == "49" ~ "Land & Pipeline Transport",
#               group == "50" ~ "Water Transport",
#               group == "51" ~ "Air Transport",
#               group == "52" ~ "Transportation Support Services",
#               group == "53" ~ "Postal Services",
#               group == "55" ~ "Accommodation Services",
#               group == "56" ~ "Food & Beverage Services",
#               group == "58" ~ "Publishing & Media Production",
#               group == "59" ~ "Media Production & Distribution",
#               group == "60" ~ "Broadcasting & Programming",
#               group == "61" ~ "Telecommunications",
#               group == "62" ~ "Computer Services & Consultancy",
#               group == "63" ~ "Information Services",
#               group == "64" ~ "Financial Services",
#               group == "65" ~ "Insurance & Pension Funding",
#               group == "66" ~ "Auxiliary Financial Services",
#               group == "68" ~ "Real Estate Activities",
#               group == "69" ~ "Legal & Accounting Services",
#               group == "70" ~ "Management Consultancy",
#               group == "71" ~ "Architectural & Engineering Services",
#               group == "72" ~ "Research & Development",
#               group == "73" ~ "Advertising & Market Research",
#               group == "74" ~ "Professional & Technical Services",
#               group == "75" ~ "Veterinary Services",
#               group == "77" ~ "Rental & Leasing Activities",
#               group == "78" ~ "Employment Services",
#               group == "79" ~ "Travel & Reservation Services",
#               group == "80" ~ "Security & Investigation",
#               group == "81" ~ "Building Services & Landscaping",
#               group == "82" ~ "Business Support Services",
#               group == "84" ~ "Public Administration & Defense",
#               group == "85" ~ "Education",
#               group == "86" ~ "Healthcare Services",
#               group == "87" ~ "Residential Care Services",
#               group == "88" ~ "Social Work Services",
#               group == "90" ~ "Arts & Entertainment",
#               group == "91" ~ "Cultural Activities",
#               group == "92" ~ "Gambling & Betting",
#               group == "93" ~ "Sports & Recreation",
#               group == "94" ~ "Membership Organization Activities",
#               group == "95" ~ "Personal Goods Repair",
#               group == "96" ~ "Personal Services",
#               group == "97" ~ "Domestic Employment",
#               group == "98" ~ "Household Production",
#               group == "99" ~ "Extraterritorial Organizations",
#               TRUE ~ NA_character_
#             ),
#             variable=variable,
#             months_later=months_later,
#             months_laterst=paste0(months_later," months later"),
#             se= `Std. Error`,
#             plus95=ifelse(variable=="salaries",(Estimate+se*1.96)/100,Estimate+se*1.96),
#             minus95=ifelse(variable=="salaries",(Estimate-se*1.96)/100,Estimate-se*1.96),
#             stars=ifelse(pvalue<=0.1, "significant", "Not significant"),
#             pretrends=pre_trends,
#             pretest=ifelse(pretrends<=.05, "significant", "Not significant"))
# 
# 
# results<-results %>% 
#   mutate(group3=as.numeric(group),
#          group4=group3,
#          group3=case_when(group3 %in% c(1:3) ~ "RURAL",
#                           group3 %in% c(5:9) ~ "MINING",
#                           group3 %in% c(10:33) ~ "MANUFACTURING",
#                           group3 ==35 ~ "ENERGY",
#                           group3 %in% c(36:39)~"WATER",
#                           group3 %in% c(41:43) ~ "CONSTRUCTION",
#                           group3 %in% c(45:47) ~ "WHOLESALE",
#                           group3 %in% c(49:53) ~ "TRANSPORTATION",
#                           group3 %in% c(55,56) ~ "ACCOMODATION",
#                           group3 %in% c(58:63) ~ "COMMUNICATION",
#                           group3 %in% c(64:66)~ "FINANCE", 
#                           group3 == 68 ~ "REAL ESTATE",
#                           group3 %in% c(69:75)~ "PROFESSIONAL",
#                           group3 %in% c(77:82) ~ "ADMINISTRATIVE",
#                           group3==84 ~ "PUBLIC ADMIN",
#                           group3==85 ~ "EDUCATION", 
#                           group3 %in% c(86:88) ~ "HEALTH",
#                           group3 %in% c(90:93) ~ "ARTS",
#                           group3 %in% c(94,95) ~ "OTHER",
#                           group3 %in% c(96:98) ~ "HOUSEHOLDS",
#                           group3 == 99 ~ "EXTRATERRITORIAL",
#                           TRUE ~ " "
#                           ),
#          relevant=ifelse(pretest=="Not significant" & stars=="significant","relevant", "not relevant")) %>% 
#   group_by(group3) %>% 
#   mutate(names= ifelse(group4==min(group4),group3, " ")) %>% 
#   ungroup()
# 
# 
# 
# 
# 
# 
# for (x in variable_names){
#   results1<-results %>% 
#     filter(variable==x)
#   results1$interaction_order <- interaction(results1$names, results1$group2, drop = TRUE)
#   results1$interaction_order <- fct_reorder(results1$interaction_order, -results1$group4)
#   color_gradient <- colorRampPalette(c("#384e78", "#02CCFE"))
#   
#   results1$group3 <- fct_reorder(results1$group3, results1$group4)
#   
#   color_df <- data.frame(group3 = unique_group3, color = color_gradient(length(unique(results1$group3))))
#   
#   results1 <- merge(results1 %>% arrange(group4), color_df, by = "group3", all.x = TRUE)
#   
#   a<-results1$color
#   gg<-
#     results1 %>% 
#     ggplot(aes(y=interaction_order, x=coefficient))+
#     geom_point(aes(color=pretest), fill="#008080", alpha=.7, shape=21)+
#     geom_errorbarh(aes(xmax=plus95, xmin=minus95), color="#008080", alpha=.5)+
#       scale_color_manual(values = c("#008080", "red"))+
#     theme_bw()+
#     theme(axis.title = element_blank(),
#           axis.text.x = element_text(angle = 90),
#           axis.text.y=element_text(color=a))+
#     geom_vline(xintercept = 0)+
#     facet_wrap(~months_later, labeller = labeller(months_later=labels2) )
#   
#   ggsave2(gg, file=paste0("../../../../../../Plots/DID_bysector/", x,".jpeg"), width = 10, height = 6)
#   
# }



############### SIXTH: BY SECTOR2 #######################

load("bysector2_panel.Rdata")
dff$salaries<- dff$salaries/100



result_models<- list()

dff<-dff %>% 
  filter(!is.na(group)) %>% 
  ungroup() %>% 
  group_by(group) %>% 
  mutate(nn=n()) %>% 
  ungroup() %>% 
  filter(nn==max(nn))

groups<-unique(dff$group)


for (g in groups) {
  tryCatch({
    dftwfe <- create_twfe_dataframe(dff[dff$group == paste0(g), ],
                                    first_variable = 3,
                                    last_variable = 16,
                                    time_dif = 12,
                                    treatment_time = 37,
                                    months = 30)
    result_models[[paste0(g)]] <- create_results(dftwfe, paste0("DID_disaggregated", g), disaggregation = TRUE, figures = F)
  }, error = function(e) {
    cat("Error occurred for group:", g, "\n")
  })
}


results<-data.frame()

for (g in groups){
  for (variable in variable_names){
    for(number in c(1,3,5)){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[g]]$did_coefs[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(group=g, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
      results<-rbind(results, results1)
    }
  }
}


results<-results %>% 
  filter(rowname=="ATT") %>% 
  transmute(rowname=rowname,
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            group=group,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends,
            stars=ifelse(pvalue<=0.1, "significant", "Not significant"),
            pretest=ifelse(pretrends<=.05, "significant", "Not significant"))

plots<-list()


results$group <- factor(results$group, levels = rev(c(
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


for (x in variable_names){
  results1<-results %>% 
    filter(variable==x)
  label<-labs[[x]]
  if(x %in% c("unemployed", "days_worked", "project_based", "permanent")) {
   
  gg<-
    results1 %>% 
    ggplot(aes(y=group, x=coefficient))+
    geom_point(aes(color=pretest), fill="#008080",size=3,shape=21, alpha=.7)+
    geom_errorbarh(aes(xmax=plus95, xmin=minus95), color="#008080", alpha=.5)+
    theme_bw()+
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90),
          legend.position = "none")+
    geom_vline(xintercept = 0)+
    scale_color_manual(values = c("Not significant"="#008080", "significant"="red"))+
    guides(color="none")+
    ggtitle(label)+
    facet_wrap(~months_later, labeller = labeller(months_later=labels2) )
  
  } else {
    gg<-
      results1 %>% 
      ggplot(aes(y=group, x=coefficient))+
      geom_point(aes(color=pretest), fill="#008080",size=3,shape=21, alpha=.7)+
      geom_errorbarh(aes(xmax=plus95, xmin=minus95), color="#008080", alpha=.5)+
      theme_bw()+
      theme(axis.title = element_blank(),
            axis.text.x = element_text(angle = 90),
            legend.position = "none",
            axis.text.y = element_blank())+
      geom_vline(xintercept = 0)+
      scale_color_manual(values = c("Not significant"="#008080", "significant"="red"))+
      guides(color="none")+
      ggtitle(label)+
      facet_wrap(~months_later, labeller = labeller(months_later=labels2) )
    
  }
  
  
  plots[[paste(x)]]<-gg
  
  ggsave2(gg, file=paste0("../../../../../../Plots/DID_bysector2/", x,".jpeg"), width = 10, height = 6)
  
}

plots1<-ggarrange(plotlist = plots[c("unemployed", "salaries", "days_worked" , "ncontracts")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2,widths = c(2,1))

plots2<-ggarrange(plotlist = plots[c("project_based", "open_ended", "permanent" , "self_emp")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2,widths = c(2,1))

ggsave2(plots1, file="../../../../../../Plots/DID_bysector2/grid1.jpeg", width = 12, height = 10)
ggsave2(plots2, file="../../../../../../Plots/DID_bysector2/grid2.jpeg", width = 12, height = 10)




####################SEVENTH: BY AGE GROUP ##################

load("byagegroup_panel.Rdata")
dff$salaries<- dff$salaries/100

result_models<- list()

groups<-unique(dff$group)


for (g in groups){
  dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                                first_variable = 3, 
                                last_variable = 16, 
                                time_dif = 12,
                                treatment_time = 37,
                                months = 30)
  result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE, figures = F )
}




results<-data.frame()

for (g in groups){
  for (variable in variable_names){
    for(number in 1:6){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[g]]$models[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(group=g, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
      results<-rbind(results, results1)
    }
  }
}

results<-results %>% 
  filter(rowname!="(Intercept)") %>% 
  transmute(time=as.numeric(gsub(".*:(-?\\d+):.*", "\\1", rowname)),
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            group=group,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends,
            pretest=ifelse(pretrends<=.05, "significant", "Not significant"))



plots<-list()

for (x in variable_names){
  label<-labs[[x]]
 gg<-
    results %>% 
    filter(variable==x, months_later %in% c(1,3,5)) %>%  
    ggplot(aes(x=time, group=group, color=group))+
    geom_point(aes(y=coefficient), alpha=.7)+
    geom_line(aes(y=coefficient), alpha=.9)+
    geom_errorbar(aes(ymin=minus95, ymax=plus95), alpha=.3, linetype="dashed")+
    facet_wrap(~months_later, labeller = labeller(months_later=labels2))+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("grey60", "#a9c7ee", "#51aff7","#006884", "black"))+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          title = element_text(hjust=.5))+
   ggtitle(label)
 plots[[paste(x)]]<-gg
  ggsave2(gg, file=paste0("../../../../../../Plots/DID_byagegroup/", x,".jpeg"), width = 7, height = 6)
  
}

plots1<-ggarrange(plotlist = plots[c("unemployed", "salaries", "days_worked" , "ncontracts")], common.legend = T, legend="bottom",
                 ncol=2, nrow=2)
plots2<-ggarrange(plotlist = plots[c("project_based", "open_ended", "permanent" , "self_emp")], common.legend = T, legend="bottom",
                  ncol=2, nrow=2)

ggsave2(plots1, file="../../../../../../Plots/DID_byagegroup/grid1.jpeg", width = 12, height = 10)
ggsave2(plots2, file="../../../../../../Plots/DID_byagegroup/grid2.jpeg", width = 12, height = 10)


######### JUST DID COEFS ################


results<-data.frame()

for (g in groups){
  for (variable in variable_names){
    for(number in c(1:6)){
      pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
      results1<- rownames_to_column(as.data.frame(result_models[[g]]$did_coefs[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
        mutate(group=g, 
               variable= variable, 
               months_later=number,
               pre_trends=pretrends)
      results<-rbind(results, results1)
    }
  }
}


results<-results %>% 
  filter(rowname=="ATT") %>% 
  transmute(rowname=rowname,
            coefficient= Estimate,
            pvalue= `Pr(>|t|)`,
            group=group,
            variable=variable,
            months_later=months_later,
            se= `Std. Error`,
            plus95=coefficient+se*1.96,
            minus95=coefficient-se*1.96,
            pretrends=pre_trends,
            stars=ifelse(pvalue<=0.1, "significant", "Not significant"),
            pretest=ifelse(pretrends<=.05, "significant", "Not significant"))


plots<-list()

labs<-c("N. days worked", "Income", "N. contracts","Open ended","Permanent","Project-based", 
           "Self-employment", "Unemployment")
names(labs)<-c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", 
               "self_emp", "unemployed")
results$variable <- factor(results$variable, levels = c("project_based", "open_ended", "permanent", "unemployed", "self_emp", "salaries", "ncontracts", "days_worked"))

    gg<-
      results %>% 
      ggplot(aes(x=as.factor(months_later), y=coefficient))+
      geom_col(aes(fill=group), position="dodge2",size=3, alpha=.7)+
      geom_errorbar(aes(ymax=plus95, ymin=minus95, group=group), position="dodge2", color="#008080", alpha=.5)+
      theme_bw()+
      theme(axis.title = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())+
      geom_hline(yintercept = 0)+
      scale_fill_manual(values = c("#8eb1c2", "#51aff7","#006884",  "#446879", "#044766"))+
      facet_wrap(~variable, labeller = labeller(variable=labs), ncol = 2, scales = "free" )
  
  ggsave2(gg, file=paste0("../../../../../../Plots/DID_byagegroup/grid.jpeg"), width = 8, height = 10)
  


########## BY QUANTILE ###############
  
  
  load("byquantile_panel.Rdata")
  dff$salaries<- dff$salaries/100
  
  result_models<- list()
  
  groups<-unique(dff$group)
  
  
  for (g in groups){
    dftwfe<-create_twfe_dataframe(dff[dff$group==g,],
                                  first_variable = 3, 
                                  last_variable = 16, 
                                  time_dif = 12,
                                  treatment_time = 37,
                                  months = 30)
    result_models[[g]]<-create_results(dftwfe, paste0("DID_disaggregated", g),disaggregation = TRUE, figures = F )
  }
  

  
  results<-data.frame()
  
  for (g in groups){
    for (variable in variable_names){
      for(number in c(1:6)){
        pretrends<-result_models[[g]]$pre_trends[[paste("mod", g, variable, number, sep = "_")]]$p
        results1<- rownames_to_column(as.data.frame(result_models[[g]]$did_coefs[[paste("mod", g, variable, number, sep = "_")]]$coeftable)) %>% 
          mutate(group=g, 
                 variable= variable, 
                 months_later=number,
                 pre_trends=pretrends)
        results<-rbind(results, results1)
      }
    }
  }
  
  
  results<-results %>% 
    filter(rowname=="ATT") %>% 
    transmute(rowname=rowname,
              coefficient= Estimate,
              pvalue= `Pr(>|t|)`,
              group=group,
              variable=variable,
              months_later=months_later,
              se= `Std. Error`,
              plus95=coefficient+se*1.96,
              minus95=coefficient-se*1.96,
              pretrends=pre_trends,
              stars=ifelse(pvalue<=0.1, "significant", "Not significant"),
              pretest=ifelse(pretrends<=.05, "significant", "Not significant"))
  
  
  plots<-list()
  
  labs<-c("N. days worked", "Income", "N. contracts","Open ended","Permanent","Project-based", 
          "Self-employment", "Unemployment")
  names(labs)<-c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", 
                 "self_emp", "unemployed")
  results$variable <- factor(results$variable, levels = c("project_based", "open_ended", "permanent", "unemployed", "self_emp", "salaries", "ncontracts", "days_worked"))
  
  
  cc<-scales::seq_gradient_pal("#0e387a","#9bc9be",  "Lab")(seq(0,1,length.out=(length(unique(df1$quantile)
  ))
  )
  )
  
  gg<-
    results %>% 
    ggplot(aes(x=as.factor(months_later), y=coefficient))+
    geom_col(aes(fill=as.factor(group), color=pretest), position="dodge2", alpha=.7)+
    geom_errorbar(aes(ymax=plus95, ymin=minus95, group=group), position="dodge2", color="#008080", alpha=.5)+
    theme_bw()+
    theme(axis.title = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank())+
    geom_hline(yintercept = 0)+
    scale_fill_manual(values = cc)+
     scale_color_manual(values = c("Significant"="red", "Not significant"= "white"))+
      guides(color="none")+
    facet_wrap(~variable, labeller = labeller(variable=labs), ncol = 2, scales = "free" )
  
  ggsave2(gg, file=paste0("../../../../../../Plots/DID_byquantile/grid.jpeg"), width = 8, height = 9)
  
  



########### PREANALYSIS: LOGIT #####

df1<- df[df$yearmonth==202112,]


##Gender

df1$sex<- ifelse(df1$sex==1, "man", "woman")

#Province

df1$person_muni_latest <- replace_province(df1$person_muni_latest)

df1$province<-as.numeric(df1$person_muni_latest)

df1 <- df1 %>%
  mutate(province = as.factor(case_when(
    province == 2 ~ "Albacete",
    province == 3 ~ "Alicante",
    province == 4 ~ "Almería",
    province == 1 ~ "Álava",
    province == 33 ~ "Asturias",
    province == 5 ~ "Ávila",
    province == 6 ~ "Badajoz",
    province == 7 ~ "Balears, Illes",
    province == 8 ~ "Barcelona",
    province == 48 ~ "Bizkaia",
    province == 9 ~ "Burgos",
    province == 10 ~ "Cáceres",
    province == 11 ~ "Cádiz",
    province == 39 ~ "Cantabria",
    province == 12 ~ "Castellón",
    province == 13 ~ "Ciudad Real",
    province == 14 ~ "Córdoba",
    province == 15 ~ "Coruña, A",
    province == 16 ~ "Cuenca",
    province == 20 ~ "Gipuzkoa",
    province == 17 ~ "Girona",
    province == 18 ~ "Granada",
    province == 19 ~ "Guadalajara",
    province == 21 ~ "Huelva",
    province == 22 ~ "Huesca",
    province == 23 ~ "Jaén",
    province == 24 ~ "León",
    province == 25 ~ "Lleida",
    province == 27 ~ "Lugo",
    province == 28 ~ "Madrid",
    province == 29 ~ "Málaga",
    province == 30 ~ "Murcia",
    province == 31 ~ "Navarra",
    province == 32 ~ "Ourense",
    province == 34 ~ "Palencia",
    province == 35 ~ "Palmas, Las",
    province == 36 ~ "Pontevedra",
    province == 26 ~ "Rioja, La",
    province == 37 ~ "Salamanca",
    province == 38 ~ "Santa Cruz de Tenerife",
    province == 40 ~ "Segovia",
    province == 41 ~ "Sevilla",
    province == 42 ~ "Soria",
    province == 43 ~ "Tarragona",
    province == 44 ~ "Teruel",
    province == 45 ~ "Toledo",
    province == 46 ~ "Valencia",
    province == 47 ~ "Valladolid",
    province == 49 ~ "Zamora",
    province == 50 ~ "Zaragoza",
    province == 51 ~ "Ceuta",
    province == 52 ~ "Melilla",
    TRUE ~ "Desconocida"  # Opción predeterminada en caso de que no haya ninguna coincidencia
  )))

## Occupation

df1<-df1 %>% 
  mutate(occupation= case_when(
  occupation == 1 ~ "Engineers/Top Management",
  occupation == 2 ~ "Technical Engineers/Experts",
  occupation == 3 ~ "Managers",
  occupation == 4 ~ "UntitledAssistants",
  occupation == 5 ~ "Administrative Officers",
  occupation == 6 ~ "Subordinates",
  occupation == 7 ~ "Administrative Assistants",
  occupation == 8 ~ "1st 2nd Grade Officers",
  occupation == 9 ~ "3rd Grade Officers/Specialists",
  occupation == 10 ~ "Unqualified +18",
  occupation ==11 ~ "<18 years old",
  TRUE ~ NA_character_
))

##sectors 

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


## age group

df1$age<-df1$yearmonth%/%100- df1$birth_date %/% 100

df1[, age_group:= case_when(age<25 ~ "<25",
                           age %in% c(25:34) ~ "25-34",
                           age %in% c(35:44) ~ "35-44",
                           age %in% c(45:54) ~ "45-54",
                           age %in% c(55:64) ~ "55-64",
                           age > 64 ~ "65+")]



df1$project_based <- ifelse(df1$contr_type == "project-based", 1,0)
df1$permanent <- ifelse(df1$contr_type == "permanent", 1, 0)
df1$open_ended <- ifelse(df1$contr_type == "open-ended", 1, 0)


df1<-df1 %>% 
  ungroup() %>% 
  mutate(project_based= as.factor(ifelse(is.na(project_based), 0, project_based)),
         permanent= as.factor(ifelse(is.na(permanent), 0, permanent)),
         open_ended=as.factor(ifelse(is.na(open_ended), 0, open_ended)))



contracts <- c("permanent", "open_ended", "project_based")
variable <- paste("sex", "province", "occupation", "sector2", "age_group", sep = "+")

# Initialize a list to store the fitted models
result_models <- list()

for (contract in contracts) {
    formula_str <- paste(contract, "~", variable)
    formula <- as.formula(formula_str)
    
    model <- glm(data = df1, formula = formula, family = binomial)
    
    result_models[[paste(contract)]]$model <- model
    result_models[[paste(contract)]]$AIC<- model$aic
}


plots<- list()
rocobjs<-list("permanent", "open_ended", "project_based")

df2 <-df1%>% 
  filter(!is.na(sex), !is.na(province), !is.na(occupation), !is.na(sector2), !is.na(age_group))

for (contract in contracts){
predicted_probs <- predict(result_models[[contract]]$model, type = "response")

rocobjs[[contract]] <- roc(df2[[contract]], predicted_probs)
}


gg<-ggroc(list("permanent (0.8197)"=rocobjs$permanent, 
           "open_ended (0.8514)" =rocobjs$open_ended, 
           "Project based (0.8095)"=rocobjs$project_based),
      linewidth=1,
      alpha= .8,
      linetype="dashed")+
  scale_color_manual(values = c("#065465", "#008080", "#b67182"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  geom_abline(intercept = 1, slope = 1, color="grey70")+
  ylab("True positive rate")+
  xlab("False positive rate")
  
  
ggsave2(gg, file=paste0("../../../../../../Plots/ROC.jpeg"), width=8, height = 7)

#dir.create("../../../../../../Tables/did_bygender")

# for (g in groups) {
#   for (variable in variable_names) {
#     cat(texreg(list(result_models[[g]]$did_coefs[[paste("mod", g, variable, 1, sep = "_")]],
#                     result_models[[g]]$did_coefs[[paste("mod", g, variable, 2, sep = "_")]],
#                     result_models[[g]]$did_coefs[[paste("mod", g, variable, 3, sep = "_")]],
#                     result_models[[g]]$did_coefs[[paste("mod", g, variable, 4, sep = "_")]],
#                     result_models[[g]]$did_coefs[[paste("mod", g, variable, 5, sep = "_")]],
#                     result_models[[g]]$did_coefs[[paste("mod", g, variable, 6, sep = "_")]]),
#                custom.model.names = labels2,
#                custom.coef.map = list("ATT"="Days worked"),
#                stars=c("*"=.1, "**"=.05, "***"=.01),
#                include.rsquared = FALSE,
#                include.adjrs = FALSE,
#                include.nobs = FALSE,
#                include.rmse = FALSE), file = paste0("../../../../../../Tables/did_byagegroup/", g,"_", variable,".tex")
#     )
#   }
#   
# }



# 
# load("anykind_cohort_panel.Rdata")
# dffnames<-names(dff[,2:15])
# 
# dff1<-mutate(dff[dff$cohort %in%c(4:24),], time2=time+12)
# dff2<- mutate(dff[dff$cohort >15,], time2=time)
# 
# for (x in dffnames){
#   for (i in 1:6) {
#     
# cutoff<-25-i #For the doughnut RDD
# 
#     dff1<-dff1 %>% 
#       arrange(cohort, -time) %>% 
#       group_by(cohort) %>% 
#       mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:24),NA, lag(get(x), i)) )
#     
#     dff2<-dff2 %>% 
#       arrange(cohort, -time) %>% 
#       group_by(cohort) %>% 
#       mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:24),NA, lag(get(x), i)) )
#   }
# }
# 
# 
# 
# dff1<-dff1 %>% filter(cohort==time)
# dff2<-dff2 %>% filter(cohort==time)
# 
# 
# for (x in dffnames){
#   dffrdd1<-dff1 %>% 
#     select(cohort, time, time2, starts_with(x)) %>% 
#     select(-x) %>% 
#     pivot_longer(cols = starts_with(x)) %>% 
#     mutate(after=ifelse(time2>24, "after", "before")) %>% ############treatment time -1
#     pivot_wider(names_from = after, values_from = value) %>% 
#     mutate(treatment= "Placebo")
#     
#   dffrdd2<-dff2 %>% 
#     select(cohort, time, time2,starts_with(x)) %>% 
#     select(-x) %>% 
#     pivot_longer(cols = starts_with(x)) %>% 
#     mutate(after=ifelse(time2>24, "after", "before")) %>% ############### treatment time-1
#     pivot_wider(names_from = after, values_from = value) %>% 
#     mutate(treatment="treatment")
#   
# dfrdd<-rbind(dffrdd1, dffrdd2)
#   
#   
# gg<-
#     dfrdd%>% 
#     mutate(after=ifelse(after%in% c(0,1), NA, after)) %>% 
#     mutate(time=time-25) %>%                     ##########treatment time
#     ggplot(aes(x=time2,color=as.factor(treatment))) +
#     geom_point(aes(y=before, shape=as.factor(treatment), alpha=as.factor(treatment))) +
#       geom_line(aes(y=before, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
#     geom_point(aes(y=after, shape= as.factor(treatment), alpha=as.factor(treatment)))+
#       geom_line(aes(y=after, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
#     geom_line(stat = "smooth", se=F, aes(y=after, alpha=as.factor(treatment)), method = "lm")+
#     geom_line(stat="smooth", se=F, aes(y=before, alpha=as.factor(treatment)), method = "lm")+
#     geom_vline(xintercept = 25)+ ############### treatment time
#     scale_color_manual(values = c("#00203FFF", "#008080"))+
#     scale_alpha_manual(values = c(.7,1))+
#     scale_shape_manual(values = c(18,16))+
#       scale_linetype_manual(values = c("dashed", "solid"))+
#       theme_bw()+
#     theme(legend.position = "bottom", 
#           legend.title = element_blank(),
#           axis.title.y=element_blank())+
#     facet_wrap(~name)
#   ggsave2(gg, file=paste0("../../../../../../Plots/RDD_descriptive3/last_try",x, ".jpeg"), width=8, height = 7)
# }
# 
# 



### ACTUAL DIFF-IN-DIFF ###
# dftwfe<-rbind(dff1 %>% mutate(treatment=0), 
#               dff2 %>% mutate(treatment=1))
# 


# 
# ######################### Analysis disaggregating by type of contract#############################
# 
# load("situation_cohort_panel.Rdata")
# 
# for (g in c("permanent", "open-ended", "project-based", "production circumstances")){
# dftwfe<-create_twfe_dataframe(dff[dff$group==g,], 
#                               first_variable = 3,
#                               last_variable = 16,
#                               time_dif = 12, 
#                               treatment_time = 37,
#                               months = 28)
# 
# 
# create_twfeplots(dftwfe, paste0("DID_long", g), 37)
# }
# 
# 
# ###############NO DONUT DESIGN ###########
# 
# load("anykind_cohort_panel.Rdata")
# 
# create_twfe_dataframe<- function(df, first_variable, last_variable, time_dif, treatment_time, months ){
#   
#   
#   dffnames<-names(df[,first_variable:last_variable])
#   beginning<-treatment_time-months
#   
#   df1<-mutate(df[df$cohort %in%c(beginning:(treatment_time-1)),], time2=time+time_dif)
#   
#   df2<- mutate(df[df$cohort >=(beginning+time_dif),], time2=time)
#   
#   for (x in dffnames){
#     for (i in 1:6) {
#       
#       df1<-df1 %>% 
#         arrange(cohort, -time) %>% 
#         group_by(cohort) %>% 
#         mutate(!!paste0(x, i):=lag(get(x), i),
#                treatment=0)
#       
#       df2<-df2 %>% 
#         arrange(cohort, -time) %>% 
#         group_by(cohort) %>% 
#         mutate(!!paste0(x, i):=lag(get(x), i),
#                treatment=1)
#     }
#   }
#   
#   df1<-df1 %>% filter(cohort==time)
#   df2<-df2 %>% filter(cohort==time)
#   
#   df<-rbind(df1,df2)
#   
# }
# 
# 
# 
# dftwfe<-create_twfe_dataframe(dff, 
#                       first_variable = 2,
#                       last_variable = 15,
#                       time_dif = 12, 
#                       treatment_time = 25,
#                       months = 28)
# 
# create_twfeplots(dftwfe, new_directory = "no-donut_long", treatment_date = 25)
# 
# 
# 
# for (i in c("permanent", "open-ended", "project-based", "production circumstances")){
#   dftwfe<-create_twfe_dataframe(dff[dff$group==i,], 
#                                 first_variable = 3,
#                                 last_variable = 16,
#                                 time_dif = 12, 
#                                 treatment_time = 37,
#                                 months = 21)
#   
#   
#   create_twfeplots(dftwfe, paste0("DID_nodonut", i), 37)
# }
# 
# 
# 
