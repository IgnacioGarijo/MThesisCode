# library(haven)
# library(data.table)
# library(tidyverse)
# library(lubridate) #to calculate differences between dates
# library(zoo) 
# library(cowplot) #To save plots
# #library(didimputation)
# library(fixest) #For feols
# library(ggfixest) #To plot the DiD with ggiplot
# 
# theme_set(theme_minimal())

# setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/2º/2 semestre/TFM/Código/DiegoPuga/esurban_replication/esurban_replication/tmp/mcvl_cdf_2022")



#Loading dataframe

# load("finalpanel2019b.Rdata")


############################ FUNCTION TO LOAD DFS WITH A SPECIFIC NAME ###################

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# min_year <- 2019
# max_year <- 2022
# min_month <- 1
# max_month <- 12




####################### FUNCTION TO CREATE THE INCOME DF ######################

create_income_df<-function(min_year=2019, max_year=2022, min_month=1, max_month=12){
  
contr1<- loadRData("processed_contribution_1.Rdata")
contr1 <- contr1[contr1$year >= min_year , ]
contr1$month <- as.numeric(contr1$month)
contr1$time <- (contr1$year - min(contr1$year)) * 12 + (contr1$month - min(contr1$month) + 1)
contr1 <- contr1[, c("person_id", "time", "income")]

contr2<- loadRData("processed_contribution_2.Rdata")

contr2 <- contr2[contr2$year >= min_year, ]
contr2$month <- as.numeric(contr2$month)
contr2$time <- (contr2$year - min(contr2$year)) * 12 + (contr2$month - min(contr2$month) + 1)
contr2 <- contr2[, c("person_id", "time", "income")]

contr3<- loadRData("processed_contribution_3.Rdata")

contr3 <- contr3[contr3$year  >= min_year, ]
contr3$month <- as.numeric(contr3$month)
contr3$time <- (contr3$year - min(contr3$year)) * 12 + (contr3$month - min(contr3$month) + 1)
contr3 <- contr3[, c("person_id", "time", "income")]

contr4<- loadRData("processed_contribution_4.Rdata")

contr4 <- contr4[contr4$year  >= min_year, ]
contr4$month <- as.numeric(contr4$month)
contr4$time <- (contr4$year - min(contr4$year)) * 12 + (contr4$month - min(contr4$month) + 1)
contr4 <- contr4[, c("person_id", "time", "income")]


contr5<- loadRData("processed_contribution_5.Rdata")

contr5 <- contr5[contr5$year  >= min_year, ]
contr5$month <- as.numeric(contr5$month)
contr5$time <- (contr5$year - min(contr5$year)) * 12 + (contr5$month - min(contr5$month) + 1)
contr5 <- contr5[, c("person_id", "time", "income")]

contr6<- loadRData("processed_contribution_6.Rdata")

contr6 <- contr6[contr6$year  >= min_year, ]
contr6$month <- as.numeric(contr6$month)
contr6$time <- (contr6$year - min(contr6$year)) * 12 + (contr6$month - min(contr6$month) + 1)
contr6 <- contr6[, c("person_id", "time", "income")]

contr7<- loadRData("processed_contribution_7.Rdata")

contr7 <- contr7[contr7$year  >= min_year, ]
contr7$month <- as.numeric(contr7$month)
contr7$time <- (contr7$year - min(contr7$year)) * 12 + (contr7$month - min(contr7$month) + 1)
contr7 <- contr7[, c("person_id", "time", "income")]

contr8<- loadRData("processed_contribution_8.Rdata")

contr8 <- contr8[contr8$year  >= min_year, ]
contr8$month <- as.numeric(contr8$month)
contr8$time <- (contr8$year - min(contr8$year)) * 12 + (contr8$month - min(contr8$month) + 1)
contr8 <- contr8[, c("person_id", "time", "income")]

contr9<- loadRData("processed_contribution_9.Rdata")

contr9 <- contr9[contr9$year  >= min_year, ]
contr9$month <- as.numeric(contr9$month)
contr9$time <- (contr9$year - min(contr9$year)) * 12 + (contr9$month - min(contr9$month) + 1)
contr9 <- contr9[, c("person_id", "time", "income")]

contr10<- loadRData("processed_contribution_10.Rdata")

contr10 <- contr10[contr10$year  >= min_year, ]
contr10$month <- as.numeric(contr10$month)
contr10$time <- (contr10$year - min(contr10$year)) * 12 + (contr10$month - min(contr10$month) + 1)
contr10 <- contr10[, c("person_id", "time", "income")]

contr11<- loadRData("processed_contribution_11.Rdata")

contr11 <- contr11[contr11$year  >= min_year, ]
contr11$month <- as.numeric(contr11$month)
contr11$time <- (contr11$year - min(contr11$year)) * 12 + (contr11$month - min(contr11$month) + 1)
contr11 <- contr11[, c("person_id", "time", "income")]

contr12<- loadRData("processed_contribution_12.Rdata")

contr12 <- contr12[contr12$year  >= min_year, ]
contr12$month <- as.numeric(contr12$month)
contr12$time <- (contr12$year - min(contr12$year)) * 12 + (contr12$month - min(contr12$month) + 1)
contr12 <- contr12[, c("person_id", "time", "income")]

dfincome<-rbind(contr1, contr2, contr3, contr4, contr5, contr6, contr7, contr8, contr9, contr10, contr11, contr12)

rm(contr1, contr2, contr3, contr4, contr5, contr6, contr7, contr8, contr9, contr10, contr11, contr12)
return(dfincome)
}

########### FUNCTION FOR MANAGEABLE DF ####################


create_manageable_df<- function(df,min_year=2019, min_month=1){
setDT(df)

#Reducing the df to make it easier to manage

df<-df[,c("person_id", "exit_date", "entry_date", "year", "month", "days_spell", "job_relationship", "regime", 
          "contr_type", "ncontracts", "occupation", "sector", "birth_date", "sex","person_muni_latest")]
df<-df[df$year>=min_year]

#First let's create the time variable for the cohorts



df[, c("time", "yearmonth", "exit_month"):= list((year - min_year) * 12 + (month - min_month + 1),
                                                  as.numeric(paste0(year, sprintf("%02d", month))),
                                                  exit_date %/% 100)]


df<-df %>% select(-c(exit_date, year, month))
return(df)
}




# min_time<- min(df$time)
# max_time<-max(df$time)
# 


##################### FUNCTION THAT CREATES THE COHORTS #####################



create_cohort<-function(df,
                        aggregation=NULL,
                        pbonly=FALSE,
                        rc=FALSE,
                        name){
  
  if(rc==T){
    df<-df[df$time>=12,]
    df$time <- df$time-12
    max_time<-max_time-12
  }
  
for (i in min_time:max_time){
  
  
  if(pbonly==T) {
    df1 <- df %>%
      group_by(person_id) %>%
      mutate(treatment = ifelse(any(time == i & yearmonth == exit_month & situation == "project-based"), 1, NA)) %>%
      filter(!is.na(treatment)) %>%
      select(-treatment)
  } else {
    df1 <- df %>%
      group_by(person_id) %>%
      mutate(treatment = ifelse(any(time == i & yearmonth == exit_month & situation != "unemp"), 1, NA)) %>%
      filter(!is.na(treatment)) %>%
      select(-treatment)
  }


if(rc==T){
  used<-unique(df1$person_id)  
  df <- df[!(df$person_id %in% used), ]
}
  
#Now let's create a complete grid for the individuals so there is one observation per person per month

complete_grid<-expand.grid(person_id=unique(df1$person_id),
                           time=i:(i+6)) 

df1<-merge(complete_grid, df1, by=c("person_id", "time"), all.x = TRUE)


df1$days_spell[is.na(df1$days_spell)]<-0


setDT(df1)


#Then let's define the situation of each person and eliminate unnecesary columns


df1[, emp:= ifelse(!is.na(contr_type)| job_relationship %in% c(87,901,902,910,930,932,937,951),1,0)]

df1[,  situation:= ifelse(!is.na(contr_type),
                          contr_type,
                          ifelse(regime %in% c(500:540, 721:740),
                                 "self-emp",ifelse(emp==1, "other",
                                                   "unemp")
                                 )
                          )
    ]

df1$situation[is.na(df1$situation)]<-"unemp"

df1$regime<-NULL
df1$job_relationship<-NULL
df1$contr_type<-NULL
df1$yearmonth<-NULL
df1$exit_month<-NULL

#Let´s also merge with income data 

df1<-merge(df1, dfincome, by=c("person_id", "time"), all.x = T )

df1$income[is.na(df1$income)] <- 0


if (is.null(aggregation)) {
  df1 <- df1 %>%
    group_by(time) 
} else {
  df1 <- df1 %>%
    arrange(person_id, time) %>% 
    group_by(person_id) %>% 
    mutate(group=first(get(aggregation))) %>% 
    group_by(time, group)
}



df1<-df1 %>% 
  summarise(days_worked=mean(days_spell, na.rm=T),
            salaries=mean(income, na.rm=T),
            ncontracts=mean(ncontracts, na.rm = T),
            days_worked=mean(days_spell, na.rm = T),
            internship = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                            0,
                                            sum(situation == "Internship or training") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            open_ended = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                0,
                                sum(situation == "open-ended") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            other_temporary = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                     0,
                                     sum(situation == "other temporary") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            permanent = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                               0,
                               sum(situation == "permanent") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            pre_retirement = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                    0,
                                    sum(situation == "Pre-retirement") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            production_circumstances = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                              0,
                                              sum(situation == "production circumstances") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            project_based = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                   0,
                                   sum(situation == "project-based") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            replacement = ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                                 0,
                                 sum(situation == "Replacement") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            other= ifelse(sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE) == 0,
                          0,
                          sum(situation == "other") / sum(situation != "unemp" & situation!= "self-emp", na.rm = TRUE)),
            unemployed= sum(situation=="unemp", na.rm=T)/n(),
            self_emp= ifelse(sum(situation!= "unemp", na.rm=T) ==0, 0, sum(situation=="self-emp", na.rm=T)/sum(situation!="unemp", na.rm=T)),
            cohort=i

  )

save(df1, file=paste0(name, i, ".Rdata"))
}

  load(paste0(name, "1.Rdata"))
  
  dff<-df1
              for (i in (min_time+1):max_time){
                load(paste0(name, i, ".Rdata"))
                dff<-rbind(df1, dff)
                gc()
              }
  
  save(dff, file=paste0(name,"_panel.Rdata"))
  
              for (i in min_time:max_time){
                file.remove(paste0(name, i, ".Rdata"))
              }
}


############### FUNCTION THAT TRANSFORMS THE DATA FROM THE COHORTS TO COMPUTE THE RESULTS ##############



create_twfe_dataframe<- function(df, first_variable, last_variable, time_dif, treatment_time, months, rdd=FALSE ){
  
  
  dffnames<-names(df[,first_variable:last_variable])
  beginning<-treatment_time-months
  
  df1<-mutate(df[df$cohort %in%c(beginning:(treatment_time-1)),], time2=time+time_dif)
  
  df2<- mutate(df[df$cohort >=(beginning+time_dif),], time2=time)
  
  for (x in dffnames){
    for (i in 1:6) {
      
      cutoff<-treatment_time-i #For the doughnut RDD
      
      df1<-df1 %>% 
        arrange(cohort, -time) %>% 
        group_by(cohort) %>% 
        mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:(treatment_time-1)),NA, lag(get(x), i)),
               treatment=0)
      
      df2<-df2 %>% 
        arrange(cohort, -time) %>% 
        group_by(cohort) %>% 
        mutate(!!paste0(x, i):=ifelse(time2 %in% c(cutoff:(treatment_time-1)),NA, lag(get(x), i)),
               treatment=1)
    }
  }
  
  df1<-df1 %>% filter(cohort==time)
  df2<-df2 %>% filter(cohort==time)
  
      if (rdd==FALSE){
      df<-rbind(df1,df2) %>% 
        mutate(time2=time2-treatment_time)
      
      return(df)
      
      } else {
        return(list(df1=df1, df2=df2, dfnames=dffnames))
      }
      
}




#######################FUNCTION THAT COMPUTES THE RDD GRAPHS ########################

#First obtain the dfs and names list

create_rdd_figures<-function(df1, df2, dffnames, treatment_time, new_directory){
  dir.create(paste0("../../../../../../Plots/", new_directory))
  
  for (x in dffnames){
    dffrdd1<-df1 %>% 
      select(cohort, time, time2, starts_with(x)) %>% 
      select(-x) %>% 
      pivot_longer(cols = starts_with(x)) %>% 
      mutate(after=ifelse(time2>(treatment_time-1), "after", "before")) %>% 
      pivot_wider(names_from = after, values_from = value) %>% 
      mutate(treatment= "Placebo")
    
    dffrdd2<-df2 %>% 
      select(cohort, time, time2,starts_with(x)) %>% 
      select(-x) %>% 
      pivot_longer(cols = starts_with(x)) %>% 
      mutate(after=ifelse(time2>(treatment_time-1), "after", "before")) %>% 
      pivot_wider(names_from = after, values_from = value) %>% 
      mutate(treatment="treatment")
    
    dfrdd<-rbind(dffrdd1, dffrdd2)
    
    gg<-
      dfrdd%>% 
      mutate(after=ifelse(after%in% c(0,1), NA, after)) %>% 
      mutate(time=(time-treatment_time)) %>%                     
      ggplot(aes(x=time2-treatment_time,color=as.factor(treatment))) +
      geom_point(aes(y=before, shape=as.factor(treatment), alpha=as.factor(treatment))) +
      geom_line(aes(y=before, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
      geom_point(aes(y=after, shape= as.factor(treatment), alpha=as.factor(treatment)))+
      geom_line(aes(y=after, linetype=as.factor(treatment), alpha=as.factor(treatment))) +
      geom_line(stat = "smooth", se=F, aes(y=after, alpha=as.factor(treatment)), method = "lm")+
      geom_line(stat="smooth", se=F, aes(y=before, alpha=as.factor(treatment)), method = "lm")+
      geom_vline(xintercept = 0)+ 
      scale_color_manual(values = c("#00203FFF", "#008080"))+
      scale_alpha_manual(values = c(.7,1))+
      scale_shape_manual(values = c(18,16))+
      scale_linetype_manual(values = c("dashed", "solid"))+
      theme_bw()+
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            axis.title=element_blank())+
      facet_wrap(~name)
    ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory, "/", x, ".jpeg"), width=8, height = 6)
    
  }
  
}













##############FUNCTION THAT COMPUTES THE RESULTS #########################



create_results<- function(dftwfe, new_directory, disaggregation=FALSE, figures=TRUE){
 if(figures==T) {dir.create(paste0("../../../../../../Plots/", new_directory))}
  
  dff1<-dftwfe[dftwfe$time2<max(dftwfe$time2),]
  dff2<-dftwfe[dftwfe$time2<max(dftwfe$time2)-1,]
  dff3<-dftwfe[dftwfe$time2<max(dftwfe$time2)-2,]
  dff4<-dftwfe[dftwfe$time2<max(dftwfe$time2)-3,]
  dff5<-dftwfe[dftwfe$time2<max(dftwfe$time2)-4,]
  dff6<-dftwfe[dftwfe$time2<max(dftwfe$time2)-5,]
  
  dflist<-list(dff1, dff2, dff3, dff4,dff5, dff6)
  modelsgg<-list()
  models<-list()
  did_coefs<-list()
  pre_trends<-list()

  outcome_variables<- c("days_worked", "salaries", "ncontracts", "open_ended", "permanent", "project_based", "self_emp", "unemployed")
  
  for(x in outcome_variables){
    for (y in c(1:6)){
      
      model<-feols(reformulate("i(time2, treatment, ref = +0)", paste0(x,y)), 
                   data = dflist[[y]])
      
      modelsgg[[paste("mod",x, y, sep = "_")]]<-model

      if (disaggregation==FALSE ){
        models[[paste("mod",x, y, sep = "_")]]<-model
        did_coefs[[paste("mod",x, y, sep = "_")]]<-summary(model, agg = c("ATT" = "time2::[^-]"))
        pre_trends[[paste("mod",x, y, sep = "_")]]<-wald(model, "time2::-")
      } else if (disaggregation==TRUE) {
        models[[paste("mod",g,x, y, sep = "_")]]<-model
        did_coefs[[paste("mod",g,x, y, sep = "_")]]<-summary(model, agg = c("ATT" = "time2::[^-]"))
        pre_trends[[paste("mod",g,x, y, sep = "_")]]<-wald(model, "time2::-")
        
      }
      
    }
    
    if (figures==TRUE){
    gg<-
      ggiplot(list("1 month later"=modelsgg[[paste("mod",x, 1, sep = "_")]], 
                   "2 months later"=modelsgg[[paste("mod",x, 2, sep = "_")]],
                   "3 months later"= modelsgg[[paste("mod",x, 3, sep = "_")]],
                   "4 months later"= modelsgg[[paste("mod",x, 4, sep = "_")]],
                   "5 months later"= modelsgg[[paste("mod",x, 5, sep = "_")]],
                   "6 months later"= modelsgg[[paste("mod",x, 6, sep = "_")]]
      ),
      pt.join=TRUE, 
      pt.pch=19,
      multi_style = 'facet',
      facet_args = list(ncol=2, 
                        scales= "free_y",
                        labeller= labeller(category="")
      ),
      col= rep("#008080",6),
      main= paste0("Treatment effects on ", x)
      )+ 
      theme_bw()+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            strip.background = element_rect(fill = "#9dbbc9"))+
      geom_point(size=2, alpha=.7)+
      geom_line(linewidth=1, alpha=.7)
      
      ggsave2(gg, file=paste0("../../../../../../Plots/", new_directory,"/DID_", x,".jpeg"), width = 7, height = 5)
    } else{}
  }
  tables<-list("models"=models, "did_coefs"=did_coefs, "pre_trends"=pre_trends)
  return(tables)
}


### FIX LOCATION CODES ##

replace_province <- function(x) {
  ifelse(nchar(x) == 1, NA,
         ifelse(nchar(x) == 4, paste0("0", substr(x, 1, 1)),
                ifelse(nchar(x) >= 5, substr(x, 1, 2), x)))
}



########JUNK #########


#Calculate number of people in each cohort
# 
# df1<-df %>% 
#   #filter(entry_date<20220000) %>% 
#   mutate(laid_off= ifelse(contr_type=="permanent" & yearmonth==exit_month,1,0),
#          end_contract= ifelse(contr_type %in% c("project-based", "Internship or training", "other temporary", "production circumstances", "Replacement") & yearmonth==exit_month,1,0))
# 
# df1<-df1 %>% 
#   group_by(time) %>% 
#   summarize(permanent_laidoff=sum(laid_off, na.rm=T),
#             temporary_ended=sum(end_contract)) %>% 
#   pivot_longer(cols = c(2,3)) %>% 
#   group_by(name) %>% 
#   mutate(diffY=(value-lag(value,12))/lag(value,12))
# 
# df1 %>% 
#   ggplot(aes(x=time, y=value, color=name))+
#   geom_line()+
#   geom_vline(xintercept = c(13,25))
# 
# 
# ## Descriptive graphs ##
# 
# df1<-df %>% 
#   mutate(time %in% c(8:28))
# 
# 
# complete_grid<-expand.grid(person_id=unique(df1$person_id),
#                            time=8:28) 
# 
# df1<-merge(complete_grid, df1, by=c("person_id", "time"), all.x = TRUE)
# 
# 
# df1$days_spell[is.na(df1$days_spell)]<-0
# df1$ncontracts[is.na(df1$ncontracts)]<-0
# 
# 
# setDT(df1)
# 
# 
# df1[, emp:= ifelse(!is.na(contr_type)| job_relationship %in% c(87,901,902,910,930,932,937,951),1,0)]
# 
# df1[,  situation:= ifelse(!is.na(contr_type),
#                           contr_type,
#                           ifelse(regime %in% c(500:540, 721:740),
#                                  "self-emp",ifelse(emp==1, "other",
#                                                    "unemp")
#                           )
# )
# ]
# 
# df1$situation[is.na(df1$situation)]<-"unemp"
# 
# 
# dfemp<-df1 %>% 
#   group_by(time) %>% 
#   summarize(employed= sum(emp))
# 
# ggunemp1<-dfemp %>% 
#   ggplot(aes(x=time, y=employed))+
#   geom_line()+
#   geom_vline(xintercept = c(13,25))+
#   ggtitle("Stock of employed")
# 
# dfnewunemp<-df1 %>% 
#   arrange(person_id, time) %>% 
#   mutate(new_unemp=ifelse(emp==0 & lag(emp)==1, 1, 0),
#          new_emp=ifelse(emp==1 & lag(emp)==0,1,0)) %>% 
#   group_by(time) %>% 
#   summarize(new_unemp=sum(new_unemp),
#             new_emp=sum(new_emp)) %>% 
#   pivot_longer(cols = c(2,3))
# 
# ggunemp2<-dfnewunemp %>% 
#   filter(time>8) %>% 
#   ggplot(aes(x=time, y=value, color=name)) +
#   geom_line()+
#   geom_vline(xintercept = c(13,25))+
#   theme(legend.position = "bottom",
#         legend.title = element_blank())+
#   ggtitle("Flows of new employment and unemployment spells")
# 
# plot_grid(ggunemp1, ggunemp2, nrow = 2, rel_heights = c(1,1.5))
# 
# dfperm<-df1 %>% 
#   group_by(time) %>% 
#   summarize(permanent_stock= sum(situation=="permanent"))
# 
# ggperm1<-dfperm %>% 
#   ggplot(aes(x=time, y=permanent_stock))+
#   geom_line()+
#   geom_vline(xintercept = c(13,25))+
#   ggtitle("Stock of permanent workers")
# 
# dfpermflows<-df1 %>% 
#   filter(situation=="permanent") %>% 
#   mutate(entry_month=entry_date %/% 100,
#          new_permanent= ifelse(situation=="permanent" & entry_month==yearmonth, 1, 0)) %>% 
#   group_by(time) %>% 
#   summarise(permanent_flows=sum(new_permanent))
# 
# ggperm2<-dfpermflows %>% 
#   ggplot(aes(x=time, y =permanent_flows))+
#   geom_line() +
#   geom_vline(xintercept = c(13,25))+
#   ggtitle("flow of permanent workers")
# 
# plot_grid(ggperm1, ggperm2, nrow = 2)
