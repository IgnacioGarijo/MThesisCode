df<- data.frame(person_id= c(1,1,1,2,2,2,2),
                entry_date= c(20200101, 20200123, 20200203, 20221102, 20221101,20221205, 20221220),
                exit_date=c(20200110, 20200201, 20200404, 20221131, 20221205, 20221216, 20230105),
                contract_type=c(1, NA, NA, 4,6,NA,99 ),
                var1=c(5,6,7,9,4,8,5),
                var2=c("A","D","G","W","A","P","O"))


df$entry_month<- as.numeric(substr(df$entry_date, 5,6))
df$exit_month<- as.numeric(substr(df$exit_date,5,6))


df$entry_year<- as.numeric(substr(df$entry_date,1,4))
df$exit_year<- as.numeric(substr(df$exit_date, 1, 4))
#########
df$entry_day<-as.numeric(substr(df$entry_date, 7,8))
df$exit_day<- as.numeric(substr(df$exit_date, 7,8))
############


for (i in 2020:2023){
  df<-df %>% 
    mutate(!!paste0("year_", i) := ifelse(i>=entry_year & i<=exit_year, i, NA))
}

df<-df %>% 
  pivot_longer(cols = c(13:16),names_to = "yearx", values_to = "year")

df<-df %>% select(-yearx) %>% filter(!is.na(year))



for (x in 1:12) {
  df<-df %>% 
    mutate(!!paste0("month_", x) := ifelse((year > entry_year & year < exit_year) |  
                                             (year==entry_year & year<exit_year & x >=entry_month) |
                                             (year>entry_year & year==exit_year & x<= exit_month) |
                                             (year==entry_year & year==exit_year & x >= entry_month & x <=exit_month),
                                           x, NA))
}

df<-df %>% 
  pivot_longer(cols = c(14:25), names_to = "monthx", values_to = "month")

df<-df %>% select(-monthx) %>% filter(!is.na(month))




df <-df %>% 
  mutate(date2=as.numeric(sprintf("%d%02d", year, month)),
         monthstart= as.numeric(substr(entry_date, 1, 6)),
         monthend= as.numeric(substr(exit_date, 1, 6)),
         daystart= as.numeric(substr(entry_date,7,8)),
         dayend= as.numeric(substr(exit_date, 7,8)),
         msd=case_when(monthstart<date2 ~ 1,
                       monthstart==date2 ~ daystart),
         med=case_when(monthend>date2 ~ ifelse(month==2, 28,30), 
                       monthend== date2 ~ ifelse(dayend==31, 30, dayend)))

setDT(df)

for (x in 1:30) {
  df[, paste0("day_", x) := +(any(x >= msd & x <= med & !is.na(contract_type))), 
           by = .(person_id, year, month)]
}



for(x in 1:30){
  df[, paste0("day_", x) := ifelse(any(x>=msd & x <= med & !is.na(contract_type)),1,0), by=.(person_id, year, month)]
}

df <- df %>%
  mutate(days_spell= rowSums(select(., starts_with("day_"))))

setorder(df, person_id)


df<-data.frame(person_id= c(1,1,1,1,1,2,2,3,3,3),
               payment_type= c("A", "B", "C", "D", "E", "A", "B", "A", "B", "B"),
               payment_amount=c(10, 20, 10, 10, 20, 20, 30, 40 ,10, 100))

df <- df %>% 
  group_by(person_id) %>% 
  mutate(pension= ifelse(payment_type=="B",payment_amount, 0),
         salaries= ifelse(payment_type != "B" & payment_type!="C" , payment_amount,0),
         unemployment= ifelse(payment_type=="C", payment_amount, 0), 
         pension= sum(pension), 
         salaries=sum(salaries),
         unemployment= sum(unemployment))

