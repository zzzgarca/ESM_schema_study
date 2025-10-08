##Rulatai secventa aceasta
install.packages("pacman")
pacman::p_load(Amelia,
               apaTables,
               broom,
               dplyr,
               emmeans,
               epiDisplay,
               flextable,
               ggplot2,
               ggpubr,
               ggsignif, 
               haven,
               lubridate,
               marginaleffects,
               mice,
               misty,
               mitools,
               modelbased,
               multcomp,
               poorman,
               QuantPsyc,
               report,
               rempsyc,
               rstatix,
               see,
               survey,
               tibble,
               tidyr,
               tidyverse
)

options(scipen=9999)

###### Zona de mai jos nu va intereseaza

data10 <- read.csv ("/Users/silviumatu/Desktop/R/Studiu Expwell/Singles study/Date_ESM_Singles.csv")
data11 <- read.csv ("/Users/silviumatu/Desktop/R/Studiu Expwell/Singles study/Date_initiale_Singles.csv")

data10 <- read.csv ("/Users/silviumatu/Desktop/R/Studiu Expwell/Anger study/Date_ESM_Anger.csv")
data11 <- read.csv ("/Users/silviumatu/Desktop/R/Studiu Expwell/Anger study/Date_initiale_Anger.csv")

data10$time_schedule<-gsub("(M).*","\\1", data10$time_schedule)

format1<-'mdy H%:%M'
format2<-'mdy I%:%M %p'

data10$start_time= parse_date_time(data10$start_time, c(format1, format2))
data10$end_time= parse_date_time(data10$end_time, c(format1, format2))
data10$time_schedule= parse_date_time(data10$time_schedule, c(format1, format2))

data10<-dplyr::arrange(data10, start_time)
data10<-data10[order(data10$participant_id),]

data10$study_begin <- ymd_hms(data10$start_time)
#data10<-subset(data10, select = -c(study_begin,study_begin2))

data10$response_no=0
data10$time_difference<-floor(as.numeric(difftime(data10$start_time, data10$study_begin, units = "hours")))
n=nrow(data10)

for (i in 2:n){
  if (data10$participant_id[i]==data10$participant_id[i-1]){
    data10$study_begin[i]<- ymd_hms(data10$study_begin[i-1])
    data10$response_no[i]<-data10$response_no[i-1]+1
  }
  
}

data10$day_difference<-floor(as.numeric(difftime(data10$start_time, data10$study_begin, units = "days")))
data10$day_id[1]<-1
data10$day_id_within_participant[1]<-0
for (i in 2:n){
  if((data10$participant_id[i]==data10$participant_id[i-1]) && (data10$day_difference[i]==data10$day_difference[i-1])){
    data10$day_id[i]<-data10$day_id[i-1]
    data10$day_id_within_participant[i]<-data10$day_id_within_participant[i-1]
  } else {
    data10$day_id[i]<-data10$day_id[i-1]+1
  }
  
}

for (i in 2:n){
  if(data10$participant_id[i]==data10$participant_id[i-1]){
    if (data10$day_id[i]==data10$day_id[i-1]){
      data10$day_id_within_participant[i]<-data10$day_id_within_participant[i-1]
    } else{
      data10$day_id_within_participant[i]<-data10$day_id_within_participant[i-1]+1
      }
  } 
  
}


data10 = merge(x=data10,y=data11,by="id", all = TRUE)
data10<-subset(data10, !is.na(response_id))



write.csv(data10,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Singles study/Date_ESM_Singles_export.csv", row.names=F)



write.csv(data10,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Anger study/Date_ESM_Anger_export.csv", row.names=F)




##De aici vă interesează pe voi!!!
##Adresa fiseirelor o sa fie ceva de forma: "C:\Users\liviu\Desktop\Studiu\Test.csv"

data10 <- read.csv ("/Users/silviumatu/Desktop/R/Studiu Expwell/Singles study/Date_ESM_Singles_export.csv")

data10 <- read.csv ("/Users/silviumatu/Desktop/R/Studiu Expwell/Anger study/Date_ESM_Anger_export.csv")


data10$C_nevoie_apartenenta_within_Day<-center(data10$nevoie_apartenenta, type = "CWC", cluster = data10$day_id)
data10$C_teama_singuratate_within_Day<-center(data10$teama_singuratate, type = "CWC", cluster = data10$day_id)
data10$C_suport_social_within_Day<-center(data10$suport_social, type = "CWC", cluster = data10$day_id)

data10 <- data10 %>%
  group_by(day_id) %>%
  mutate(nevoie_apartenenta_Daily_average= mean(nevoie_apartenenta, na.rm=TRUE)) %>%
  ungroup()
data10 <- data10 %>%
  group_by(participant_id) %>%
  mutate(nevoie_apartenenta_Total_average= mean(nevoie_apartenenta, na.rm=TRUE)) %>%
  ungroup()


data10 <- data10 %>%
  group_by(day_id) %>%
  mutate(teama_singuratate_Daily_average= mean(teama_singuratate, na.rm=TRUE)) %>%
  ungroup()
data10 <- data10 %>%
  group_by(participant_id) %>%
  mutate(teama_singuratate_Total_average= mean(teama_singuratate, na.rm=TRUE)) %>%
  ungroup()


data10 <- data10 %>%
  group_by(day_id) %>%
  mutate(suport_social_Daily_average= mean(suport_social, na.rm=TRUE)) %>%
  ungroup()
data10 <- data10 %>%
  group_by(participant_id) %>%
  mutate(suport_social_Total_average= mean(suport_social, na.rm=TRUE)) %>%
  ungroup()

data10$C_nevoie_apartenenta_Daily_average_within_Participant<-center(data10$nevoie_apartenenta_Daily_average, type = "CWC", cluster = data10$participant_id)
data10$C_nevoie_apartenenta_Total_average_GM<-center(data10$nevoie_apartenenta_Total_average)

data10$C_teama_singuratate_Daily_average_within_Participant<-center(data10$teama_singuratate_Daily_average, type = "CWC", cluster = data10$participant_id)
data10$C_teama_singuratate_Total_average_GM<-center(data10$teama_singuratate_Total_average)

data10$C_suport_social_Daily_average_within_Participant<-center(data10$suport_social_Daily_average, type = "CWC", cluster = data10$participant_id)
data10$C_suport_social_Total_average_GM<-center(data10$suport_social_Total_average)

data10 <- data10 %>%                           
  group_by(participant_id) %>%
  dplyr::mutate(lagged_C_nevoie_apartenenta_within_participant = lag(C_nevoie_apartenenta_within_Day, n = 1, default = NA))

data10 <- data10 %>%                           
  group_by(day_id) %>%
  dplyr::mutate(lagged_C_teama_singuratate_within_Day = lag(C_teama_singuratate_within_Day, n = 1, default = NA))


data10 <- data10 %>%                           
  group_by(day_id) %>%
  dplyr::mutate(lagged_C_suport_social_within_Day = lag(C_suport_social_within_Day, n = 1, default = NA))

write.csv(data10,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Singles study/Date_ESM_Singles_export.csv", row.names=F)

write.csv(data10,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Anger study/Date_ESM_Anger_export.csv", row.names=F)


