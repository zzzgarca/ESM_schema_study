### install packages and load data ###
##install and load packages
install.packages('pacman') #no need to run if you have it installed

pacman::p_load(
  arsenal,
  plyr, #this has to be before dplyr because of some overwriting functions
  dplyr,
  lme4,
  lmerTest,
  lubridate,
  RcmdrMisc,
  readr,
  sjPlot,
  sjmisc,
  sjlabelled,
  stringr,
  tidyr,
  tidyverse
)
##


##read data and sent general variables of the environment
options(scipen=9999)

data1 <- read.csv ('/Users/silviumatu/Desktop/R/Studiu Expwell/Data.csv') #change path accordingly
data2 <- read.csv ('/Users/silviumatu/Desktop/R/Studiu Expwell/Pre-post data.csv') #change path accordingly
##


#### Processing ###
##basic checks
names(data1)
names(data2)
head(data1)
str(data1)
max(data1$participant_id)
max(data2$participant_id)
data1<-data1[order(data1$response_id),] #the data frame is sorted so that it is ordered by participant, day, and each response within each day of the participant
##

##Descriptive statistics of the sample
summary(data2) #this is for mean, min and max
numSummary(data2[,c("age", "BDI_TOTAL_pre", "YSQ_D1_pre", "YSQ_D2_pre", "LSAS_ANX_pre", "LSAS_AVOID_pre","LSAS_TOTAL_pre")], #this is for SD
           statistics = c("mean", "sd", "se(mean)", "skewness","kurtosis"))

unique(data1$participant_id)
count_participants<-as.data.frame(table(data1$participant_id)) #this data frame will include the number of reports for each participant
count_participants
numSummary(count_participants[,c("Freq")],
           statistics = c("mean", "sd", "se(mean)", "skewness","kurtosis"))
#count for reports per day per participants is the aggregation section bellow
#descriptive statistics for the N=145 sample is also in the aggregation section
##


##merge trait data with ESM data
n<-nrow(data1) #this is the n used throughout the code
m<-nrow(data2)

for (i in 1:n){
  for (j in 1:m) {
    if (data1$participant_id[i]==data2$participant_id[j]){
      data1$age[i]<-data2$age[j]
      data1$gender[i]<-data2$gender[j]
      data1$BDI_TOTAL_pre[i]<-data2$BDI_TOTAL_pre[j]
      data1$YSQ_D1_pre[i]<-data2$YSQ_D1_pre[j]
      data1$YSQ_D2_pre[i]<-data2$YSQ_D2_pre[j]
      data1$LSAS_ANX_pre[i]<-data2$LSAS_ANX_pre[j]
      data1$LSAS_AVOID_pre[i]<-data2$LSAS_AVOID_pre[j]
      data1$LSAS_TOTAL_pre[i]<-data2$LSAS_TOTAL_pre[j]
      data1$BDI_TOTAL_post[i]<-data2$BDI_TOTAL_post[j]
      data1$YSQ_D1_post[i]<-data2$YSQ_D1_post[j]
      data1$YSQ_D2_post[i]<-data2$YSQ_D2_post[j]
      data1$LSAS_ANX_post[i]<-data2$LSAS_ANX_post[j]
      data1$LSAS_AVOID_post[i]<-data2$LSAS_AVOID_post[j]
      data1$LSAS_TOTAL_post[i]<-data2$LSAS_TOTAL_post[j]
    }
  }
}
##


##compute scores on the ESM data
Self_Criticism<-c('q5', 'q6', 'q7')
Exp_Avoidance<-c('q8', 'q9', 'q10')
Soc_Anx<-c('q11', 'q12', 'q13')
Depress<-c('q14', 'q15', 'q16')

data1$Self_Criticism_T<- apply(data1[ ,Self_Criticism], 1, sum)
data1$Exp_Avoidance_T<- apply(data1[ ,Exp_Avoidance], 1, sum)
data1$Soc_Anx_T<- apply(data1[ ,Soc_Anx], 1, sum)
data1$Depress_T<- apply(data1[ ,Depress], 1, sum)
##


##number the responses within day for each participant
data1$response_within_day_participant[1]=0
for (x in 2:n){
  y=x-1
  if (data1$day_id[x]==data1$day_id[y]){
    data1$response_within_day_participant[x]<-data1$response_within_day_participant[y]+1
  } else {
    data1$response_within_day_participant[x]<-0
  }
} 
##


##re-code q2, q3, and q4
for (x in 1:n){
  if (data1$q2[x]==1){
    data1$q2_r[x]<-0
    data1$q2_r_social[x]<-0 #we are creating a dummy variable that codes if participants were confronted with a socially relevant situation
    data1$q2_r_other[x]<-0 #we are creating a dummy variable that codes if participants were confronted with a situation other than a socially relevant one
  } else{
    data1$q2_r[x]<-1
  }
  if (data1$q2[x]==2) {
    data1$q2_r_social[x]<-1
    data1$q2_r_other[x]<-0
  } else{
    if (data1$q2[x]>2) {
    data1$q2_r_other[x]<-1
    data1$q2_r_social[x]<-0
    }
  }
}

for (x in 1:n){
  if (data1$q2[x]==1){
    data1$q3_r[x]<-0 #here, if there is no situation on q2, then the stress level related to the situation is set to 0
    data1$q4_r[x]<-5 #here, if there is no situation on q2, then the coping level related to the situation is set to 5 (which means that the individual was capable to cope)
  } else{
    data1$q3_r[x]<-data1$q3[x]
    data1$q4_r[x]<-data1$q4[x]
  }
}
##


##compute number of reports and number of days with reports for each participant
columns = c('id','no', 'days') 
data3<-data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(data3) = columns
data3[nrow(data3) + 1 , ] <- NA
data3$id[1]<-data1$participant_id[1]
data3$no[1]<-1
data3$days[1]<-1
j<-1
for (i in 2:n){
  if (data1$participant_id[i]==data3$id[j]){
    data3$no[j]<-data3$no[j]+1
    if (data1$day_id[i]!=data1$day_id[i-1]){
      data3$days[j]<-data3$days[j]+1
    }
  } else {
    data3[nrow(data3) + 1 , ] <- NA
    j<-j+1
    data3$id[j]<-data1$participant_id[i]
    data3$no[j]<-1
    data3$days[j]<-1
  }
}

p<-nrow(data3) 
for(j in 1:p){ 
  for (i in 1:n){
    if(data1$participant_id[i]==data3$id[j]){
      data1$number_reports[i]<-data3$no[j]
      data1$number_days[i]<-data3$days[j]
    }
  }
}
##


##replace 22 as a year with 2022 in start_day, start_of_study, and end_day
data1$start_day<-str_replace(data1$start_day, '22$', '2022')
data1$start_of_study<-str_replace(data1$start_of_study,'22$', '2022')
data1$end_day<-str_replace(data1$end_day, '22$', '2022')
##


##extract start and end for time_schedule
data1$start_time_scheduled<-str_sub(data1$time_scheduled,start=1,end=18)
data1$end_time_scheduled<-str_sub(data1$time_scheduled,start=22,end=39)
##


##save time variables as POSIXct
data1$start_date<-format(as.POSIXct(data1$start_date, format = '%m/%d/%Y %I:%M%p'), format = '%m/%d/%Y %H:%M')
data1$end_date<-format(as.POSIXct(data1$end_date, format = '%m/%d/%Y %I:%M%p'), format = '%m/%d/%Y% %H:%M')
data1$start_time_scheduled<-format(as.POSIXct(data1$start_time_scheduled, format = '%m/%d/%Y %I:%M%p'), format = '%m/%d/%Y% %H:%M')
data1$end_time_scheduled<-format(as.POSIXct(data1$end_time_scheduled, format = '%m/%d/%Y %I:%M%p'), format = '%m/%d/%Y% %H:%M')
data1$start_day<-format(as.POSIXct(data1$start_day, format = '%m/%d/%Y'), format = '%m/%d/%Y')
data1$end_day<-format(as.POSIXct(data1$end_day, format = '%m/%d/%Y'), format = '%m/%d/%Y')
data1$start_of_study<-format(as.POSIXct(data1$start_of_study, format = '%m/%d/%Y'), format = '%m/%d/%Y')
##


##calculate reference times
data1<-data1[order(data1$response_id),] #to be sure that the data is sorted

#here we calculate the date and time of the first completion
for(i in 1:n){
  if(data1$report_id[i]==0){
    x<-data1$start_time_scheduled[i]
  }
  data1$begin_date_time[i]<-format(as.POSIXct(x, format = "%m/%d/%Y %H:%M"), format = "%Y-%m-%d %H:%M")
}

#here we calculate the date and time of the first completion within each day
for(i in 1:n){
  if(data1$response_within_day_participant[i]==0){
    y<-data1$start_time_scheduled[i]
  }
  data1$begin_date_time_within_day[i]<-format(as.POSIXct(y, format = "%m/%d/%Y %H:%M"), format = "%Y-%m-%d %H:%M")
}

#use the commented line bellow to remove a column; we used it to troubleshoot when calculating the reference_date_time column
#data1 <-subset(data1, select = -c(reference_date_time))

#here we calculate a new reference that is the beginning of the time window for the first completion
for(i in 1:n){
  reference_4<-as.POSIXct(paste(as.Date(data1$begin_date_time[i], format="%Y-%m-%d %H:%M"), format("19:00", format = "%H:%M" )), format = "%Y-%m-%d %H:%M")
  difference_4<-as.numeric(difftime(data1$begin_date_time[i], reference_4, units="mins"))
  if(difference_4>=0){
    data1$reference_date_time[i]<-format(as.POSIXct(reference_4, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M")
  } else {
    reference_3<-as.POSIXct(paste(as.Date(data1$begin_date_time[i], format="%Y-%m-%d %H:%M"), format("16:00", format = "%H:%M" )), format = "%Y-%m-%d %H:%M")
    difference_3<-as.numeric(difftime(data1$begin_date_time[i], reference_3, units="mins"))
    if(difference_3>=0){
      data1$reference_date_time[i]<-format(as.POSIXct(reference_3, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M")
    } else {
      reference_2<-as.POSIXct(paste(as.Date(data1$begin_date_time[i], format="%Y-%m-%d %H:%M"), format("13:00", format = "%H:%M" )), format = "%Y-%m-%d %H:%M")
      difference_2<-as.numeric(difftime(data1$begin_date_time[i], reference_2, units="mins"))
      if(difference_2>=0){
        data1$reference_date_time[i]<-format(as.POSIXct(reference_2, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M")
      } else {
        reference_1<-as.POSIXct(paste(as.Date(data1$begin_date_time[i], format="%Y-%m-%d %H:%M"), format('10:00', format = "%H:%M" )), format = "%Y-%m-%d %H:%M")
        difference_1<-as.numeric(difftime(data1$begin_date_time[i], reference_1, units="mins"))
        if(difference_1>=0){
          data1$reference_date_time[i]<-format(as.POSIXct(reference_1, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M")
        }
      }
    }
  }
}

#here we calculate a reference that contains only the day of the first response
data1$reference_day<-as.Date(data1$reference_date_time, format = "%Y-%m-%d")

#here we calculate a reference within each day
data1$reference_date_time_within_day<-as.POSIXct(paste(as.Date(data1$start_date, format="%m/%d/%Y %H:%M"), format("10:00", format = "%H:%M" )), format = "%Y-%m-%d %H:%M")
##


##check if any completion time is outside scheduled time
data1$end_time_scheduled<-format(as.POSIXct(data1$end_time_scheduled, format = "%m/%d/%Y %H:%M"), format = "%Y-%m-%d %H:%M")
data1$end_date<-format(as.POSIXct(data1$end_date, format = "%m/%d/%Y %H:%M"), format = "%Y-%m-%d %H:%M")
data1$difference_completion_schedule<-as.numeric(difftime(data1$end_time_scheduled, data1$end_date, units="mins"))
invisible(ifelse(data1$difference_completion_schedule<0,data1$check<-1, data1$check<-0)) #invisible to avoid displaying all the values generated by the function
data1$check<-as.factor(data1$check)
table(data1$check) #there are no cases where completion time is outside scheduled time, so we can use the entire data set
##


##compute time variables
data1$start_date<-format(as.POSIXct(data1$start_date, format = "%m/%d/%Y %H:%M"), format = "%Y-%m-%d %H:%M")
data1$time_intervals<-floor(as.numeric(difftime(data1$start_date, data1$reference_date_time, units='hours'))/3)
data1$start_day<-format(as.POSIXct(data1$start_day, format = "%m/%d/%Y"), format = "%Y-%m-%d")
data1$reference_day<-format(as.POSIXct(data1$reference_day, format = "%m/%d/%Y"), format = "%Y-%m-%d")
data1$time_days<-floor(as.numeric(difftime(data1$start_day, data1$reference_day, units='days')))
data1$time_intervals_intraday<-floor(as.numeric(difftime(data1$start_date, data1$reference_date_time_within_day, units='hours'))/3) #this variable acts as the moment of the day
##


##lagged variables
data_esm<-data1 #we create a new data set, before creating lagged variables

data_esm <- data_esm %>%                  
  group_by(day_id) %>%
  dplyr::mutate(lag_Self_Criticism_T = lag(Self_Criticism_T, n = 1, default = NA))
data_esm <- data_esm %>%                           
  group_by(day_id) %>%
  dplyr::mutate(lag_Exp_Avoidance_T = lag(Exp_Avoidance_T, n = 1, default = NA))
data_esm <- data_esm %>%                  
  group_by(day_id) %>%
  dplyr::mutate(lag_Soc_Anx_T = lag(Soc_Anx_T, n = 1, default = NA))
data_esm <- data_esm %>%                           
  group_by(day_id) %>%
  dplyr::mutate(lag_Depress_T = lag(Depress_T, n = 1, default = NA))
##


##centering predictors
#level 1 centering; we are centering using dplyr package
data_esm <- data_esm %>%
  group_by(day_id) %>%
  mutate(C_Self_Criticism_T_within_Day=Self_Criticism_T-mean(Self_Criticism_T, na.rm=TRUE), #these are the non-lagged variables; we are centering these for some contemporaneous analysis; laggs are centered below
         C_Exp_Avoidance_T_within_Day=Exp_Avoidance_T-mean(Exp_Avoidance_T, na.rm=TRUE),
         C_Soc_Anx_T_within_Day=Soc_Anx_T-mean(Soc_Anx_T, na.rm=TRUE),
         C_Depress_T_within_Day=Depress_T-mean(Depress_T, na.rm=TRUE),
         C_q2_r_within_Day=q2_r-mean(q2_r, na.rm=TRUE), #we are also centering the presence or absence of a negative event
         C_q2_r_social_within_Day=q2_r_social-mean(q2_r_social, na.rm=TRUE), #similar for the presence of a socially relevant event and bellow for another type of event
         C_q2_r_other_within_Day=q2_r_other-mean(q2_r_other, na.rm=TRUE),
         C_time_intervals_intraday_within_Day=time_intervals_intraday-mean(time_intervals_intraday, na.rm=TRUE), #here we center level 1 time (moment of the day)
         C_lag_Self_Criticism_T_within_Day=lag_Self_Criticism_T-mean(lag_Self_Criticism_T, na.rm=TRUE), #these are the lagged variables
         C_lag_Exp_Avoidance_T_within_Day=lag_Exp_Avoidance_T-mean(lag_Exp_Avoidance_T, na.rm=TRUE),
         C_lag_Soc_Anx_T_within_Day=lag_Soc_Anx_T-mean(lag_Soc_Anx_T, na.rm=TRUE),
         C_lag_Depress_T_within_Day=lag_Depress_T-mean(lag_Depress_T, na.rm=TRUE)) %>% 
  ungroup()

#level 2 centering
data_esm <- data_esm %>%
  group_by(day_id) %>%
  mutate(Self_Criticism_T_Day_average= mean(Self_Criticism_T, na.rm=TRUE),
         Exp_Avoidance_T_Day_average= mean(Exp_Avoidance_T, na.rm=TRUE),
         Soc_Anx_T_Day_average= mean(Soc_Anx_T, na.rm=TRUE),
         Depress_T_Day_average= mean(Depress_T, na.rm=TRUE),
         q2_r_Day_average= mean(q2_r, na.rm=TRUE),
         q2_r_social_Day_average= mean(q2_r_social, na.rm=TRUE),
         q2_r_other_Day_average= mean(q2_r_other, na.rm=TRUE),
         lag_Self_Criticism_T_Day_average= mean(lag_Self_Criticism_T, na.rm=TRUE),
         lag_Exp_Avoidance_T_Day_average= mean(lag_Exp_Avoidance_T, na.rm=TRUE),
         lag_Soc_Anx_T_Day_average= mean(lag_Soc_Anx_T, na.rm=TRUE),
         lag_Depress_T_Day_average= mean(lag_Depress_T, na.rm=TRUE)) %>%
  ungroup() 

data_esm <- data_esm%>%
  group_by(participant_id) %>%
  mutate(C_Self_Criticism_T_Day_average_within_Participant=Self_Criticism_T_Day_average-mean(Self_Criticism_T_Day_average, na.rm=TRUE),
         C_Exp_Avoidance_T_Day_average_within_Participant=Exp_Avoidance_T_Day_average-mean(Exp_Avoidance_T_Day_average, na.rm=TRUE),
         C_Soc_Anx_T_Day_average_within_Participant=Soc_Anx_T_Day_average-mean(Soc_Anx_T_Day_average, na.rm=TRUE),
         C_Depress_T_Day_average_within_Participant=Depress_T_Day_average-mean(Depress_T_Day_average, na.rm=TRUE),
         C_q2_r_Day_average_within_Participant=q2_r_Day_average-mean(q2_r_Day_average, na.rm=TRUE),
         C_q2_r_social_Day_average_within_Participant=q2_r_social_Day_average-mean(q2_r_social_Day_average, na.rm=TRUE),
         C_q2_r_other_Day_average_within_Participant=q2_r_other_Day_average-mean(q2_r_other_Day_average, na.rm=TRUE),
         C_time_days_within_Participant=time_days-mean(time_days, na.rm=TRUE), #this is the centered time variable reflecting the day of the reports; we don't need an day average as for the others, because the day is the same for all measurements within a day
         C_lag_Self_Criticism_T_Day_average_within_Participant=lag_Self_Criticism_T_Day_average-mean(lag_Self_Criticism_T_Day_average, na.rm=TRUE),
         C_lag_Exp_Avoidance_T_Day_average_within_Participant=lag_Exp_Avoidance_T_Day_average-mean(lag_Exp_Avoidance_T_Day_average, na.rm=TRUE),
         C_lag_Soc_Anx_T_Day_average_within_Participant=lag_Soc_Anx_T_Day_average-mean(lag_Soc_Anx_T_Day_average, na.rm=TRUE),
         C_lag_Depress_T_Day_average_within_Participant=lag_Depress_T_Day_average-mean(lag_Depress_T_Day_average, na.rm=TRUE)) %>%
  ungroup()

#level 3 centering
data_esm <- data_esm %>%
  group_by(participant_id) %>%
  mutate(Self_Criticism_T_Participant_average= mean(Self_Criticism_T_Day_average, na.rm=TRUE), #calculating the average score for each participant as the average of all the day averages (DA in GMDA bellow) for the participant; this might yield different results from a direct grand mean centering
         Exp_Avoidance_T_Participant_average= mean(Exp_Avoidance_T_Day_average, na.rm=TRUE),
         Soc_Anx_T_Participant_average= mean(Soc_Anx_T_Day_average, na.rm=TRUE),
         Depress_T_Participant_average= mean(Depress_T_Day_average, na.rm=TRUE),
         q2_r_Participant_average= mean(q2_r_Day_average, na.rm=TRUE),
         q2_r_social_Participant_average= mean(q2_r_social_Day_average, na.rm=TRUE),
         q2_r_other_Participant_average= mean(q2_r_other_Day_average, na.rm=TRUE),
         lag_Self_Criticism_T_Participant_average= mean(lag_Self_Criticism_T_Day_average, na.rm=TRUE),
         lag_Exp_Avoidance_T_Participant_average= mean(lag_Exp_Avoidance_T_Day_average, na.rm=TRUE),
         lag_Soc_Anx_T_Participant_average= mean(lag_Soc_Anx_T_Day_average, na.rm=TRUE),
         lag_Depress_T_Participant_average= mean(lag_Depress_T_Day_average, na.rm=TRUE)) %>%
  ungroup()

data_esm <- data_esm%>%
  mutate(C_Self_Criticism_T_Participant_average_GMDA=Self_Criticism_T_Participant_average-mean(Self_Criticism_T_Participant_average, na.rm=TRUE), #we are centering compared to the average of the other participants, as stated above, calculated based on the day averages (this is what DA in GMDA stands for) of each day for each participant
         C_Exp_Avoidance_T_Participant_average_GMDA=Exp_Avoidance_T_Participant_average-mean(Exp_Avoidance_T_Participant_average, na.rm=TRUE),
         C_Soc_Anx_T_Participant_average_GMDA=Soc_Anx_T_Participant_average-mean(Soc_Anx_T_Participant_average, na.rm=TRUE),
         C_Depress_T_Participant_average_GMDA=Depress_T_Participant_average-mean(Depress_T_Participant_average, na.rm=TRUE),
         C_q2_r_Participant_average_GMDA=q2_r_Participant_average-mean(q2_r_Participant_average, na.rm=TRUE),
         C_q2_r_social_Participant_average_GMDA=q2_r_social_Participant_average-mean(q2_r_social_Participant_average, na.rm=TRUE),
         C_q2_r_other_Participant_average_GMDA=q2_r_other_Participant_average-mean(q2_r_other_Participant_average, na.rm=TRUE),
         C_lag_Self_Criticism_T_Participant_average_GMDA=lag_Self_Criticism_T_Participant_average-mean(lag_Self_Criticism_T_Participant_average, na.rm=TRUE),
         C_lag_Exp_Avoidance_T_Participant_average_GMDA=lag_Exp_Avoidance_T_Participant_average-mean(lag_Exp_Avoidance_T_Participant_average, na.rm=TRUE),
         C_lag_Soc_Anx_T_Participant_average_GMDA=lag_Soc_Anx_T_Participant_average-mean(lag_Soc_Anx_T_Participant_average, na.rm=TRUE),
         C_lag_Depress_T_Participant_average_GMDA=lag_Depress_T_Participant_average-mean(lag_Depress_T_Participant_average, na.rm=TRUE))

#Grand mean centering
data_esm <- data_esm%>%
  mutate(C_BDI_TOTAL_pre_GM=BDI_TOTAL_pre-mean(BDI_TOTAL_pre, na.rm=TRUE), #trait characteristics can only be GM centered
         C_LSAS_TOTAL_pre_GM=LSAS_TOTAL_pre-mean(LSAS_TOTAL_pre, na.rm=TRUE),
         C_YSQ_D1_pre_GM=YSQ_D1_pre-mean(YSQ_D1_pre, na.rm=TRUE),
         C_YSQ_D2_pre_GM=YSQ_D2_pre-mean(YSQ_D2_pre, na.rm=TRUE),
         C_q2_r_GM=q2_r-mean(q2_r, na.rm=TRUE), #we also grand mean centered (GM not GMDA) the presence and types of negative events because we might want to work with a single level variable, in which case we need a centered version
         C_q2_r_social_GM=q2_r_social-mean(q2_r_social, na.rm=TRUE),
         C_q2_r_other_GM=q2_r_other-mean(q2_r_other, na.rm=TRUE),
         C_time_intervals_GM=time_intervals-mean(time_intervals, na.rm=TRUE), #we are also GM centering the time variables
         C_time_days_GM=time_days-mean(time_days, na.rm=TRUE),
         C_Self_Criticism_T_GM=Self_Criticism_T-mean(Self_Criticism_T, na.rm=TRUE),  #we also GM (not GMDA) contemporaneous and lagged exp. avoidance, criticism, depression, and soc. anxiety
         C_Exp_Avoidance_T_GM=Exp_Avoidance_T-mean(Exp_Avoidance_T, na.rm=TRUE),
         C_Soc_Anx_T_GM=Soc_Anx_T-mean(Soc_Anx_T, na.rm=TRUE),
         C_Depress_T_GM=Depress_T-mean(Depress_T, na.rm=TRUE),
         C_lag_Self_Criticism_T_GM=lag_Self_Criticism_T-mean(lag_Self_Criticism_T, na.rm=TRUE), 
         C_lag_Exp_Avoidance_T_GM=lag_Exp_Avoidance_T-mean(lag_Exp_Avoidance_T, na.rm=TRUE),
         C_lag_Soc_Anx_T_GM=lag_Soc_Anx_T-mean(lag_Soc_Anx_T, na.rm=TRUE),
         C_lag_Depress_T_GM=lag_Depress_T-mean(lag_Depress_T, na.rm=TRUE))
##


##save files
write.csv(data1,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Processed.csv", row.names=F) #this is the file after processing data, before centering and generating lag variables; change the file location accordingly
write.csv(data_esm,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Data_v4.csv", row.names=F) #this is the file that will contain the data used for the analysis; change the file location accordingly

data4<-subset(data1, number_reports>1) #this is the data that is going to be used in Mplus
data4$time<-data4$time_intervals #this is because we need another time variable in Mplus
data4 <-subset(data4, select = -c(start_date, end_date, time_scheduled, start_day, start_of_study, end_day, day_participant, duration, finished, within_id, q2_other, start_time_scheduled, end_time_scheduled, begin_date_time, begin_date_time_within_day, reference_date_time, reference_day, reference_date_time_within_day, difference_completion_schedule, check))
data5<-subset(data4, select = -c(response_id, report_id, q2, q3, q4, response_within_day_participant, time_intervals, time_intervals_intraday)) #we are preparing the data that will be the aggregated Mplus file; the NA values should not be converted to an arbitrary value before the aggregation, as the conversion might affect the aggregation itself
data5$time<-data5$time_days #similar to data4, we need another time variable in Mplus
data6<-data4 #the Mplus data will have replaced NA values; before that, we are generating and additional data frame, in order to keep the original for verification purposes
data6[is.na(data6)] <- 9999 #Mplus can't work with NA so we are converting these to an arbitrary numerical value;
write.csv(data6,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Data_v4_Mplus.csv", row.names=F) #this is the Mplus data, but keeping the column names for tracking purposes; change the file location accordingly
write.table(data6,"/Users/silviumatu/Desktop/R/Studiu Expwell/Data_v4_Mplus_R.csv", sep=',', row.names=F, col.names=F) #this is the file to be used in Mplus; change the file location accordingly

#aggregated file for Mplus
data5<-ddply(data5,.(day_id),numcolwise(mean), na.rm=TRUE) #here we aggregate the data
data5_clean<-subset(data5, number_days>1) #Mplus can't use participants that have just one single report in a cross lagged model
data7<-data5_clean #we are generating an additional data frame before replacing missing values for verification purposes
data7[is.na(data7)] <- 9999 #convert the missing data, if any
write.csv(data7,file='/Users/silviumatu/Desktop/R/Studiu Expwell/Data_v4_Mplus_agreg.csv', row.names=F) #the aggregated file with the column names
write.table(data7,'/Users/silviumatu/Desktop/R/Studiu Expwell/Data_v4_Mplus_R_agreg.csv', sep=',', row.names=F, col.names=F) #the aggregated file without the column names
print(cor(data7[,c('q5','q6','q7','q8','q9','q10','q11','q12','q13','q14','q15','q16')])) #this is a correlation matrix for the items in the ESM survey; it was necessary for indentifying some issues in the ML-CFA

#some descriptives
data5$average_reports_day<-data5$number_reports/data5$number_days #here we compute the average number of reports per participants per day, based on the aggregated data
mean(data5$average_reports) #and here we report the descriptive statistics for the number of reports per day per participant
sd(data5$average_reports)
data8<-ddply(data_esm,.(participant_id),numcolwise(mean), na.rm=TRUE) #we generate an aggregate file here for descriptive statistics
mean(data8$age) #mean age and other descriptives below
sd(data8$age)
table(data8$gender)
#descriptives for ESM data; generated on the complete data set without aggregation accross participants
mean(data_esm$Depress_T)
mean(data_esm$Soc_Anx_T)
mean(data_esm$Self_Criticism_T)
mean(data_esm$Exp_Avoidance_T)
sd(data_esm$Depress_T)
sd(data_esm$Soc_Anx_T)
sd(data_esm$Self_Criticism_T)
sd(data_esm$Exp_Avoidance_T)
min(data_esm$Depress_T)
min(data_esm$Soc_Anx_T)
min(data_esm$Self_Criticism_T)
min(data_esm$Exp_Avoidance_T)
max(data_esm$Depress_T)
max(data_esm$Soc_Anx_T)
max(data_esm$Self_Criticism_T)
max(data_esm$Exp_Avoidance_T)
table(data_esm$q2_r)/nrow(data_esm) #this is the frequency of negative events


#this calls the arsenal function to compare the complete data with an intermediary file that we compiled for OSF; bellow we remove all non necesary columns form our file
summary(comparedf(data_esm, esm_data))


#exporting data for OSF
data9 <-subset(data2, select = -c(BDI_TOTAL_post, YSQ_D1_post, YSQ_D2_post, LSAS_ANX_post, LSAS_AVOID_post, LSAS_TOTAL_post))
data10 <-subset(data_esm, select = -c(start_date,
                                      end_date,
                                      time_scheduled,
                                      start_day,
                                      start_of_study,
                                      end_day,
                                      duration,
                                      finished,
                                      within_id,
                                      q2_other,
                                      start_time_scheduled,
                                      end_time_scheduled,
                                      begin_date_time,
                                      begin_date_time_within_day,
                                      reference_date_time,
                                      reference_day,
                                      reference_date_time_within_day,
                                      difference_completion_schedule,
                                      check,
                                      C_Self_Criticism_T_within_Day,
                                      C_Exp_Avoidance_T_within_Day,
                                      C_Soc_Anx_T_within_Day,
                                      C_Depress_T_within_Day,
                                      C_q2_r_within_Day,
                                      C_q2_r_social_within_Day,
                                      C_q2_r_other_within_Day,
                                      Self_Criticism_T_Day_average,
                                      Exp_Avoidance_T_Day_average,
                                      Soc_Anx_T_Day_average,
                                      Depress_T_Day_average,
                                      q2_r_Day_average,
                                      q2_r_social_Day_average,
                                      q2_r_other_Day_average,
                                      C_Self_Criticism_T_Day_average_within_Participant,
                                      C_Exp_Avoidance_T_Day_average_within_Participant,
                                      C_Soc_Anx_T_Day_average_within_Participant,
                                      C_Depress_T_Day_average_within_Participant,
                                      C_q2_r_Day_average_within_Participant,
                                      C_q2_r_social_Day_average_within_Participant,
                                      C_q2_r_other_Day_average_within_Participant,
                                      Self_Criticism_T_Participant_average,
                                      Exp_Avoidance_T_Participant_average,
                                      Soc_Anx_T_Participant_average,
                                      Depress_T_Participant_average,
                                      q2_r_Participant_average,
                                      q2_r_social_Participant_average,
                                      q2_r_other_Participant_average,
                                      C_Self_Criticism_T_Participant_average_GMDA,
                                      C_Exp_Avoidance_T_Participant_average_GMDA,
                                      C_Soc_Anx_T_Participant_average_GMDA,
                                      C_Depress_T_Participant_average_GMDA,
                                      C_q2_r_Participant_average_GMDA,
                                      C_q2_r_social_Participant_average_GMDA,
                                      C_q2_r_other_Participant_average_GMDA,
                                      C_q2_r_GM,
                                      C_time_intervals_GM,
                                      C_time_days_GM,
                                      C_Self_Criticism_T_GM,
                                      C_Exp_Avoidance_T_GM,
                                      C_Soc_Anx_T_GM,
                                      C_Depress_T_GM,
                                      C_lag_Self_Criticism_T_GM,
                                      C_lag_Exp_Avoidance_T_GM,
                                      C_lag_Soc_Anx_T_GM,
                                      C_lag_Depress_T_GM))
write.csv(data9,file='/Users/silviumatu/Desktop/R/Studiu Expwell/Trait_data.csv', row.names=F)
write.csv(data10,file='/Users/silviumatu/Desktop/R/Studiu Expwell/ESM_data.csv', row.names=F)
write.csv(data6,file="/Users/silviumatu/Desktop/R/Studiu Expwell/Mplus_data_with_column_names.csv", row.names=F) 
write.table(data6,"/Users/silviumatu/Desktop/R/Studiu Expwell//Mplus_data.csv", sep=',', row.names=F, col.names=F) 
##




### running analysis ###
##reading data file for analysis
data_esm <- read.csv ("/Users/silviumatu/Desktop/R/Studiu Expwell/Data_v4.csv") #read this file to run the analysis, jumping over the processing
##


##MLM analysis
#analysis for social anxiety
#reported model
model_Soc_Anx <- lmer(Soc_Anx_T ~ C_time_intervals_intraday_within_Day + C_time_days_within_Participant +
                             C_q2_r_social_GM + C_q2_r_other_GM +
                             C_lag_Self_Criticism_T_within_Day + C_lag_Self_Criticism_T_Day_average_within_Participant + C_lag_Self_Criticism_T_Participant_average_GMDA +
                             C_lag_Exp_Avoidance_T_within_Day + C_lag_Exp_Avoidance_T_Day_average_within_Participant + C_lag_Exp_Avoidance_T_Participant_average_GMDA +
                             C_lag_Depress_T_within_Day + C_lag_Depress_T_Day_average_within_Participant + C_lag_Depress_T_Participant_average_GMDA +
                             (1|participant_id/day_participant)+(0+C_lag_Depress_T_Day_average_within_Participant+C_lag_Self_Criticism_T_Day_average_within_Participant+C_lag_Exp_Avoidance_T_Day_average_within_Participant|participant_id), data_esm, REML = TRUE)

summary(model_Soc_Anx)
table_model_Soc_Anx<-tab_model(model_Soc_Anx,  show.se = TRUE)
table_model_Soc_Anx
#confidence_intervals_Soc_Anx<-confint(model_Soc_Anx) #runs very slow
#confidence_intervals_Soc_Anx

#analysis for depression
#reported model
model_Depress <- lmer(Depress_T ~ C_time_intervals_intraday_within_Day + C_time_days_within_Participant +
                             C_q2_r_social_GM + C_q2_r_other_GM +
                             C_lag_Self_Criticism_T_within_Day + C_lag_Self_Criticism_T_Day_average_within_Participant + C_lag_Self_Criticism_T_Participant_average_GMDA +
                             C_lag_Exp_Avoidance_T_within_Day + C_lag_Exp_Avoidance_T_Day_average_within_Participant + C_lag_Exp_Avoidance_T_Participant_average_GMDA +
                             C_lag_Soc_Anx_T_within_Day + C_lag_Soc_Anx_T_Day_average_within_Participant + C_lag_Soc_Anx_T_Participant_average_GMDA +
                             C_BDI_TOTAL_pre_GM+
                             (1|participant_id/day_participant)+(0+C_lag_Self_Criticism_T_Day_average_within_Participant+C_lag_Exp_Avoidance_T_Day_average_within_Participant+C_lag_Soc_Anx_T_Day_average_within_Participant|participant_id), data_esm, REML = TRUE)
summary(model_Depress)
table_model_Depress<-tab_model(model_Depress,  show.se = TRUE)
table_model_Depress
#confidence_intervals_Depress<-confint(model_Depress) runs very slow
#confidence_intervals_Depress
##



