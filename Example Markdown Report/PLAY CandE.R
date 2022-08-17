library(RODBC)
Staging <- odbcConnect("BI01", uid = "randd", pwd = "Mhcd6770")
CE <- sqlQuery(Staging, 'SELECT * FROM [Staging].[sp].[ConsultEducate]')

library(dplyr)
#only want PLAY program-specific C&Es so need to filter on Service Program = 728
PLAY_CE <- CE %>%
  filter(CEServiceProgram == '728')

#create new variable for grant year of service (grant year starts Aug 1)
#first, change date column to date format
PLAY_CE$CEDateofService <- as.Date(PLAY_CE$CEDateofService)
library(lubridate)
FY20 <- interval("2019-08-01", "2020-07-31")
FY19 <- interval("2018-08-01", "2019-07-31")
FY18 <- interval("2017-08-01", "2018-07-31")
FY17 <- interval("2016-08-01", "2017-07-31")
FY16 <- interval("2015-08-01", "2016-07-31")

PLAY_CE$GrantYear <- with(PLAY_CE, ifelse(CEDateofService %within% FY20, 'FY20',
                                          ifelse(CEDateofService %within% FY19, 'FY19',
                                          ifelse(CEDateofService %within% FY18, 'FY18',
                                                 ifelse(CEDateofService %within% FY17, 'FY17',
                                                        ifelse(CEDateofService %within% FY16, 'FY16',"Old")))))
                                          )
#change GrantYear to factor
PLAY_CE$GrantYear <- as.factor(PLAY_CE$GrantYear)

#summarizing number of services and number of recipients per year by staff
PLAY_CE %>%
  group_by(GrantYear, CEStaffName) %>%
  summarise(sum(CENumberofRecipients), sum(CEDurationofService)) %>%

#summarizing number of services and number of recipients per year by location
PLAY_CE %>%
  group_by(GrantYear, CEECCECenter) %>%
  summarise(sum(CENumberofRecipients), sum(CEDurationofService))

#plot number of services by staff over time
library(ggplot2)
PLAY_CE %>%
  group_by(GrantYear, CEStaffName) %>%
  ggplot() + geom_col(mapping = aes(CEStaffName, CENumberofRecipients, fill=GrantYear)) 




##**Training Survey Data**##
#bring data in, then add columns for FYs
Staging <- odbcConnect("BI01", uid = "randd", pwd = "Mhcd6770")
TrainSurveys <- sqlQuery(Staging, 'SELECT * FROM [adhoc].[play].[Survey]')

#using date intervals already created with lubridate
TrainSurveys$Date <- as.Date(TrainSurveys$Date)
TrainSurveys$GrantYear <- with(TrainSurveys, ifelse(Date %within% FY20, 'FY20',
                                          ifelse(Date %within% FY19, 'FY19',
                                                 ifelse(Date %within% FY18, 'FY18',
                                                        ifelse(Date %within% FY17, 'FY17',
                                                               ifelse(Date %within% FY16, 'FY16',"Old")))))
)
#change GrantYear to factor
TrainSurveys$GrantYear <- as.factor(TrainSurveys$GrantYear)
#need to re-factor "Position"
levels(TrainSurveys$Position)[levels(TrainSurveys$Position)=="Teacher "] <- "Teacher"
#recode likert 0 as N/A
colnames(TrainSurveys)
TrainSurveys[TrainSurveys == 0] <- NA

#Percent agree/strongly agree with statements on training eval
#First, need longform
colnames(TrainSurveys)
library(tidyr)
TrainSurveysLong <- TrainSurveys %>%
  gather("Question", "Response", 8:16)
TrainSurveysLong$Response <- as.integer(TrainSurveysLong$Response)
#now add new variable for response category (3s and 4s are Agree, 1s and 2s are Disagree)
TrainSurveysLong$ResponseCategory <- ifelse(TrainSurveysLong$Response > 2, 'Agree', ifelse(TrainSurveysLong$Response < 3, 'Disagree', NA))

TeacherResponseSummary <- TrainSurveysLong %>% 
  filter(GrantYear=='FY19', Position=='Teacher', !is.na(ResponseCategory)) %>%
  group_by(Question, ResponseCategory) %>%
  summarise(n = n()) %>%
  mutate(Proportion = n / sum(n))

TeacherResponseAgree <- TeacherResponseSummary %>%
  filter(ResponseCategory=='Agree')

#Parent surveys proportion agree
ParentResponseSummary <- TrainSurveysLong %>% 
  filter(GrantYear=='FY19', Position=='Parent', !is.na(ResponseCategory)) %>%
  group_by(Question, ResponseCategory) %>%
  summarise(n = n()) %>%
  mutate(Proportion = n / sum(n))

ParentResponseAgree <- ParentResponseSummary %>%
  filter(ResponseCategory=='Agree')



##***Running code to insert into Markdown***###

Recip2018 <- PLAY_CE %>%
  filter(GrantYear=='FY19') %>%
  summarise(sum(CENumberofRecipients))
format(Recip2018[1,1],big.mark=",", trim=TRUE)

TeachTrain <- TrainSurveys %>%
  filter(GrantYear=='FY19', Position=='Teacher') %>%
  summarise(Freq = n())
format(TeachTrain[1,1],big.mark=",",trim=TRUE)

ParentTrain <- TrainSurveys %>%
  filter(GrantYear=='FY19', Position=='Parent') %>%
  summarise(Freq = n())
format(TeachTrain[1,1],big.mark=",",trim=TRUE)

min <- TrainSurveys %>%
  filter(GrantYear=='FY19', Position=='Parent') %>%
  group_by(Date) %>%
  summarise(Freq = n())
min(min$Freq)

#Table of teacher training topics by center with count of teachers present
TrainSurveys %>%
  filter(GrantYear=='FY19') %>%
  group_by(Location, `Activity/Topic`) %>%
  summarise(Teachers = n())


#Percentage of teachers who agree with statements
sprintf("%.1f%%", 100*mean(TeacherResponseAgree$Proportion))

sprintf("%.1f%%", 100*mean(ParentResponseAgree$Proportion))












