#prepping and cleaning PLAY project data for annual report

#connect to data
Staging <- odbcConnect("BI01", uid = "randd", pwd = "Mhcd6770")
CE <- sqlQuery(Staging, 'SELECT * FROM [Staging].[sp].[ConsultEducate] WHERE CEServiceProgram = 728 ')
TrainSurveys <- sqlQuery(Staging, 'SELECT * FROM [DataEntry].[play].[Survey]')
DECA <- read.csv('DECA_7.16.19.csv', header = TRUE)

##initial CE data cleaning
CE$CEDateofService <- as.Date(CE$CEDateofService)
FY20 <- interval("2019-07-01", "2020-06-30")
FY19 <- interval("2018-07-01", "2019-06-30")
FY18 <- interval("2017-07-01", "2018-06-30")
FY17 <- interval("2016-07-01", "2017-06-30")
FY16 <- interval("2015-07-01", "2016-06-30")
CE$GrantYear <- with(
  CE, ifelse(CEDateofService %within% FY20, 'FY20',
             ifelse(CEDateofService %within% FY19, 'FY19',
                    ifelse(CEDateofService %within% FY18, 'FY18',
                           ifelse(CEDateofService %within% FY17, 'FY17',
                                  ifelse(CEDateofService %within% FY16, 'FY16',"Old")))))
)
# change GrantYear to factor
CE$GrantYear <- as.factor(CE$GrantYear)



##initial Training data cleaning
# Training Survey Data - bring data in, then add columns for FYs
# using date intervals already created with lubridate 
TrainSurveys$Date <- as.Date(TrainSurveys$Date)
TrainSurveys$GrantYear <- with(
  TrainSurveys, ifelse(Date %within% FY20, 'FY20',
                       ifelse(Date %within% FY19, 'FY19',
                              ifelse(Date %within% FY18, 'FY18',
                                     ifelse(Date %within% FY17, 'FY17',
                                            ifelse(Date %within% FY16, 'FY16',"Old")))))
)

#Rename Activity/Topic column
TrainSurveys <- TrainSurveys %>%
  rename(Activity = `Activity/Topic`)
# recode training names that don't match - all 7/5/18 should be "Breathing, Rituals, and Perspective Shifting"
library(plyr)
TrainSurveys$Activity <- mapvalues(TrainSurveys$Activity, from = c("Breathing, Rituals, and","Breating, Rituals, and Perspective Shifting
                                                                   ",
                                                                   "Breathing, Rituals, and Perspective Shifting  ", 
                                                                   "Breathing, Rituals, and PErspective Shifting"), 
                                   to = c("Breathing, Rituals, and Perspecitve Shifting","Breathing, Rituals, and Perspecitve Shifting",
                                          "Breathing, Rituals, and Perspecitve Shifting","Breathing, Rituals, and Perspecitve Shifting"))
TrainSurveys$Activity[587] = 'Breathing, Rituals, and Perspective Shifting'
detach("package:plyr", unload=TRUE)

# Change GrantYear to factor 
TrainSurveys$GrantYear <- as.factor(TrainSurveys$GrantYear)

#re-factor "Position"
levels(TrainSurveys$Position)[levels(TrainSurveys$Position)=="Teacher "] <- "Teacher"

#recoding 0s as NA
TrainSurveys[TrainSurveys == 0] <- NA

#need only the trainings provided by Piton-funded consultants
library(stringr)
TrainSurveys <- TrainSurveys %>%
  filter(str_detect(Consultant, "Na|Jennie|Trish|Hilary"))

#Percent agree/strongly agree with statements on training eval. First, need longform
library(tidyr)
TrainSurveysLong <- TrainSurveys %>%
  gather("Question", "Response", 8:16)
TrainSurveysLong$Response <- as.integer(TrainSurveysLong$Response)
#now add new variable for response category (3s and 4s are Agree, 1s and 2s are Disagree)
TrainSurveysLong$ResponseCategory <- ifelse(TrainSurveysLong$Response > 2, 'Agree', ifelse(TrainSurveysLong$Response < 3, 'Disagree', NA))

#teacher surveys proportion agree - for reference and calling later
TeacherResponseSummary <- TrainSurveysLong %>% 
  filter(GrantYear=='FY19', Position=='Teacher', !is.na(ResponseCategory)) %>%
  group_by(Question, ResponseCategory) %>%
  summarise(n = n()) %>%
  mutate(Proportion = n / sum(n))

TeacherResponseAgree <- TeacherResponseSummary %>%
  filter(ResponseCategory=='Agree')

#Parent surveys proportion agree - for reference and calling later
ParentResponseSummary <- TrainSurveysLong %>% 
  filter(GrantYear=='FY19', Position=='Parent', !is.na(ResponseCategory)) %>%
  group_by(Question, ResponseCategory) %>%
  summarise(n = n()) %>%
  mutate(Proportion = n / sum(n))

ParentResponseAgree <- ParentResponseSummary %>%
  filter(ResponseCategory=='Agree')

#might need data in wide form for pre/post analysis, definitely need to restrict columns
DECAanalysis <- DECA %>%
  select(SiteName, RatingYear, ChildId, RaterType, RatingPeriod, TpfRaw, TpfDescription) %>%
  filter(RatingYear=='2018-2019', SiteName %in% c('Lowry', 'Sun Valley', 'Rude Park', 'DCCA', 'Edna Oliver'),
         RatingPeriod %in% c('P', 'T'))

DECAraw <- DECAanalysis %>%
  select(-TpfDescription) %>%
  spread(RatingPeriod, TpfRaw)

DECAraw$Change <- DECAraw$`T`-DECAraw$P
DECAraw$ChangeCat <- if_else(DECAraw$Change>0,'Positive', 
                             if_else(DECAraw$Change<0,'Negative','No Change'))

DECAcat <- DECAanalysis %>%
  select(-TpfRaw) %>%
  spread(RatingPeriod, TpfDescription)
