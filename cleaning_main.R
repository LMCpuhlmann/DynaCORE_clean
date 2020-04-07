# pipeline template to clean and structure data from the DynaCORE project for analysis
# contributors:
# Matthias Zerban (matthias.zerban@unimedizin-mainz.de)
# Lara Puhlmann (puhlmann@cbs.mpg.de)
# Jeroen Weermeijer (jeroen.weermeijer@kuleuven.be)
# Haakon Engen



#
#WARNING: Only run once! 
#



rm(list = ls())
require(foreign)
require(plyr)
require(dplyr)
require(stringr)
require(BBmisc)
require(stringr)
require(formattable)
require(Hmisc)
require(corrplot)
# run the functions 'rename.R', 'formatting.R', ... or source:
# source("/.../DynaCORE_clean/rename.R")
# source("/.../DynaCORE_clean/formatting.R")

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# load data and add column indicating the origin of the data
data_en = read.csv("DynaCORE_test_answer_number.csv", sep = ",", stringsAsFactors = FALSE)
#data_test = read.csv("DynaCORE_test_data_firstround.csv", sep = ",", stringsAsFactors = FALSE)

data_en$survey_country = as.factor("en")

data_en = rename(data_en)
data_en = formatting(data_en) #group occupation + status in lists


################### general cleaning ########################

# remove rows without respondent ID
xx = which(is.na(data_en$Respondent.ID))
data_en = data_en[-xx,]

########## combine files from multiple languages

# data_xx = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xx$survey_country = as.factor("xx")
# 
# data_xy = read.csv("DynaCORE_test_data_xx.csv", sep = ",", stringsAsFactors = FALSE)
# data_xy$survey_country = as.factor("xy")
# data_all = rbind(data_en, data_xx, data_xy)


#################### covariates: plausibility checks & basic formatting ########################
data_en[,c(1:2, 10:12,14:16, 18:19, 53:54, 58:59, 60:61,64)] <- lapply(data_en[,c(1:2, 10:12,14:16, 18:19, 53:54, 58:59, 60:61,64)], as.factor)

data_en$household.income = factor(data_en$household.income, order = TRUE)
data_en$health.status = factor(data_en$health.status, order = TRUE)
data_en$symptom.severity = factor(data_en$symptom.severity, order = TRUE)

data_en$people.in.household.under.18 = as.numeric(data_en$people.in.household.under.18)
data_en$opinion.about.authorities.measures = as.numeric(data_en$opinion.about.authorities.measures)
data_en$adherence.to.recommended.procedures = as.numeric(data_en$adherence.to.recommended.procedures)

##### people.in.household as continuous ####
data_en$people.in.household.cont = as.numeric(data_en$people.in.household)
data_en$people.in.household.cont[which(data_en$people.in.household.cont == 3)] = 3.5
data_en$people.in.household.cont[which(data_en$people.in.household.cont == 4)] = 5.5
xx = numextract(data_en$X.28)
data_en$people.in.household.cont[which(data_en$people.in.household.cont == 5)] = as.numeric(xx)

###### date and completion time ########

#split weird month-day-year date + time col into two col, one with the weird date format, one with correct time
for(i in 1:length(data_en$Respondent.ID)){
  start = strsplit(data_en$Start.Date[i], " ")
  data_en$Start.Date[i] = start[[1]][1]
  data_en$Start.Time[i] = paste(start[[1]][2], start[[1]][3])
  
  end = strsplit(data_en$End.Date[i], " ")
  data_en$End.Date[i] = end[[1]][1]
  data_en$End.Time[i] = paste(end[[1]][2], end[[1]][3])
}

#line of code that deals with different separators that occur in surveymonkey raw data for date outputs (e.g. mm/dd/yyyy vs. mm.dd.yyyy).
data_en$Start.Date = gsub(".", "/", data_en$Start.Date, fixed=TRUE) #mm.dd.yyyy becomes mm/dd/yyyy

#convert month-day-year to year-month-day date, then to POSIXlt
data_en$Start.Date = as.Date(data_en$Start.Date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
data_en$End.Date = as.Date(data_en$End.Date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
data_en$Start.Date = as.POSIXlt(paste(data_en$Start.Date), tz = "Europe/Berlin", format="%Y-%m-%d")
data_en$End.Date = as.POSIXlt(paste(data_en$End.Date), tz = "Europe/Berlin", format="%Y-%m-%d")
data_en$Start.DateTime = as.POSIXlt(paste(data_en$Start.Date, data_en$Start.Time), tz = "Europe/Berlin", format="%Y-%m-%d %H:%M:%S %p")
data_en$End.DateTime = as.POSIXlt(paste(data_en$End.Date, data_en$End.Time), tz = "Europe/Berlin", format="%Y-%m-%d %H:%M:%S %p")

# #test
# data_en$Start.Date[4] #should give year/month/day GMT
# data_en$End.Date[4] #should give year/month/day GMT
# data_en$Start.DateTime[4] #should give year/month/day hour/minutes/seconds GMT
# data_en$End.DateTime[4] #should give year/month/day hour/minutes/seconds GMT

#completion time
data_en$completionTime = difftime(data_en$End.DateTime, data_en$Start.DateTime)
# #test
# data_en$completionTime[3] #gives time difference in mins

data_en$completionTime <- as.numeric(data_en$completionTime, units="secs")

# #test
# data_en$completionTime[3] #returns numeric, equals time in seconds (1 minute=60 seconds)



###### date of Corona test #####
data_en$infection.test.status.date = gsub(".", "/", data_en$infection.test.status.date, fixed=TRUE)#mm.dd.yyyy becomes mm/dd/yyyy
data_en$infection.test.status.date[which(nchar(data_en$infection.test.status.date)<5)]=NA # set all that are not a date to NA
#convert month-day-year to year-month-day date
data_en$infection.test.status.date = as.Date(data_en$infection.test.status.date, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"), optional = FALSE)
#date as POSIXlt
data_en$infection.test.status.date = as.POSIXlt(paste(data_en$infection.test.status.date), tz = "Europe/Berlin", format="%Y-%m-%d")


###### age #####
# # test
# data_en$age[2] = "2o"
# data_en$age[4] = "I am 711 years old"

data_en$age = gsub("o", "0", data_en$age)
# if any ages are 0, this could be due to a leading o
 
# extract the numeric component of free form age response
data_en$age = numextract(data_en$age)
data_en$age = as.numeric(data_en$age)
data_en$age[which(data_en$age > 100)] = NA

###### education #####

# # test
# data_en$years.of.education[2] = "22 years"
# data_en$years.of.education[4] = "5 primary school 10 highschool"

data_en$years.of.education.fulltext = data_en$years.of.education
data_en$years.of.education = numextract(data_en$years.of.education) # extract the numeric component of free form years.of.education response
data_en$years.of.education = as.numeric(data_en$years.of.education)

for(i in 1:length(data_en$Respondent.ID)){
  if (!is.na(data_en$years.of.education[i])) {
    if(data_en$years.of.education[i] > data_en$age[i]){
      data_en$years.of.education[i] = NA
    }
    if(!is.na(data_en$years.of.education[i]) && data_en$years.of.education[i] > 10){
      data_en$years.of.education.fulltext[i] = NA
    }
  }
}

###### current.location ####
## combines the variables "country.of.residence", "current.stay.out.of.town.country" and "currently.away" into a variable "current.location" -> if subjects are away, their their location is the away country location, if they are not, the country of residence location is
##R can default character columns to factors, so first step is to make sure variables of interest are characters
data_en[ , c("current.stay.out.of.town.country" ,"country.of.residence") ] <- sapply( data_en[ , c("current.stay.out.of.town.country" ,"country.of.residence") ] , as.character )
##now we use a simple ifelse statement to create a new variable that gives us currenty location (country)
data_en$current.location <- ifelse(data_en$current.stay.out.of.town == '1', data_en$current.stay.out.of.town.country, data_en$country.of.residence)

# #test
# data_en$country.of.residence[2] #gives Algeria
# data_en$current.stay.out.of.town.country[2] #Gives Andorra
# data_en$current.stay.out.of.town[2] #Gives Yes, so current loc should be Andorra
# data_en$current.location[2] #gives Andorra, hooray            

#### clean-up inconsistent responses ####
# set responses for place of location to NA if away currently was answered with 0
data_en$current.stay.out.of.town.country[which(data_en$current.stay.out.of.town==2)] <- NA
data_en$current.stay.out.of.town.city[which(data_en$current.stay.out.of.town==2)] <- NA

# set cases where more/same people in household are underage than total household to NA
xx = which(data_en$people.in.household.cont+0.5<=data_en$people.in.household.under.18)
data_en$people.in.household[xx] = NA
data_en$people.in.household.cont[xx] = NA
data_en$people.in.household.under.18[xx] = NA

# set cases with mismatch in occulation to NA
data_en$not.working.12 <- lapply(data_en$occupation, function(ch) grep("16", ch))
data_en$not.working.12[sapply(data_en$not.working.12, function(x) length(x)==0)] <- NA

### Employment
employed = c("1", "2", "3", "4")
not.employed = c("7", "8", "9", "10", "12") #parental leave, sick leave, unemployment w/ or w/o benefits, retired
data_en$employed.13 <- lapply(data_en$occupational.status, function(ch) grep(paste(employed, collapse="|"), ch))
data_en$not.employed.13 <- lapply(data_en$occupational.status, function(ch) grep(paste(not.employed, collapse="|"), ch))
data_en$employed.13[sapply(data_en$employed.13, function(x) length(x)==0)] <- NA
data_en$not.employed.13[sapply(data_en$not.employed.13, function(x) length(x)==0)] <- NA

# find people that are unemployed in 12 but working in 13
index = which(!is.na(data_en$not.working.12)) %in% which(!is.na(data_en$employed.13))
xx = which(!is.na(data_en$not.working.12))[index]
data_en$occupation[xx] <- NA
data_en$occupational.status[xx] <- NA

# add 'not working' to occupational status to all individuals listing forms of not working in 13
index = which(is.na(data_en$not.working.12)) %in% which(!is.na(data_en$not.employed.13))
xx = which(is.na(data_en$not.working.12))[index]
data_en$occupational.status[xx][[1]][length(data_en$occupational.status[xx][[1]])+1] <- 16

#  inconsistent corona info
data_en$symptom.severity[which(data_en$infection.test.status==1)] <- NA # set symptom severity rating to NA if they indicated they were not tested pos

# indicate individuals who report COVID symptoms but in stressor exposure said this situation did not happen
data_en$symptom.inconsistency = NA
data_en$symptom.inconsistency[which(data_en$symptom.severity >0 && data_en$CE_01 == 0)] = 1

# indicate individuals who report being in a risk group but in stressor exposure said to risk group "this situation did not happen"
# PLEASE MAKE SURE THAT THE 'risk.group' SCALE ACTUALLY STARTS WITH A 0, such that 1 corresponds to 'yes'. if the scale starts with a 1, then 2 = yes
data_en$risk.group.inconsistency = NA
data_en$risk.group.inconsistency[which(data_en$risk.group == 1 && data_en$CE_04 == 0)] = 1

data_en$CE_04[data_en$risk.group==0]=0 # set risk group stressor to "did not happen" if participants indicated in covariates they were not in a risk group

# I think mental health is wrongly coded as 0 = yes, 1 = no, so recode ONCE ONLY:
 data_en$diagnosed.mental.health = revalue(data_en$diagnosed.mental.health, c("0"="1", "1"="0"))

################### restructure questionnaire variables ########################

data_en[,c(68:154,156:167)] <- lapply(data_en[,c(68:154,156:167)], as.numeric)

# indicate any cases with missings:
# (since we have loads of incomplete data, we will describe them rather than getting rid off them all)
data_en$missings <- rowSums(is.na(data_en[,c(68:154,156:167)]))
dim(data_en[data_en$missings == 0,]) # should be 5000
#data_en <- data_en[data_en$missings == 0,] 

#Mental Health Problems 'P': 
data_en$CM_07 <- 5 - data_en$CM_07
term <- "CM"
GHQ <- grep(term, names(data_en))
GHQrecode <- function(x){recode(x, '1'=0L, '2'=1L, '3'=2L, '4'=3L)}
data_en[GHQ] <- lapply(data_en[GHQ], GHQrecode)
GHQ <- GHQ[1:12]
data_en$P <- rowSums(data_en[GHQ])

#PSS (percieved social support):
term <- "H2_"
PSSindex <- grep(term, names(data_en))
PSSindex <- PSSindex[1:7]
data_en$PSS <- rowSums(data_en[PSSindex])

#Optimism:
data_en$OPT <- as.numeric(data_en$H3_01)

#Perceived general self efficacy:
term <-"H4_"
GSEindex <- grep(term, names(data_en))
data_en$GSE <- rowSums(data_en[GSEindex])

# self-percieved reslience (BRS):
term <- "H5_"
BRS <- grep(term, names(data_en))
BRSrec <- c("H5_02", "H5_04", "H5_06")
data_en[,BRSrec] <- 6 - data_en[,BRSrec]
data_en$REC <- rowMeans(data_en[BRS])

#BFI Neuroticism:
data_en$H6_01 <- 6 - data_en$H6_01
term <- "H6_"
BFI <- grep(term, names(data_en))
BFIrecode <- function(x){recode(x, '1'=-2L, '2'=-1L, '3'=0L, '4'=1L,'5'=2L)}
data_en[BFI] <- lapply(data_en[BFI], BFIrecode)
data_en$NEU <- rowSums(data_en[BFI])

#Behavioral Coping style
term <- "COPE"
COPE <- grep(term, names(data_en))
COPE <- COPE[c(1:5,7:9)]
data_en$BCS <- rowSums(data_en[COPE])

#CERQ
term <- "CERQ"
CERQ <- grep(term, names(data_en))
data_en$CERQSum <- rowSums(data_en[CERQ])

#Positive Appraisal Style:
PAS <- data_en[,c(CERQ, 106, 110 )]
PAS[,c("H1_COPE_18","H1_COPE_28")] <- PAS[,c("H1_COPE_18","H1_COPE_28")]*5/4 #rescale
data_en$PAS <- rowMeans(PAS)

#CORONA specific appraisal:
term <- "H1_Cor_"
PAC <- grep(term, names(data_en))
data_en$PAC <- rowSums(data_en[PAC])

#### calculation of stressors
# SCM = stressor count method
# SSM = stressor severity method

#CORONA Stressors:
term <- "CE_"
CE <- grep(term, names(data_en))
CE <- CE[1:30]
data_en$Es.SCM <- rowSums(data_en[CE] >0) #stressor count
data_en$Es.SSM <- rowSums(data_en[CE])/5 #weighted

#DHs:
term <- "GE_"
GE <- grep(term, names(data_en))
GE <- GE[1:12]
data_en$Eg.SCM <- rowSums(data_en[GE] >0) #stressor count
data_en$Eg.SSM <- rowSums(data_en[GE])/5 #weighted

#combined:
Eall <- c(grep("GE_", names(data_en))[1:12], grep("CE_", names(data_en))[1:30])
data_en$Ec.SCM <- rowSums(data_en[Eall] >0) #stressor count
data_en$Ec.SSM <- rowSums(data_en[Eall])/5 #weighted

# test
which(data_en$Ec.SCM!=rowSums(data_en[ , c("Eg.SCM" ,"Es.SCM")]))
#--> should be 0

##################### SR Score ###################

#adapted from Haakon's script

m1 <- summary(lm(scale(P)~scale(Eg.SCM),data= data_en[!is.na(data_en$Eg.SCM)]))
data_en$SR_Eg.SCM[!is.na(data_en$Eg.SCM)] <-as.numeric(scale(resid(m1)))

m2 <- summary(lm(scale(P)~scale(Es.SCM),data= data_en[!is.na(data_en$Es.SCM)]))
data_en$SR_Es.SCM[!is.na(data_en$Es.SCM)] <-as.numeric(scale(resid(m2)))

m3 <- summary(lm(scale(P)~scale(Ec.SCM),data= data_en))
data_en$SR_c.SCM <-as.numeric(scale(resid(m3)))

## do the same with severity ratings?
m4 <- summary(lm(scale(P)~scale(Eg.SSM),data= data_en))
data_en$SR_Eg.SSM <-as.numeric(scale(resid(m4)))

m5 <- summary(lm(scale(P)~scale(Es.SSM),data= data_en))
data_en$SR_Es.SSM <-as.numeric(scale(resid(m5)))

m6 <- summary(lm(scale(P)~scale(Ec.SSM),data= data_en))
data_en$SR_c.SSM <-as.numeric(scale(resid(m6)))
######################## subgroup selection ######################## 

##### select subjects FROM Europe
Europe = c(2, 4, 9, 11, 12, 17, 18, 23, 28, 45, 47, 48, 51, 60, 63, 64, 67, 68, 70, 77, 80, 81, 86, 88, 98, 103, 104, 105, 111, 117, 119, 127, 132, 142, 143, 146, 147, 154, 158, 162, 163, 168, 174, 175, 180, 191, 193)
# for now, the above list does not include Russia (148), Kasakhstan (92) & Turkey (186), since they are trans-continental
# NOTE: List of countries migh change when Kosovo is included!

xx = which(data_en$country.of.residence %in% Europe)
data_en$from.eu = 0
data_en$from.eu[xx] = 1
data_en$from.eu = as.factor(data_en$from.eu)

##### select subjects IN Europe
xx = which(data_en$current.location %in% Europe)
data_en$in.eu = 0
data_en$in.eu[xx] = 1
data_en$in.eu = as.factor(data_en$in.eu)

# example of subgroup indices:

# index people who work in at risk jobs?

# index people with potentially precarious job conditions: freelancer, self-employed, temp contract, unemployed, by excluding everyone with a stable status
stable.occupational.status = c("1", "3", "7", "8", "11", "12")
insecure.occulational.status = c("2", "4", "5", "6", "9", "10")

index.stable.occupational.status <- lapply(data_en$occupational.status, function(ch) grep(paste(stable.occupational.status, collapse="|"), ch))
index.stable.occupational.status[sapply(index.stable.occupational.status, function(x) length(x)==0)] <- NA

index.insecure.occupational.status <- lapply(data_en$occupational.status, function(ch) grep(paste(insecure.occulational.status, collapse="|"), ch))
index.insecure.occupational.status[sapply(index.insecure.occupational.status, function(x) length(x)==0)] <- NA

data_en$unstable.occupational.status = NA 
data_en$unstable.occupational.status[which(!is.na(index.stable.occupational.status))] = 0 # 0 = no, stable status
data_en$unstable.occupational.status[which(is.na(index.stable.occupational.status)&!is.na(index.insecure.occupational.status))] = 1 # 1 = yes, unstable status (and no additional stable status)

##################### identify subjects to exclude ##############

# exclude subjects under 18
data_en$Respondent.ID[which(data_en$age < 18)]<- NA

# exclude subjects with mental health conditions?

Europe = c(2, 4, 9, 11, 12, 17, 18, 23, 28, 45, 47, 48, 51, 60, 63, 64, 67, 68, 70, 77, 80, 81, 86, 88, 98, 103, 104, 105, 111, 117, 119, 127, 132, 142, 143, 146, 147, 154, 158, 162, 163, 168, 174, 175, 180, 191, 193)
# for now, the above list does not include Russia (148), Kasakhstan (92) & Turkey (186), since they are trans-continental

##### select subjects FROM Europe
#xx = which(data_en$country.of.residence %in% Europe)

##### select subjects IN Europe
#xx = which(data_en$current.location %in% Europe)

data_eu = data_en[xx,]


### exclude subjects with no response variance (check block-wise for all questionnaires with more than 2 items)
var = matrix(NA, nrow = length(data_en$Respondent.ID), ncol = 8)
for (i in 1:nrow(data_en)){ 
  var[i,1] = (var(as.vector(as.matrix(data_en[i, GHQ])))) 
  var[i,2] = (var(as.vector(as.matrix(data_en[i, PSSindex])))) 
  var[i,3] = (var(as.vector(as.matrix(data_en[i, GSEindex])))) #object asku not found
  var[i,4] = (var(as.vector(as.matrix(data_en[i, BRS])))) 
  var[i,5] = (var(as.vector(as.matrix(data_en[i, COPE])))) 
  var[i,6] = (var(as.vector(as.matrix(data_en[i, CERQ])))) 
  var[i,7] = (var(as.vector(as.matrix(data_en[i, CE])))) 
  var[i,8] = (var(as.vector(as.matrix(data_en[i, GE])))) 
}

data_en$response_variance = rowSums(var)
#add variance as an additional column in the data frame
#data_en$variance_Q1 <- apply(data_en,1,function(row) var(as.vector(row[5:10]))) #change 5:10 
 
data_en$Respondent.ID[which(data_en$response_variance == 0)]<- NA

#### exclude all subjects that were set to NA:
xx = which(is.na(data_en$Respondent.ID))
if(length(xx)>0){data_en = data_en[-xx,]}

####### outliers

# distribution of response variance
hist(data_en$response_variance)

# exclude subjects with just little response variance?
# v = threshold variance
# data_en$Respondent.ID[which(data_en$response_variance < v)]<- NA

# distribution of completion time
hist(data_en$completionTime)

# exclude subjects with very short completion time?
#t = threshold completion time
# data_en$Respondent.ID[which(data_en$completionTime < t)]<- NA

# distribution of age
hist(data_en$age)


#### exclude all subjects that were set to NA:
xx = which(is.na(data_en$Respondent.ID))
if(length(xx)>0){data_en = data_en[-xx,]}

######### remove unnecessary columns 
xx = grep("X", colnames(data_en))
data_en = data_en[-xx]
# note that in the real data, the above step will also exclude the column IP address

data_en = data_en[, colSums(is.na(data_en)) != nrow(data_en)]


##### quality control #####

# "years.of.education.fulltext" includes the full answer for years.of.education for anyone with less than 10 years
# check these answers to make sure this was not due to typos or nor summing the total years of 

# number of subjects that were inconsistent in whether they had COVID related symptoms or not:
sum(data_en$symptom.inconsistency, na.rm = T)

# number of subjects that were inconsistent in whether they belong to a risk group:
sum(data_en$risk.group.inconsistency, na.rm = T)

# depending on the question addressed, these may be excluded:
# xx = which(data_en$symptom.inconsistency==1) # COVID test
# xx = which(data_en$risk.group.inconsistency==1) # risk group

# if(length(xx)>0){data_en = data_eu[-xx,]}

### ideas for quality control:

# of people listing 'full-time studying" in 13, how many indicate undergoing education in 12? 

# among people listing 'being in an occupation with enhanced risk of infection' what are the most frequent occupations?

# frequency table of 10 most frequent mental health conditions
head(count(data_en, 'mental.health.details'), n = 10)

# frequency table of 10 most frequent other quarantine situations
head(count(data_en, 'quarantine.status.text'), n = 10)

# financial insecurity by profession

########### check incomplete datasets #######
### remove missings 
data_complete <- data_en[data_en$missings == 0,]

##################### supplementary tables #################

######### table 1: sample demographics and health status 
# (all the initial basic variables and people‘s thinking about how the crisis is managed).

''' very awkward solution from myside; preparing aver single variable as a vector and rbind in the end to a dataframe.. /mz

demovar <- c("age", "gender", "current.stay.out.of.town", "years.of.education", "occupational.status", 
             "household.income", "relationship.status", "people.in.household", "diagnosed.mental.health", 
             "risk.group", "infection.test.status", "quarantine.status", "opinion.about.authorities.measures", 
             "adherence.to.recommended.procedures" )
demog <- data_en[demovar]
age <- summary(demog$age)
gender <- 
demog.summary <- data.frame()
age_descr <- c("Median age (range)", str_glue("{age[3]}","({age[1]}-{age[6]})"))
Gender_title <- c("Gender", "")
Male
'''
#demog.summary <- rbind(age_descr,sex_title)



######### table 2: average values + SD of the sample in all the dependent and independent variables

###step 1: extract all variables of interest into a new dataframe
##NOTE: excel sheet SR_Ec.SCM SR_Ec.SSM do not match in R, in R they are defined as SR_c.SCM and SR_c.SSM - if adjusted, change in line below and code above
variables.of.interest = data_en[, c("SR_Eg.SCM", "SR_Es.SCM", "SR_c.SCM", "SR_Eg.SSM", "SR_Es.SSM", "SR_c.SSM", "PAS", "PSS", "CSS", "OPT", "GSE", "REC", "NEU", "BCS", "PAC", "P", "Es.SCM", "Es.SSM", "Eg.SCM", "Eg.SSM", "Ec.SCM", "Ec.SSM", "CERQSum", "PAS", "age")]

###step 2: calculate mean/SD and put into a new dataframe
##NOTE: SR... variables are Z-scores, mean of Z-score is always 0 and SD will be 1 -> pointless to calculate?
Mean = as.data.frame(colMeans(variables.of.interest))
SD = as.data.frame(apply(variables.of.interest, 2, sd))
table.mean.sd = cbind(Mean, SD) #combine mean and SD into one table
colnames(table.mean.sd)[1] <- "Mean" #rename header
colnames(table.mean.sd)[2] <- "SD" #rename header
table.mean.sd[, c(1:2)] = table.mean.sd %>% mutate_at(vars(Mean, SD), funs(round(., 2))) #round to two decimals

###step 3: use formattable package to create a nice table that can be viewed in the viewer window of R
formattable(table.mean.sd)


######### table 3: intercorrelations of dependent and independent variables

###Basic correlation matrix
corr.matrix = as.data.frame(cor(variables.of.interest, use = "complete.obs")) #intercorrelation matrix
corr.matrix[, c(1:25)] = corr.matrix %>% mutate_at(vars(c(1:25)), funs(round(., 2))) #round to two decimals
formattable(corr.matrix, 
            align = c("c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c") #center values
)

###basic cor(...) does not give p-values, so use rcorr instead (requires more than 4 cases)
##variables.of.interest[nrow(variables.of.interest) + 1,] = 1 #testing purpose (needed more than 4 cases, ignore this line) 

corr.p.matrix <- rcorr(as.matrix(variables.of.interest))
corr.p.matrix$r #gives correlations
corr.p.matrix$P #gives p values of correlations

flattenCorrMatrix <- function(cormat, pmat) { #function to put r and p value into one table
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

corr.p.matrix <- as.data.frame(flattenCorrMatrix(corr.p.matrix$r, corr.p.matrix$P))
corr.p.matrix$Sign <- ifelse(corr.p.matrix$p < 0.05, "significant", "not significant") #threshold set at .05
corr.p.matrix[, c(3,4)] = round(corr.p.matrix[, c(-1, -2, -5)],2) #round 2 decimals, change last value (the 2) if you want more/less decimals
formattable(corr.p.matrix) #viewer table


###helpful intercorrlation matrix graph 
x = cor(variables.of.interest)
corrplot(x, type = "lower")


