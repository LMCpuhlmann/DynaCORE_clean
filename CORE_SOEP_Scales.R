setwd("C:/Users/Matze/Documents/Promotion/Analysen/qualitative_data")
load("data_pull_5000_completesurveys_eu_cleaned_data.RData")

data_en.eu.complete <- data_en[!is.na(data_en$complete.eu),] 

# Positive Appraisal Style:
term <- "CERQ"
CERQ <- grep(term, names(data_en.eu.complete))
PAS <- data_en.eu.complete[,c(CERQ,100,104 )]
PAS[,c("H1_COPE_18","H1_COPE_28")] <- PAS[,c("H1_COPE_18","H1_COPE_28")]*5/4 #rescale
PAS$CERQSum <- NULL
data_en.eu.complete$PAS <- rowMeans(PAS)

aPAS <- psych::alpha(PAS) #raw alpha 0.83

# SOEP 2-4 CERQ 15, 16, 11 

PAS_SOEP <- data_en.eu.complete[,c(108,110,111)]
data_en.eu.complete$PAS_SOEPsum <- rowSums(data_en.eu.complete[,c(108,110,111)])

aPAS_SOEP <- psych::alpha(PAS_SOEP) #raw alpha 0.65

#SOEP Question one: data_en.eu.complete$H5_01
#SOEP Question 5: COPE 10 (BRS)
#SOEP Question 6: catastrophizing item (not in CORE-C)
#SOEP Question 7+8: PAC
#SOEP Question 9-12: Mental Health (other questions than CORE-C)

cor.test(data_en.eu.complete$PAS_SOEPsum, data_en.eu.complete$SR_Ec.SSM)

#Pearson's product-moment correlation
#
#data:  data_en.eu.complete$PAS_SOEPsum and data_en.eu.complete$SR_Ec.SSM
#t = -19.484, df = 4995, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2913454 -0.2398042
#sample estimates:
#       cor 
#-0.2657647 

cor.test(data_en.eu.complete$PAS, data_en.eu.complete$SR_Ec.SSM)

#	Pearson's product-moment correlation
#
#data:  data_en.eu.complete$PAS and data_en.eu.complete$SR_Ec.SSM
#t = -19.444, df = 4995, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.2908474 -0.2392913
#sample estimates:
#  cor 
#-0.265259 