# This file serves to run all the summary code for the fish exemplar, to seperate the task of running summaries
# and performing the complex models

## Author: Paul Scherer
##		   Tom Bishop
## Date: 06/06/2017

###############################################################################
########################### Dependencies   ####################################
###############################################################################
library(opal)
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(dsBetaTestClient)
library(metafor)

###############################################################################
########################### SET UP SERVERS  ###################################
###############################################################################
# Set working directory to source our credentials
setwd("/home/l_pms69/exemplar_analyses/")
#setwd("/home/l_trpb2/git/exemplar_analyses/")

# Retrieve Credential Details
source("creds/fish_exemplar_creds.R")
setwd("~")

datashield.logout(opals)

myvars = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
           	"AGE_BASE", "AGE_END","MI", "STROKE", "CANCER", "HYPERTENSION", "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           	"FAM_DIAB", "E_INTAKE", "FRUIT", "VEG", "DAIRY", "FIBER", "RED_MEAT" , "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS", 
           "AGE_END_OBJ_SELF", "AGE_END_OBJ")

opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')

# # To include all possible variables uncomment this line and and comment out previus line
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish')

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# all participants
all_participants <- ds.length('D$TOTAL')
all_participants_split <- ds.length('D$TOTAL',type = 'split')


# Set studynames and numstudies
temp <- ds.summary('D$TOTAL')
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

# adding in zero columns to the studies
for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('newStartDate','D'), newobj='D1')

# remove participants with prevalent diabetes and type 1
ds.subset(x = 'D1', subset = 'E1', logicalOperator = 'PREV_DIAB==', threshold = 0)
noPrevalence <- ds.length('E1$SEX', type = 'split')
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB==', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# In order to deal with the intake subsets stratified by sex we will have to create subsets of sex,
# do the intake subset and then rbind the groups back together. What follows is DataSHIELD magic
ds.asNumeric("E2$SEX", newobj = "sexNumbers")
ds.assign(toAssign="(sexNumbers*300)+E2$E_INTAKE", newobj = "adjustedLowerBound")
ds.assign(toAssign="(sexNumbers*700)+E2$E_INTAKE", newobj = "adjustedUpperBound")
ds.cbind(x=c("adjustedLowerBound", "E2"), newobj = "L1")
ds.cbind(x=c("adjustedUpperBound", "L1"), newobj = "L2")
# remove participants with too little and excessive consumption of calories
ds.subset(x = 'L2', subset = 'E3', logicalOperator = 'adjustedUpperBound<=', threshold = 4200)
under3500cal <- ds.length('E3$SEX', type = 'split')
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'adjustedLowerBound>=', threshold = 800)
afterIntake <- ds.length('E4$SEX', type = 'split')

# # Setup an additional proxy ID column for each study 
# for(i in 1:length(opals)){
#   work1 <- afterIntake[[i]]
#   work2 <- paste0("datashield.assign(opals[",i,"],'fakeIds', quote(c(1:",work1,")))")
#   eval(parse(text=work2))
# }
# ds.cbind(x=c('fakeIds','E4'), newobj='E5')
# 
# # Loop to produce E4 and model_all_len for descriptive stats
# # Note that this doesnt actually handle well if a study has lost all its participants before this section
# my_vars_all = c("AGE_BASE", "CASE_OBJ_SELF", "CASE_OBJ","AGE_END", "FATTY", "FRESH", "FRIED", "LEAN", "NONFISH", "SALT", "SSD", "TOTAL",
# 	"SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION", "E_INTAKE", "FRUIT",
# 	"VEG", "DAIRY", "FIBER", "RED_MEAT", "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS")
# my_vars_all <- c('fakeIds', my_vars_all) #because datashield doesnt like single column subsets
# 
# 
# # Dataframe to hold length figures
# model_all_len <- data.frame()
# model_all_len <- rbind(model_all_len, all_participants_split, noPrevalence, noType1, under3500cal, afterIntake)
# 
# 
# for (i in 2:length(my_vars_all)){
#   ds.subset(x = 'E5', subset = 'E6', cols =  my_vars_all[1:i])+
#   ds.subset(x = 'E6', subset = 'E7', completeCases = TRUE)
#   # model_all_len <- rbind(model_all_len, ds.length('E7$fakeIds', type = 'split'))
#   thingToBind = vector("numeric")
#   print(i)
#   for (k in 1:num_studies){
#     lengthNum = ds.length('E7$fakeIds', datasources = opals[k])
#     thingToBind = c(thingToBind, lengthNum)
#     print(thingToBind)
#   }
#   thingToBind = unlist(unname(thingToBind))
#   print("this is thingtobind unlistedunnamed")
#   print(k)
#   print(thingToBind)
#   model_all_len = rbind(model_all_len, thingToBind)
# }
# rownames = c("ALL", "PREV_DIAB", "TYPE_DIAB", "under3500cal", "afterIntake", my_vars_all[2:length(my_vars_all)])
# row.names(model_all_len) <- rownames

###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
summaryContExp <- function(column, study_names, num_studies) {
    # given a table$column combination as a string, return the summary table 
    # for the continous variable
    summary_column_temp = ds.summary(column)
    summary_column = data.frame(matrix(unlist(summary_column_temp), nrow = num_studies, ncol=10, byrow=TRUE))
    rownames(summary_column) = study_names
    colnames(summary_column) = c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
    summary_column = summary_column[,c(2,6,5,7)]
    rm(summary_column_temp)
    return(summary_column)
}

summaryBinExp <- function(column, study_names, num_studies) {
    # given a table$column combination as a string, return the summary
    # table for the binary variable
    summary_column_temp = ds.summary(column)
    summary_column = data.frame(matrix(unlist(summary_column_temp), nrow = num_studies, ncol=6, byrow=TRUE))
    rownames(summary_column) <- study_names
    colnames(summary_column) <- c('type', 'n', '0', '1', 'No', 'Yes')
    rm(summary_column_temp)
    return(summary_column)
}

summaryCatExp <- function (column, study_names, num_studies, levels = 2){
	# given a table$column combination as a string, return the overall summary for categorical
	# variables set in the levels parameter.
	summary_column_temp = ds.summary(column)
	summary_column = data.frame(matrix(unlist(summary_column_temp), nrow=num_studies, ncol=(2+(2*levels)), byrow = TRUE))
	rownames(summary_column) <- study_names
	colnames(summary_column) = c('type', 'n')
	rm(summary_column_temp)
	return(summary_column)
}

#---------------------------------------------------------
# Exposures Missing Checker
fullNum = ds.length('D$AGE_BASE', type = 'split') 
fattyMissing =  ds.numNA('D$FATTY')
freshMissing = ds.numNA('D$FRESH')
friedMissing = ds.numNA('D$FRIED')
leanMissing = ds.numNA('D$LEAN')
nonfishMissing = ds.numNA('D$NONFISH')
saltMissing = ds.numNA('D$SALT')
ssdMissing = ds.numNA('D$SSD')
totalMissing = ds.numNA('D$TOTAL')
exposure_missings_table = data.frame(cbind(study_names,fullNum, fattyMissing, freshMissing, friedMissing, leanMissing, nonfishMissing, saltMissing, ssdMissing, totalMissing))
colnames(exposure_missings_table) <- c('Study Name', 'Total in Study', 'fattyMissing', 'freshMissing', 'friedMissing', 'leanMissing', 'nonfishMissing', 'saltMissing', 'ssdMissing', 'totalMissing')

#---------------------------------------------------------
# Previous alt Case Missing Checker
miMissing = ds.numNA('D$MI')
strokeMissing = ds.numNA('D$STROKE')
cancerMissing = ds.numNA('D$CANCER')
hypertensionMissing = ds.numNA('D$HYPERTENSION')
alt_case_missing_table = data.frame(cbind(study_names, fullNum, miMissing, cancerMissing, strokeMissing, hypertensionMissing))
colnames(alt_case_missing_table) <- c('Study Name', 'Total in Study', 'miMissing', 'cancerMissing', 'strokeMissing', 'hypertensionMissing')

#---------------------------------------------------------
# Confounders Missing Checker
agebaseMissing = ds.numNA('D$AGE_BASE')
ageendMissing = ds.numNA('D$AGE_END')
sexMissing = ds.numNA('D$SEX')
bmiMissing = ds.numNA('D$BMI')
educationMissing = ds.numNA('D$EDUCATION')
smokingMissing = ds.numNA('D$SMOKING')
paMissing = ds.numNA('D$PA')
alcoholMissing  = ds.numNA('D$ALCOHOL')
famdiabMissing  = ds.numNA('D$FAM_DIAB')
eintakeMissing = ds.numNA('D$E_INTAKE')
fruitMissing = ds.numNA('D$FRUIT')
vegMissing = ds.numNA('D$VEG')
dairyMissing  = ds.numNA('D$DAIRY')
fiberMissing = ds.numNA('D$FIBER')
redmeatMissing = ds.numNA('D$RED_MEAT')
procmeatMissing = ds.numNA('D$PROC_MEAT')
sugbevsMissing = ds.numNA('D$SUG_BEVS')
medsMissing = ds.numNA('D$MEDS')
waistMissing = ds.numNA('D$WAIST')
supplementsMissing = ds.numNA('D$SUPPLEMENTS')
conf_missing_table = data.frame(cbind(study_names, fullNum, agebaseMissing, ageendMissing, sexMissing, bmiMissing, educationMissing, smokingMissing, paMissing, alcoholMissing,
  famdiabMissing, eintakeMissing, fruitMissing, vegMissing, dairyMissing, fiberMissing, redmeatMissing, procmeatMissing, sugbevsMissing, medsMissing, 
  waistMissing, supplementsMissing))
colnames(conf_missing_table) <- c('Study Name', 'Total in Study', 'agebaseMissing', 'ageendMissing', 'sexMissing', 'bmiMissing', 'educationMissing', 'smokingMissing', 'paMissing', 'alcoholMissing',
  'famdiabMissing', 'eintakeMissing', 'fruitMissing', 'vegMissing', 'dairyMissing', 'fiberMissing', 'redmeatMissing', 'procmeatMissing', 'sugbevsMissing', 'medsMissing', 
  'waistMissing', 'supplementsMissing')

#---------------------------------------------------------
# Outcomes Missing Checker
caseobjMissing = ds.numNA('D$CASE_OBJ')
caseobjselfMissing = ds.numNA('D$CASE_OBJ_SELF')
prevdiabMissing = ds.numNA('D$PREV_DIAB')
typediabMissing = ds.numNA('D$TYPE_DIAB')
outcomes_missings_table = data.frame(cbind(study_names, fullNum, caseobjMissing, caseobjselfMissing, prevdiabMissing, typediabMissing))
colnames(outcomes_missings_table) <- c('Study Name', 'Total in Study', 'caseobjMissing', 'caseobjselfMissing', 'prevdiabMissing', 'typediabMissing')



#---------------------------------------------------------
# Summaries for exposures
summary_fatty = summaryContExp('E4$FATTY', study_names, num_studies)
summary_fresh = summaryContExp('E4$FRESH', study_names, num_studies)
summary_fried = summaryContExp('E4$FRIED', study_names, num_studies)
summary_lean = summaryContExp('E4$LEAN', study_names, num_studies)
summary_nonfish = summaryContExp('E4$NONFISH', study_names, num_studies)
summary_salt = summaryContExp('E4$SALT', study_names, num_studies)
summary_ssd = summaryContExp('E4$SSD', study_names, num_studies)
summary_total = summaryContExp('E4$TOTAL', study_names, num_studies)
                               
#---------------------------------------------------------
# Summaries for outcomes
summary_objective_case = summaryBinExp('E4$CASE_OBJ', study_names, num_studies)
summary_self_case = summaryBinExp("E4$CASE_OBJ_SELF", study_names, num_studies)

summary_self_age = summaryContExp("E4$AGE_END_OBJ_SELF", study_names, num_studies)
summary_obj_age = summaryContExp("E4$AGE_END_OBJ", study_names, num_studies)
summary_age_base = summaryContExp("E4$AGE_BASE", study_names, num_studies)

summary_prevalence = summaryBinExp("D$PREV_DIAB", study_names, num_studies)
summary_type_diab = summaryBinExp("D$TYPE_DIAB", study_names, num_studies)


#---------------------------------------------------------
# Summaries for covariates and confounders
# education
summary_education = summaryCatExp('E4$EDUCATION', study_names, num_studies)
summary_smoking = summaryBinExp('E4$SMOKING', study_names, num_studies)

# # mi, stroke, cancer, hypertension
summary_mi = summaryBinExp('E4$MI', study_names, num_studies)
summary_stroke = summaryBinExp('E4$STROKE', study_names, num_studies)
summary_cancer = summaryBinExp('E4$CANCER', study_names, num_studies)
summary_hypertension = summaryBinExp('E4$HYPERTENSION', study_names, num_studies)

# # Continous covariates
summary_pa = summaryContExp('E4$PA', study_names, num_studies)
summary_alc = summaryContExp('E4$ALCOHOL', study_names, num_studies)
summary_supplements = summaryContExp('E4$SUPPLEMENTS', study_names, num_studies)
summary_eintake = summaryContExp('E4$E_INTAKE', study_names, num_studies)
summary_red_meat = summaryContExp('E4$RED_MEAT', study_names, num_studies)
summary_proc_meat = summaryContExp('E4$PROC_MEAT', study_names, num_studies)
summary_fruit = summaryContExp('E4$FRUIT', study_names, num_studies)
summary_veg = summaryContExp('E4$VEG', study_names, num_studies)
summary_dairy = summaryContExp('E4$DAIRY', study_names, num_studies)
summary_fiber = summaryContExp('E4$FIBER', study_names, num_studies)
summary_sugardrinks = summaryContExp('E4$SUG_BEVS', study_names, num_studies)