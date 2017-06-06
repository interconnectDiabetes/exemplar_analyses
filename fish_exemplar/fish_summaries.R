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
           	"AGE_BASE", "AGE_END","MI", "STROKE", "CANCER", "HYPERTENSION", "SEX", "BMI", "GEOG_AREA", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           	"FAM_DIAB", "E_INTAKE", "FRUIT", "VEG", "DAIRY", "FIBER", "RED_MEAT" , "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS")

opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')

# # To include all possible variables uncomment this line and and comment out previus line
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish')

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# Dataframe to hold length figures
model_all_len <- data.frame()

# all participants
all_participants <- ds.length('D$TOTAL')
all_participants_split <- ds.length('D$TOTAL',type = 'split')


# Set studynames and numstudies
temp <- ds.summary('D$TOTAL')
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

# remove participants with prevalent diabetes and type 1
ds.subset(x = 'D', subset = 'E1', logicalOperator = 'PREV_DIAB<=', threshold = 1)
noPrevalence <- ds.length('E1$SEX', type = 'split')
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB>=', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# remove participants with too little and excessive consumption of calories
ds.subset(x = 'E2', subset = 'E3', logicalOperator = 'E_INTAKE>=', threshold = 3500)
under3500cal <- ds.length('E3$SEX', type = 'split')
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'E_INTAKE<=', threshold = 500)
afterIntake <- ds.length('E4$SEX', type = 'split')

model_all_len <- rbind(model_all_len, all_participants_split, noPrevalence, noType1, under3500cal, afterIntake)

# adding in zero columns to the studies
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('newStartDate','E4'), newobj='E5')

# Loop to produce E4 and model_all_len for descriptive stats
my_vars_all = c("AGE_BASE", "CASE_OBJ_SELF", "CASE_OBJ")

gimm = c("AGE_END", "FATTY", "FRESH", "FRIED", "LEAN", "NONFISH", "SALT", "SSD", "TOTAL", 
	"SEX", "BMI", "GEOG_AREA", "EDUCATION", "SMOKING", "PA", "ALCOHOL", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION", "E_INTAKE", "FRUIT",
	"VEG", "DAIRY", "FIBER", "RED_MEAT", "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS")
my_vars_all <- c('newStartDate', my_vars_all) #because datashield doesnt like single column subsets

for (i in 2:length(my_vars_all)){
  ds.subset(x = 'E5', subset = 'E6', cols =  my_vars_all[1:i], completeCases = TRUE)
  model_all_len <- rbind(model_all_len, ds.length('E6$newStartDate', type = 'split'))
}
rownames = c("ALL", "PREV_DIAB", "TYPE_DIAB", "under3500cal", "afterIntake", my_vars_all[2:length(my_vars_all)])
row.names(model_all_len) <- rownames

# Only Complete Cases (currently not in use for testing behaviour with nulls and the fact that complete 
# cases knock out every available participant at the moment)
ds.subset(x = 'E7', subset = 'E8', completeCases = TRUE)
complete_participants <- ds.length('E7$TOTAL')
complete_participants_split <- ds.length('E7$TOTAL',type = 'split')

## TODO CHANGE ACCORDING TO TOP WHEN IT RUNS
# Setup an additional proxy ID column for each study 
for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('ID','D'), newobj='D2')

# adding in zero columns to the studies
for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('newStartDate','D2'), newobj='D3')

ds.assign(toAssign = 'D$AGE_END-D$AGE_BASE', newobj = 'newEndDate')
ds.cbind(x=c('newEndDate','D3'), newobj='D4')




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

# Exposures Missing Checker
fullNum = ds.length('D4$AGE_BASE', type = 'split') 
fattyMissing =  ds.numNA('D4$FATTY')
freshMissing = ds.numNA('D4$FRESH')
friedMissing = ds.numNA('D4$FRIED')
leanMissing = ds.numNA('D4$LEAN')
nonfishMissing = ds.numNA('D4$NONFISH')
saltMissing = ds.numNA('D4$SALT')
ssdMissing = ds.numNA('D4$SSD')
totalMissing = ds.numNA('D4$TOTAL')

exposure_missings_table = data.frame(cbind(study_names,fullNum, fattyMissing, freshMissing, friedMissing, leanMissing, nonfishMissing, saltMissing, ssdMissing, totalMissing))
colnames(exposure_missings_table) <- c('Study Name', 'Total in Study', 'fattyMissing', 'freshMissing', 'friedMissing', 'leanMissing', 'nonfishMissing', 'saltMissing', 'ssdMissing', 'totalMissing')

# Confounders Missing Checker
miMissing = ds.numNA('D4$MI')
strokeMissing = ds.numNA('D4$STROKE')
cancerMissing = ds.numNA('D4$CANCER')
hypertensionMissing = ds.numNA('D4$HYPERTENSION')

conf_missings_table = data.frame(cbind(study_names, fullNum, miMissing, cancerMissing, strokeMissing, hypertensionMissing))
colnames(conf_missings_table) <- c('Study Name', 'Total in Study', 'miMissing', 'cancerMissing', 'strokeMissing', 'hypertensionMissing')



#---------------------------------------------------------
# Summaries for exposures
# fatty fish
# summary_fatty = summaryContExp('D$FATTY', study_names, num_studies)
# fresh fish
# summary_fresh = summaryContExp('D$FRESH', study_names, num_studies)
# fried fish
# summary_fried = summaryContExp('D$FRIED', study_names, num_studies)
# lean fish
# summary_lean = summaryContExp('D$LEAN', study_names, num_studies)
# nonfish
# summary_nonfish = summaryContExp('D$NONFISH', study_names, num_studies)
# salt fish
# summary_salt = summaryContExp('D$SALT', study_names, num_studies)
# ssd fish
# summary_ssd = summaryContExp('D$SSD', study_names, num_studies)
# total fish
# summary_total = summaryContExp('D$TOTAL', study_names, num_studies)

                               
#---------------------------------------------------------
# Summaries for outcomes
summary_objective_case = summaryBinExp('D$CASE_OBJ', study_names, num_studies)
summary_self_case = summaryBinExp("D$CASE_OBJ_SELF", study_names, num_studies)

#---------------------------------------------------------
# Summaries for covariates and confounders
# education

# ses

# # smoking
# summaryBinExp('D$SMOKING', study_names, num_studies)

# # mi, stroke, cancer, hypertension
# summaryBinExp('D$MI', study_names, num_studies)
# summaryBinExp('D$STROKE', study_names, num_studies)
# summaryBinExp('D$CANCER', study_names, num_studies)
# summaryBinExp('D$HYPERTENSION', study_names, num_studies)

# # Continous covariates
# summary_pa = summaryContExp('D$PA', study_names, num_studies)
# summary_alc = summaryContExp('D$ALCOHOL', study_names, num_studies)
# summary_supplements = summaryContExp('D$SUPPLEMENTS', study_names, num_studies)
# summary_eintake = summaryContExp('D$E_INTAKE', study_names, num_studies)
# summary_red_meat = summaryContExp('D$RED_MEAT', study_names, num_studies)
# summary_proc_meat = summaryContExp('D$PROC_MEAT', study_names, num_studies)
# summary_fruit = summaryContExp('D$FRUIT', study_names, num_studies)
# summary_veg = summaryContExp('D$VEG', study_names, num_studies)
# summary_dairy = summaryContExp('D$DAIRY', study_names, num_studies)
# summary_fiber = summaryContExp('D$FIBER', study_names, num_studies)
# summary_sugardrinks = summaryContExp('D$SUG_BEVS', study_names, num_studies)