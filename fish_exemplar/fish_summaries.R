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
noPrevalence <- ds.length('E2$SEX', type = 'split')

# remove participants with too little and excessive consumption of calories
ds.subset(x = 'E2', subset = 'E3', logicalOperator = 'E_INTAKE>=', threshold = 3500)
under3500cal <- ds.length('E3$SEX', type = 'split')
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'E_INTAKE<=', threshold = 500)
afterIntake <- ds.length('E4$SEX', type = 'split')


# Loop to produce E4 and model_all_len for descriptive stats
my_vars_all = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
	"AGE_BASE", "AGE_END","MI", "STROKE", "CANCER", "HYPERTENSION", "SEX", "BMI", "GEOG_AREA", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
	"FAM_DIAB", "E_INTAKE", "FRUIT", "VEG", "DAIRY", "FIBER", "RED_MEAT" , "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS")
model_all_len <- data.frame()

for (i in 2:length(my_vars_all)){
  ds.subset(x = 'E4', subset = 'E5', cols =  c(my_vars_all[1:i]))
  ds.subset(x = 'E5', subset = 'E6', completeCases = TRUE)
  model_all_len <- rbind(model_all_len,ds.length('E6$temp', type = 'split'))
}

row.names(model_all_len) <- my_vars_all[2:length(my_vars_all)]

# Only Complete Cases (currently not in use for testing behaviour with nulls and the fact that complete 
# cases knock out every available participant at the moment)
ds.subset(x = 'E6', subset = 'E7', completeCases = TRUE)
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
