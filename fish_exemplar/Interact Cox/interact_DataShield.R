## ## Analysis script for Fish Exemplar Analysis
## Author: Paul Scherer
##		   Tom Bishop
## Date: 18/0062017

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

# get extra functions from betaTestClient that didnt make it
source("fish_exemplar/helperFunctions.R")
# Retrieve Credential Details
source("creds/fish_exemplar_creds.R")
setwd("~")

datashield.logout(opals)

myvars = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
           "AGE_BASE", "AGE_END_OBJ","MI", "STROKE", "CANCER", "HYPERTENSION", "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           "FAM_DIAB", "E_INTAKE", "FRUIT", "VEG", "DAIRY", "FIBER", "RED_MEAT" , "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS")

opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')