# R Analysis Script for first trimester PAIP analysis
# Paper results edition: Running this script will give you the same results as those reported
# in paper: Association between maternal physical activity pregnancy and offspring brith size 
# differs between early and late pregnancy: an individual level meta-analysis


## Author: Paul Scherer
##         Tom Bishop
## Date: 02/05/2017

## Datasets involved in study:
## ABCD
## ALSPAC
## DNBC
## REPRO
## ROLO
## SWS

###############################################################################
########################### Dependencies   ####################################
###############################################################################
library(opal)
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(metafor)

###############################################################################
########################### SET UP SERVERS  ###################################
###############################################################################
# Set working directory to source our credentials

#setwd("/home/l_pms69/exemplar_analyses/")
setwd("/home/l_trpb2/git/exemplar_analyses/")


# Sourcing the credentials sets values for the following variables:
# server
# url
# table
# password 
# user
# logindata_all

source("creds/pa_exemplar_creds.R")
setwd("~")
datashield.logout(opals)

myvars = list('MOD_VIG_filt', 'LTPA_DUR_filt', 'LTPA_EE_filt','VIG_filt', 'BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA',
              'GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING','ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 
              'GDM', 'MATERNAL_BMI', 'MATERNAL_OB', 'PREECLAMPSIA', 'BIRTH_WEIGHT_SGA')
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/pa')

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# this section outlines the filtering processes that were performed on the data
# prior to analysis

# basic counts
all_infants <- ds.length('D$SEX', type = 'split')

# remove preterm <37w
ds.subset(x = 'D', subset = 'D1', logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37)
no_preterm <- ds.length('D1$SEX', type = 'split')

# remove preeclampsia
ds.subset(x = 'D1', subset = 'D2', logicalOperator = 'PREECLAMPSIA==', threshold = 0)
no_preecl <- ds.length('D2$SEX', type = 'split')

# variables for model 1
# need to generate a 'temp' variable with no missings and add this in,
# because the ds.subset command needs at least 2 columns to work with (a ds bug)
for(i in 1:length(opals)){
  work1 <- no_preecl[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'temp', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('temp','D2'), newobj='D2a')

# Filter out missing values
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

# Variables used within analysis
my_exp_all = c('MOD_VIG_filt', 'LTPA_DUR_filt', 'LTPA_EE_filt', 'VIG_filt')
my_outcome_all = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA', 'BIRTH_WEIGHT_SGA')
my_cov_all = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
               'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM', 'MATERNAL_BMI', 'MATERNAL_OB')

# Make a complete case subset with the availbale variables
my_vars_all <- c(my_exp_all, my_outcome_all, my_cov_all)
ds.subset(x = 'D2a', subset = 'E3', cols =  my_vars_all)
ds.subset(x = 'E3', subset = 'E4', completeCases = TRUE)