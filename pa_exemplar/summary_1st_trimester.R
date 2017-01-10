## Summaries for first trimester PAIP analysis
## Author: Tom Bishop
##         Paul Scherer
## Date: 29/07/2016

## Datasets:
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

myvars = list('MOD_VIG_filt', 'LTPA_DUR_filt', 'LTPA_EE_filt','BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA',
              'GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING','ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 
              'GDM', 'MATERNAL_BMI', 'MATERNAL_OB', 'PREECLAMPSIA')
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars)

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
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

# for GECKO only dummy variable for LTPA_DUR_filt since this does not exist
# comment out if not using GECKO
#work1 <- no_preecl$GECKO
#work2 <- paste0("datashield.assign(opals[\"GECKO\"],'LTPA_DUR_filt', quote(rep(1,",work1,")))")
#eval(parse(text=work2))
#ds.cbind(x=c('temp','D2','LTPA_DUR_filt'), newobj='D2a', datasource=opals["GECKO"])

# Filter out missing values
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

# Variables used within analysis
my_exp_all = c('MOD_VIG_filt', 'LTPA_DUR_filt', 'LTPA_EE_filt')
my_outcome_all = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_cov_all = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
               'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM', 'MATERNAL_BMI', 'MATERNAL_OB')

# Loop to produce E4 and model_all_len for descriptive stats
# my_vars_all <- c('temp', my_exp_all, my_outcome_all, my_cov_all)
# model_all_len <- data.frame()
# 
# for (i in 2:length(my_vars_all)){
#   ds.subset(x = 'D2a', subset = 'E3', cols =  c(my_vars_all[1:i]))
#   ds.subset(x = 'E3', subset = 'E4', completeCases = TRUE)
#   model_all_len <- rbind(model_all_len,ds.length('E4$temp', type = 'split'))
# }
# 
# row.names(model_all_len) <- my_vars_all[2:length(my_vars_all)]


# Generate E4 without the loop, doesnt produce model_all_len
my_vars_all <- c(my_exp_all, my_outcome_all, my_cov_all)
ds.subset(x = 'D2a', subset = 'E3', cols =  my_vars_all)
ds.subset(x = 'E3', subset = 'E4', completeCases = TRUE)