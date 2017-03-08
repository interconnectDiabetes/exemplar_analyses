## Work in progress seminar 2017
## Live Demo 

###############################################################################
########################### Dependencies   ####################################
###############################################################################
library(opal)
library(dsBaseClient)
library(dsModellingClient)
library(metafor)

###############################################################################
########################### SET UP SERVERS  ###################################
###############################################################################
# Login Details

# Set working directory to source our credentials

setwd("/home/l_trpb2/git/exemplar_analyses/")
source("creds/pa_exemplar_3_creds.R")
setwd("~")


# Log in
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, 
          directory = '/home/shared/certificates/pa',symbol = 'harmonised_data')

# some useful values for later processing - how many studies and their names
num_studies <- length(opals)
study_names <- names(opals)

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# basic counts
all_infants <- ds.length('harmonised_data$SEX', type = 'split')

# remove preterm <37w
ds.subset(x = 'harmonised_data', subset = 'harm_data_filt_1', 
          logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37)
no_preterm <- ds.length('harm_data_filt_1$SEX', type = 'split')

# remove preeclampsia
ds.subset(x = 'harm_data_filt_1', subset = 'harm_data_filt_2', 
          logicalOperator = 'PREECLAMPSIA==', threshold = 0)
no_preecl <- ds.length('harm_data_filt_2$SEX', type = 'split')

# Filter data set to exclude participants with missing values for 
# variables used in the analysis

ds.subset(x = 'harm_data_filt_2', subset = 'harm_data_complete', 
          completeCases = TRUE)

###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
#---------------------------------------------------------
# Summaries for covariates and confounders

# Sex

ds.summary('harm_data_complete$SEX')

# Moderate-vigorous physical activity

ds.summary('harm_data_complete$MOD_VIG_3_filt')


###############################################################################
########################### RUN MODELS  #######################################
###############################################################################

#  MODEL 1 - simple association

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')


ds.random_effects(my_exposure = exposures, my_outcome = outcomes, 
        my_covariate = covariates_mod_1, ref_table = 'harm_data_complete')

#  MODEL 2 - include confounders

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')

ds.random_effects(my_exposure = exposures, my_outcome = outcomes,
      my_covariate = covariates_mod_2, ref_table = 'harm_data_complete')

#  MODEL 3 - ad hoc investigation

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT_SGA')


ds.random_effects(my_exposure = exposures, my_outcome = outcomes, 
        my_covariate = covariates_mod_3, ref_table = 'harm_data_complete')

