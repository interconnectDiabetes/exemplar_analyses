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
#setwd("/home/l_pms69/exemplar_analyses/")
setwd("/home/l_trpb2/git/exemplar_analyses/")
source("creds/pa_exemplar_3_creds.R")

setwd("~")
datashield.logout(opals) # kill off any previous sessions

myvars = list('MOD_VIG_3_filt', 'LTPA_EE_3_filt', 'BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA',
              'GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING','ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 
              'GDM', 'MATERNAL_BMI', 'MATERNAL_OB', 'PREECLAMPSIA', 'BIRTH_WEIGHT_SGA')
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/pa',symbol = 'harmonised_data')

# some useful values for later processing - how many studies and their names
num_studies <- length(opals)
study_names <- names(opals)

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# basic counts
all_infants <- ds.length('harmonised_data$SEX', type = 'split')

# remove preterm <37w
ds.subset(x = 'harmonised_data', subset = 'harm_data_filt_1', logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37)
no_preterm <- ds.length('harm_data_filt_1$SEX', type = 'split')

# remove preeclampsia
ds.subset(x = 'harm_data_filt_1', subset = 'harm_data_filt_2', logicalOperator = 'PREECLAMPSIA==', threshold = 0)
no_preecl <- ds.length('harm_data_filt_2$SEX', type = 'split')

# Filter data set to exclude participants with missing values for variables used in the analysis

ds.subset(x = 'harm_data_filt_2', subset = 'harm_data_complete', completeCases = TRUE)

###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
#---------------------------------------------------------
# Summaries for covariates and confounders

# Sex
summary_sex_temp <- ds.summary('harm_data_complete$SEX')
summary_sex <- data.frame(matrix(unlist(summary_sex_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_sex) <- study_names
colnames(summary_sex) <- c("type", "N", "male", "female", "count0", "count1")
rm(summary_sex_temp)

# MOD_VIG_3_filt
summary_mod_temp <- ds.summary('harm_data_complete$MOD_VIG_3_filt')
summary_mod <- data.frame(matrix(unlist(summary_mod_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_mod) <- study_names
colnames(summary_mod) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_mod <- summary_mod[,c(2,6,5,7)]
rm(summary_mod_temp)

###############################################################################
########################### RUN MODELS  #######################################
###############################################################################

#  MODEL 1 - simple association

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')
covariates = c('GESTATIONAL_AGE', 'SEX')

ds.random_effects(my_exposure = exposures, my_outcome = outcomes, my_covariate = covariates, ref_table = 'harm_data_complete')

#  MODEL 2 - include confounders

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')
covariates = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

ds.random_effects(my_exposure = exposures, my_outcome = outcomes, my_covariate = covariates, ref_table = 'harm_data_complete')

#  MODEL 3 - ad hoc investigation

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT_SGA')
covariates = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

ds.random_effects(my_exposure = exposures, my_outcome = outcomes, my_covariate = covariates, ref_table = 'harm_data_complete')

