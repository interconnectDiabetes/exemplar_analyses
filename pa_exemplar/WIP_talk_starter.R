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
datashield.logout(opals) # kill off any previous sessions

# List of variables for the analysis


# Log in
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/pa',symbol = 'harmonised_data')


###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# basic counts


# remove preterm <37w


# remove preeclampsia

# Filter data set to exclude participants with missing values for variables used in the analysis


###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
#---------------------------------------------------------
# Summaries for covariates and confounders

# Sex



# Moderate-vigorous physical activity




###############################################################################
########################### RUN MODELS  #######################################
###############################################################################

#  MODEL 1 - simple association PA and birth weight

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')

# covariates



#  MODEL 2 - include confounders with PA and birth weight

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')
# covariates



#  MODEL 3 - ad hoc investigation confounders, PA and SGA

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT_SGA')
#covariates



