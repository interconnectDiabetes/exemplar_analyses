## Work in progress seminar 2017
## Live Demo 

# show that we have logged into to create 
# the HD object at each study location



###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# basic counts


# remove preterm <37w


# show that a new object has been created in each study


# show that the new object is smaller after filtering


# remove preeclampsia


# Filter data set to exclude participants with missing values for 
# variables used in the analysis



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

#  MODEL 1 - simple association

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')
covariates = c('GESTATIONAL_AGE', 'SEX')



#  MODEL 2 - include confounders

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')
covariates= c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
              'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')



#  MODEL 3 - ad hoc investigation

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT_SGA')
covariates = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
               'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')



