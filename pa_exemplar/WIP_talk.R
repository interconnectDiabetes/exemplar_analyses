## Work in progress seminar 2017
## Live Demo 

# show that we have logged into to create 
# the HD object at each study location

ds.ls()

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# basic counts
ds.length('HD$SEX')

# remove preterm <37w
ds.subset(x = 'HD', subset = 'HD_filt_1', 
          logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37)

# show that a new object has been created in each study
ds.exists(x = 'HD_filt_1')

# show that the new object is smaller after filtering
ds.length('HD_filt_1$SEX')

# remove preeclampsia
ds.subset(x = 'HD_filt_1', subset = 'HD_filt_2', 
          logicalOperator = 'PREECLAMPSIA==', threshold = 0)

# Filter data set to exclude participants with missing values for 
# variables used in the analysis

ds.subset(x = 'HD_filt_2', subset = 'HD_comp', 
          completeCases = TRUE)

###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
#---------------------------------------------------------
# Summaries for covariates and confounders

# Sex

ds.summary('HD_comp$SEX')

# Moderate-vigorous physical activity

ds.summary('HD_comp$MOD_VIG_3_filt')


###############################################################################
########################### RUN MODELS  #######################################
###############################################################################

#  MODEL 1 - simple association

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')
covariates = c('GESTATIONAL_AGE', 'SEX')


ds.random_effects(my_exposure = exposures, my_outcome = outcomes, 
        my_covariate = covariates, ref_table = 'HD_comp')

#  MODEL 2 - include confounders

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT')
covariates= c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                     'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

ds.random_effects(my_exposure = exposures, my_outcome = outcomes,
      my_covariate = covariates, ref_table = 'HD_comp')

#  MODEL 3 - ad hoc investigation

exposures = c('MOD_VIG_3_filt')
outcomes = c( 'BIRTH_WEIGHT_SGA')
covariates = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                     'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

ds.random_effects(my_exposure = exposures, my_outcome = outcomes, 
        my_covariate = covariates, ref_table = 'HD_comp')

