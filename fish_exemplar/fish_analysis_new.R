## Analysis script for Fish Exemplar Analysis
## Author: Paul Scherer
##		   Tom Bishop
## Date: 31/03/2017

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
#setwd("/home/l_pms69/exemplar_analyses/")
setwd("/home/l_trpb2/git/exemplar_analyses/")

# Source in the Extra functions for analysis
source("variable_functions.R")
#source("fish_exemplar/helperFunctions.R")
source("survival_analysis_dsFunctions.R")
# Retrieve Credential Details
source("creds/fish_exemplar_creds.R")

# complete cases per study only on variables known to be not all missing
# the _vars.csv file stores information about which variables a study is expected to have and
# which variables are completely missing for all participants
# the buckets file contains defined buckets for the piecewise poisson analysis
setwd("/home/l_trpb2/git/exemplar_analyses/fish_exemplar")
filter_csv = read.csv(file = 'fish_opal_vars.csv',  header=TRUE, row.names = 1 )
source("fish_buckets.R")
setwd("~")

# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals)

#all the variables in the analysis
myvars = c(
  'AGE_BASE', 'TYPE_DIAB', 'PREV_DIAB', 'CASE_OBJ_SELF', 'CASE_OBJ', 'FUP_OBJ', 'FUP_OBJ_SELF', 'FATTY',
  'FRESH', 'FRIED', 'LEAN', 'NONFISH', 'SALT', 'SSD', 'TOTAL', 'SEX', 'BMI', 'EDUCATION', 'SMOKING', 'PA',
  'ALCOHOL', 'FAM_DIAB', 'E_INTAKE', 'FRUIT', 'VEG', 'FIBER', 'SUG_BEVS', 'WAIST', 'SUPPLEMENTS', 'COMORBID',
  'MEAT','QRT', 'SERVINGS', 'REGION_CH'
)


opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/fish')

rm(temp_opals)

# # To include all possible variables uncomment this line and and comment out previus line
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish')

# Set studynames and numstudies
temp <- ds.summary('D$TOTAL', datasources = opals)
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)


###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# all participants
all_participants <- ds.length('D$TOTAL', datasources = opals)
all_participants_split <- ds.length('D$TOTAL',type = 'split', datasources = opals)

# remove participants with prevalent diabetes and type 1
ds.subset(x = 'D', subset = 'E1', logicalOperator = 'PREV_DIAB==', threshold = 0)
noPrevalence <- ds.length('E1$SEX', type = 'split')
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB==', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# Remove CKB
opals_temp = opals
opals_temp["CKB"] = NULL

# In order to deal with the intake subsets stratified by sex we will have to create subsets of sex,
# do the intake subset and then rbind the groups back together. What follows is DataSHIELD magic
ds.asNumeric("E2$SEX", newobj = "sexNumbers", opals_temp)
ds.assign(toAssign="(sexNumbers*300)+E2$E_INTAKE", newobj = "adjustedLowerBound", opals_temp)
ds.assign(toAssign="(sexNumbers*700)+E2$E_INTAKE", newobj = "adjustedUpperBound", opals_temp)
ds.cbind(x=c("adjustedLowerBound", "E2"), newobj = "L1", opals_temp)
ds.cbind(x=c("adjustedUpperBound", "L1"), newobj = "L2", opals_temp)
# remove participants with too little and excessive consumption of calories
ds.subset(x = 'L2', subset = 'E3', logicalOperator = 'adjustedUpperBound<=', threshold = 4200, datasources = opals_temp)
under3500cal <- ds.length('E3$SEX', type = 'split', opals_temp)
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'adjustedLowerBound>=', threshold = 800, datasources = opals_temp)
afterIntake <- ds.length('E4$SEX', type = 'split', opals_temp)

rm(opals_temp)

#exception for CKB
ds.assign(toAssign="E2", newobj = "E4", opals["CKB"])
#generate under3500cal, afterIntake
under3500cal["CKB"] = noType1["CKB"]
afterIntake["CKB"] = noType1["CKB"]

#variables not to be used in complete cases
# stops loss of missing for these variables during complete cases, when they are not used in most models

none_cc_vars = c("WAIST","SUPPLEMENTS", "FAM_DIAB", 'tid.f','CENSOR')
#none_cc_vars = c('tid.f','CENSOR')

for (i in c(1:num_studies)) {
  my_name = names(opals[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  ds.subset(x = 'E4', subset = 'E5', cols =  list_variables, datasources = opals[i])
  ds.subset(x = 'E5', subset = 'E6', completeCases = TRUE, datasources = opals[i])
}
length_complete = ds.length("E6$AGE_BASE", type = "split", datasources = opals)

########################################################################
#### GLUE COMPLETE MISSINGS BACK ON AFTER FILTERING
########################################################################


for (i in c(1:num_studies)) {
  my_name = names(opals[i])
  size <- length_complete[[i]]
  list_variables = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  to_eval = paste0("ds.subset(x='D', subset='TRIM', rows = c(1:", size, "), datasources = opals[",i,"])")
  eval(parse(text=to_eval))
  for (j in 1:length(list_variables)){
    
    var_name <- list_variables[j]
    #to_eval = paste0("ds.subset(x='D$",var_name,"', subset='", var_name,"', rows = c(1:", size, "), datasources = opals[",i,"])")
    to_eval2 = paste0("datashield.assign(opals[",i,"],'",var_name,"', quote(TRIM$",var_name,"))")
    eval(parse(text=to_eval2))
  }
  ds.cbind(x=c(list_variables, "E6"), newobj = "E7", opals[i])
  
}

# Setup an additional proxy ID column for each study
for(i in 1:length(opals)){
  work1 <- length_complete[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('ID','E7'), newobj='E8')

# Zeros, new start date and end date
for(i in 1:length(opals)){
  work1 <- length_complete[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
rm(i)

ds.cbind(x=c('newStartDate','E8'), newobj='E9', datasources = opals)
ds.assign(toAssign = 'E9$FUP_OBJ', newobj = 'newEndDate', datasources = opals)
ds.assign(toAssign = 'E9$FUP_OBJ_SELF', newobj = 'newEndDate_SELF', datasources = opals)
ds.cbind(x=c('newEndDate', 'newEndDate_SELF','E9'), newobj='E10', datasources = opals)

# Adding in the weights as described by Prof. Burton
ds.asNumeric('E9$CASE_OBJ', newobj = "caseNums")
ds.assign(toAssign="((1 - caseNums)*35.92055) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_france'])
ds.assign(toAssign="((1 - caseNums)*23.55086) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_italy'])
ds.assign(toAssign="((1 - caseNums)*11.0115) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_spain'])
ds.assign(toAssign="((1 - caseNums)*27.87205) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_uk'])
ds.assign(toAssign="((1 - caseNums)*24.27497) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_netherlands'])
ds.assign(toAssign="((1 - caseNums)*24.62187) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_germany'])
ds.assign(toAssign="((1 - caseNums)*17.68276) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_sweden'])
ds.assign(toAssign="((1 - caseNums)*27.28305) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_denmark'])

# Non InterAct studies get a weighting of 1 in either case or noncase
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals[!names(opals) 
                                                                                      %in% c('InterAct_france', 'InterAct_italy','InterAct_spain','InterAct_uk',
                                                                                             'InterAct_netherlands', 'InterAct_germany', 'InterAct_sweden', 'InterAct_denmark')])

ds.cbind(x=c('burtonWeights','E10'), newobj='F1')

###############################################################################
########################### GENERATE TIME BUCKETS  ############################
###############################################################################

# Since we have fixed the studies and their variables, it is only necessary to generate
# the time buckets once

# For OBJ follow up time

my_outcome = c('CASE_OBJ')
my_exit_col = c('newEndDate')
final_table = 'F1'
bucket_table =  'A_OBJ'

for (study in c(1:length(opals))) {
  tunedLexisB(ref_table_in = final_table, my_outcome_in =  my_outcome, my_exit_col_in = my_exit_col, study = opals[study], new_table = bucket_table)
}
ds.asNumeric(paste0(bucket_table,'$CENSOR'),paste0(my_outcome,'_cens'), datasources = opals)
ds.asFactor(paste0(bucket_table, '$TIME.PERIOD'),'tid.f', datasources = opals)
# Check the time buckets for problems and fix them before going on
checkFactoredTime(studies = opals)
ds.assign(toAssign=paste0('log(',bucket_table,'$SURVTIME)'), newobj='logSurvival', datasources = opals)
ds.cbind(x=c(bucket_table,'tid.f', 'logSurvival'), newobj=bucket_table, datasources = opals)

# For OBJ_SELF follow up time

my_outcome = c('CASE_OBJ_SELF')
my_exit_col = c('newEndDate_SELF')
final_table = 'F1'
bucket_table =  'A_OBJ_SELF'

for (study in c(1:length(opals))) {
  tunedLexisB(ref_table_in = final_table, my_outcome_in =  my_outcome, my_exit_col_in = my_exit_col, study = opals[study], new_table = bucket_table)
}
ds.asNumeric(paste0(bucket_table,'$CENSOR'),paste0(my_outcome,'_cens'), datasources = opals)
ds.asFactor(paste0(bucket_table, '$TIME.PERIOD'),'tid.f', datasources = opals)
# Check the time buckets for problems and fix them before going on
checkFactoredTime(studies = opals)
ds.assign(toAssign=paste0('log(',bucket_table,'$SURVTIME)'), newobj='logSurvival', datasources = opals)
ds.cbind(x=c(bucket_table,'tid.f', 'logSurvival'), newobj=bucket_table, datasources = opals)

# ___  ___          _      _   __  
# |  \/  |         | |    | | /  | 
# | .  . | ___   __| | ___| | `| | 
# | |\/| |/ _ \ / _` |/ _ \ |  | | 
# | |  | | (_) | (_| |  __/ | _| |_
# \_|  |_/\___/ \__,_|\___|_| \___/

# Exposure: total fish (g/d) at baseline
# Outcome: CASE_OBJ
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
#my_exposure = c('FATTY')
#my_exposure = c('LEAN')
#my_exposure = c('SALT')
#my_exposure = c('FRESH')
#my_exposure = c('NONFISH')
#my_exposure = c('FRIED')
#my_exposure = c('SSD')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", "REGION_CH")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run (normally blank for model 1!!)

#studies_model1 = which( names(temp_opals) %in% c("InterAct_germany") )

#temp_opals = temp_opals[-studies_model1]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_ckb_test_2_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)

model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_ckb_test_2_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

####################################################

# Exposure: total fish (g/d) at baseline
# Outcome: CASE_OBJ_SELF
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_outcome = c('CASE_OBJ_SELF')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_SELF'

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_SMC_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_SMC_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

####################################


####################################################
## Stratified analyses by quartile

quartile_studies = study_names[! study_names %in% c("CARDIA", "Zutphen", "HOORN", "Whitehall")]
opals_quartiles = opals[quartile_studies]
rev(opals_quartiles)


temp_quants = ds.quantileMean(x=paste0("D8$", my_exposure), type = 'split', datasources = opals_quartiles)
quant_df = t(as.data.frame(temp_quants))
for (x in c(1:length(row.names(quant_df)))) {
  ds.subset(x='D8',subset = 'D8_l50', logicalOperator = paste0(my_exposure, '<'), threshold = quant_df[x,'50%'], datasources = opals_quartiles[x])
  ds.subset(x='D8',subset = 'D8_h50', logicalOperator = paste0(my_exposure, '>='), threshold = quant_df[x,'50%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_l50',subset = 'D8_l25', logicalOperator = paste0(my_exposure, '<'), threshold = quant_df[x,'25%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_l50',subset = 'D8_h25', logicalOperator = paste0(my_exposure, '>='), threshold = quant_df[x,'25%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_h50',subset = 'D8_l75', logicalOperator = paste0(my_exposure, '<'), threshold = quant_df[x,'75%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_h50',subset = 'D8_h75', logicalOperator = paste0(my_exposure, '>='), threshold = quant_df[x,'75%'], datasources = opals_quartiles[x])
}


ref_table = 'D8_l25'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q1.svg')
model_1_Q1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q1_alltuned = model_1_Q1[[1]]
model_1_Q1_remtuned = model_1_Q1[[2]]
write.csv(x = model_1_Q1_alltuned[model_1_Q1_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q1_survivaltuned.csv')

ref_table = 'D8_h25'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q2.svg')
model_1_Q2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q2_alltuned = model_1_Q2[[1]]
model_1_Q2_remtuned = model_1_Q2[[2]]
write.csv(x = model_1_Q2_alltuned[model_1_Q2_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q2_survivaltuned.csv')

ref_table = 'D8_l75'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q3.svg')
model_1_Q3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q3_alltuned = model_1_Q3[[1]]
model_1_Q3_remtuned = model_1_Q3[[2]]
write.csv(x = model_1_Q3_alltuned[model_1_Q3_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q3_survivaltuned.csv')

ref_table = 'D8_h75'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q4.svg')
model_1_Q4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q4_alltuned = model_1_Q4[[1]]
model_1_Q4_remtuned = model_1_Q4[[2]]
write.csv(x = model_1_Q4_alltuned[model_1_Q4_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q4_survivaltuned.csv')

##################################
#Servings analysis
# This yields an HR for the highest consumers vs lowest

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('SERVINGS')


my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'
#ref_table =  'A_OBJ_SELF'



#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run (normally blank for model 1!!)

#studies_model1 = which( names(temp_opals) %in% c("InterAct_germany") )

#temp_opals = temp_opals[-studies_model1]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
#nb grep to get all categorical variables with SERVINGS
write.csv(x = model_1_alltuned[grep(my_exposure, model_1_alltuned$cov),], file = paste0('~/plots/test/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



# ___  ___          _      _   _____ 
# |  \/  |         | |    | | / __  \
# | .  . | ___   __| | ___| | `' / /'
# | |\/| |/ _ \ / _` |/ _ \ |   / /  
# | |  | | (_) | (_| |  __/ | ./ /___
# \_|  |_/\___/ \__,_|\___|_| \_____/
# Model 2a: As model 1 + adj for energy intake, alcohol intake, fibre intake, meat intake, 

#     fruit intake, vegetables intake, sugary drinks intake


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
#my_exposure = c('SERVINGS')
#my_exposure = c('FATTY')
#my_exposure = c('LEAN')
#my_exposure = c('SALT')
#my_exposure = c('FRESH')
#my_exposure = c('NONFISH')
#my_exposure = c('SSD')
#my_exposure = c('FRIED')



my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                 "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'
#ref_table =  'A_OBJ_SELF'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )
#studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab") )

temp_opals = temp_opals[-studies_model2]

# For lean, exclude more

#studies_model2 = which( names(temp_opals) %in% c( "InterAct_germany") )
#studies_model2 = which( names(temp_opals) %in% c( "JPHC") )

#temp_opals = temp_opals[-studies_model2]


mypath = file.path('~', 'plots/test', paste0('model_2_CKB_fix_',my_exposure,'_',ref_table,'.svg'))
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[grep(my_exposure, model_2_alltuned$cov),], file = paste0('~/plots/test/model_2_CKB_fix_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


##############################
#Regional split 

regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                               "InterAct_denmark","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "AusDiab"))

for_RMA = model_2_alltuned[model_2_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('~', 'plots', paste0('model_2_survivaltuned_', names(regions[z]), '_SELF.svg'))
  svg(filename=mypath, 
      width=4.5 * length(my_exposure), 
      height=3.5 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  do_REM(coeffs = temp_data$Estimate, s_err = temp_data$`Std. Error`, labels = temp_data$study,fmla = "see main plot", out_family = 'poisson', variable = my_exposure)
  dev.off()
  
}

####################################################
## Stratified analyses by quartile

quartile_studies = study_names[! study_names %in% c("CARDIA", "Zutphen", "HOORN", "Whitehall", "InterAct_france",
                                                    "NHAPC", "AusDiab", "PRHHP")]
opals_quartiles = opals[quartile_studies]
rev(opals_quartiles)

my_exposure = c('QRT')

ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_survivaltuned_qrt_obj.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_model2)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[grep(my_exposure, model_2_alltuned$cov),], file = '~/plots/model_2_survivaltuned_qrt_obj.csv')



temp_quants = ds.quantileMean(x=paste0("D8$", my_exposure), type = 'split', datasources = opals_quartiles)
quant_df = t(as.data.frame(temp_quants))
for (x in c(1:length(row.names(quant_df)))) {
  ds.subset(x='D8',subset = 'D8_l50', logicalOperator = paste0(my_exposure, '<'), threshold = quant_df[x,'50%'], datasources = opals_quartiles[x])
  ds.subset(x='D8',subset = 'D8_h50', logicalOperator = paste0(my_exposure, '>='), threshold = quant_df[x,'50%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_l50',subset = 'D8_l25', logicalOperator = paste0(my_exposure, '<'), threshold = quant_df[x,'25%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_l50',subset = 'D8_h25', logicalOperator = paste0(my_exposure, '>='), threshold = quant_df[x,'25%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_h50',subset = 'D8_l75', logicalOperator = paste0(my_exposure, '<'), threshold = quant_df[x,'75%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_h50',subset = 'D8_h75', logicalOperator = paste0(my_exposure, '>='), threshold = quant_df[x,'75%'], datasources = opals_quartiles[x])
}


ref_table = 'D8_l25'
#mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q1_SELF.svg')
mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q1.svg')
model_2_Q1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_2_Q1_alltuned = model_2_Q1[[1]]
model_2_Q1_remtuned = model_2_Q1[[2]]
#write.csv(x = model_2_Q1_alltuned[model_2_Q1_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q1_survivaltuned_SELF.csv')
write.csv(x = model_2_Q1_alltuned[model_2_Q1_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q1_survivaltuned.csv')

ref_table = 'D8_h25'
#mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q2_SELF.svg')
mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q2.svg')
model_2_Q2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_2_Q2_alltuned = model_2_Q2[[1]]
model_2_Q2_remtuned = model_2_Q2[[2]]
#write.csv(x = model_2_Q2_alltuned[model_2_Q2_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q2_survivaltuned_SELF.csv')
write.csv(x = model_2_Q2_alltuned[model_2_Q2_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q2_survivaltuned.csv')

ref_table = 'D8_l75'
#mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q3_SELF.svg')
mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q3.svg')
model_2_Q3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_2_Q3_alltuned = model_2_Q3[[1]]
model_2_Q3_remtuned = model_2_Q3[[2]]
#write.csv(x = model_2_Q3_alltuned[model_2_Q3_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q3_survivaltuned_SELF.csv')
write.csv(x = model_2_Q3_alltuned[model_2_Q3_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q3_survivaltuned.csv')

ref_table = 'D8_h75'
#mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q4_SELF.svg')
mypath = file.path('~', 'plots', 'model_2_survivaltuned_Q4.svg')
model_2_Q4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_2_Q4_alltuned = model_2_Q4[[1]]
model_2_Q4_remtuned = model_2_Q4[[2]]
#write.csv(x = model_2_Q4_alltuned[model_2_Q4_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q4_survivaltuned_SELF.csv')
write.csv(x = model_2_Q4_alltuned[model_2_Q4_alltuned$cov==my_exposure,], file = '~/plots/model_2_Q4_survivaltuned.csv')

##################################
#Servings analysis
# This yields an HR for the highest consumers vs lowest

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('SERVINGS')


my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )
#studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab") )

temp_opals = temp_opals[-studies_model2]

# For lean, exclude more

#studies_model2 = which( names(temp_opals) %in% c( "InterAct_germany") )
#studies_model2 = which( names(temp_opals) %in% c( "JPHC") )

#temp_opals = temp_opals[-studies_model2]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_2_',my_exposure,'_',ref_table,'.svg'))
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
#nb grep to get all categorical variables with SERVINGS
write.csv(x = model_2_alltuned[grep(my_exposure, model_2_alltuned$cov),], file = paste0('~/plots/test/model_2_REG_CKB_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

#### dose response for servings

my_exposure = c('TOTAL')


ds.subset(x=ref_table, subset = 'serve1', logicalOperator = 'SERVINGS==', threshold = 1, datasources = opals)
ds.subset(x=ref_table, subset = 'serve2', logicalOperator = 'SERVINGS==', threshold = 2, datasources = opals)
ds.subset(x=ref_table, subset = 'serve3', logicalOperator = 'SERVINGS==', threshold = 3, datasources = opals)
ds.subset(x=ref_table, subset = 'serve4', logicalOperator = 'SERVINGS==', threshold = 4, datasources = opals)

my_vars_check = c(my_exposure, my_outcome)

temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model2]

# won't run for servings

studies_model2 = which( names(temp_opals) %in% c( "PRHHP", "InterAct_france") )

temp_opals = temp_opals[-studies_model2]

mypath = file.path('~', 'plots/test', paste0('model_2_',my_exposure,'_serve1.svg'))
model_2 = tunedSurvivalModel('serve1', my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_2_',my_exposure,'_serve1.csv'))

rm(temp_opals)

my_vars_check = c(my_exposure, my_outcome)

temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model2]

# won't run for servings

studies_model2 = which( names(temp_opals) %in% c("CKB","NOWAC","NHAPC","CARDIA", "PRHHP") )

temp_opals = temp_opals[-studies_model2]


mypath = file.path('~', 'plots/test', paste0('model_2_',my_exposure,'_serve2.svg'))
model_2 = tunedSurvivalModel('serve2', my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_2_',my_exposure,'_serve2.csv'))

rm(temp_opals)

my_vars_check = c(my_exposure, my_outcome)

temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model2]

# won't run for servings

studies_model2 = which( names(temp_opals) %in% c("CKB", "FMC", "NOWAC", "Whitehall", "PRHHP") )

temp_opals = temp_opals[-studies_model2]


mypath = file.path('~', 'plots/test', paste0('model_2_',my_exposure,'_serve3.svg'))
model_2 = tunedSurvivalModel('serve3', my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_2_',my_exposure,'_serve3.csv'))

rm(temp_opals)

my_vars_check = c(my_exposure, my_outcome)

temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model2 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model2]

# won't run for servings

studies_model2 = which( names(temp_opals) %in% c("InterAct_netherlands") )

temp_opals = temp_opals[-studies_model2]

mypath = file.path('~', 'plots/test', paste0('model_2_',my_exposure,'_serve4.svg'))
model_2 = tunedSurvivalModel('serve4', my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_2_',my_exposure,'_serve4.csv'))

rm(temp_opals)


# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |____ |
# | .  . | ___   __| | ___| |     / /
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | .___/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 
# sensitivity analysis

####################################################
# Model 3a: As model 2 + adj for family history of diabetes
# Model Specific Setup
####################################################


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "FAM_DIAB", "REGION_CH")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen", "NHAPC") )

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("Golestan", "InterAct_italy", "InterAct_spain","MESA", "NOWAC"))

temp_opals = temp_opals[-studies_model3]


# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3a_FAM_DIAB_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3a_FAM_DIAB_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

## Stratified analyses by sex (men, women) if significant
# Men
ds.subset(x = 'A_OBJ', subset = 'A_OBJ_men', logicalOperator = 'SEX==', threshold = 0, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')


my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH", "FAM_DIAB")

#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")



my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_men'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen", "NHAPC") )

temp_opals = temp_opals[-studies_model3]

# exclude studies with women only as will not run model 3 for men

studies_model3 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI", "CARDIA", "InterAct_germany"))

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("Golestan", "InterAct_italy", "InterAct_spain","MESA", "NOWAC"))

temp_opals = temp_opals[-studies_model3]


# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3a_men_CKB_MESA_','FAM_DIAB','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3a_men_CKB_MESA_FAM_DIAB_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

## Stratified analyses by sex (men, women) if significant
# Women
ds.subset(x = 'A_OBJ', subset = 'A_OBJ_women', logicalOperator = 'SEX==', threshold = 1, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')


my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH", "FAM_DIAB")

#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")



my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_women'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen", "NHAPC") )

temp_opals = temp_opals[-studies_model3]

# exclude studies with women only as will not run model 3 for men

studies_model3 = which( names(temp_opals) %in% c("PRHHP"))

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("Golestan", "InterAct_italy", "InterAct_spain","MESA", "NOWAC"))

temp_opals = temp_opals[-studies_model3]


# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3a_women_CKB_MESA_','FAM_DIAB','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3a_women_CKB_MESA_FAM_DIAB_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

####################################################
# Model 3b: As model 2 + adj for waist circumference
####################################################
# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "WAIST", "REGION_CH")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("FMC", "JPHC", "NOWAC", "PRHHP"))

temp_opals = temp_opals[-studies_model3]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3b_','WAIST','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3b_','WAIST','_', my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

#Equivalent model 2

my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("FMC", "Zutphen", "JPHC", "NOWAC", "PRHHP"))

temp_opals = temp_opals[-studies_model3]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_2b_SMC_','WAIST','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_2b_SMC_','WAIST','_', my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

### Model 3b for men

my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "WAIST", "REGION_CH")

#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")


my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_men'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model3]

# exclude studies with women only as will not run model 3 for men

studies_model3 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI", "CARDIA","InterAct_germany", "InterAct_italy"))

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("FMC", "JPHC", "NOWAC", "PRHHP"))

temp_opals = temp_opals[-studies_model3]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3b_men_CKB_MESA_','WAIST','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3b_men_CKB_MESA_','WAIST','_', my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

### model 3b for women

my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "WAIST", "REGION_CH")

#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")


my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_women'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model3]

# exclude studies with women only as will not run model 3 for men

studies_model3 = which( names(temp_opals) %in% c("PRHHP"))

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("FMC", "JPHC", "NOWAC", "PRHHP"))

temp_opals = temp_opals[-studies_model3]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3b_women_CKB_MESA_','WAIST','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3b_women_CKB_MESA_','WAIST','_', my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

####################################################
# Model 3c: As model 2 + adj for fish oil supplements
####################################################
# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "REGION_CH")


my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run from model 2

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("AusDiab", "FMC", "JPHC", "Zutphen", "PRHHP", "WHI", "ARIC", "CARDIA", "MESA",
                                                 "InterAct_italy", "InterAct_spain", "InterAct_denmark", "InterAct_france",
                                                 "InterAct_uk", "InterAct_sweden", "InterAct_germany", "InterAct_netherlands", "Golestan"
))

temp_opals = temp_opals[-studies_model3]


# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3c_','SUPPLEMENTS','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3c_SUPPLEMENTS_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

### Model 3c men

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "REGION_CH")

#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")


my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_men'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model3]

# exclude studies with women only as will not run model 3 for men

studies_model3 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI", "CARDIA","InterAct_germany", "InterAct_italy"))

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("AusDiab", "FMC", "JPHC", "Zutphen", "PRHHP", "WHI", "ARIC", "CARDIA", "MESA",
                                                 "InterAct_italy", "InterAct_spain", "InterAct_denmark", "InterAct_france",
                                                 "InterAct_uk", "InterAct_sweden", "InterAct_germany", "InterAct_netherlands", "Golestan"
))
temp_opals = temp_opals[-studies_model3]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3c_men_CKB_MESA_','SUPPLEMENTS','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3c_men_CKB_MESA_','SUPPLEMENTS','_', my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


### Model 3c women


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "REGION_CH")

#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")


my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_women'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model3 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model3]

# exclude studies with men only as will not run model 3 for women

studies_model3 = which( names(temp_opals) %in% c("PRHHP"))

temp_opals = temp_opals[-studies_model3]

# studies with missing variable - need to forcibly leave them out or they get included without the missing variable

studies_model3 = which( names(temp_opals) %in% c("AusDiab", "FMC", "JPHC", "Zutphen", "PRHHP", "WHI", "ARIC", "CARDIA", "MESA",
                                                 "InterAct_italy", "InterAct_spain", "InterAct_denmark", "InterAct_france",
                                                 "InterAct_uk", "InterAct_sweden", "InterAct_germany", "InterAct_netherlands", "Golestan"
))
temp_opals = temp_opals[-studies_model3]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_3c_women_CKB_MESA_','SUPPLEMENTS','_', my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_3c_women_CKB_MESA_','SUPPLEMENTS','_', my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

# ___  ___          _      _     ___ 
# |  \/  |         | |    | |   /   |
# | .  . | ___   __| | ___| |  / /| |
# | |\/| |/ _ \ / _` |/ _ \ | / /_| |
# | |  | | (_) | (_| |  __/ | \___  |
# \_|  |_/\___/ \__,_|\___|_|     |_/

# Exposure: total fish (g/d) at baseline*sex
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, 
#             fibre intake, meat intake, fruit intake, vegetables intake, sugary drinks intake.


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SEX", "REGION_CH")


my_interaction = "SEX"
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run from model 2

studies_model4 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model4]

# exclude studies with men or women only as will not run model 4

studies_model4 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI", "PRHHP"))

temp_opals = temp_opals[-studies_model4]

# exclude studies that will not run model 4 after gender split

#studies_model4 = which( names(temp_opals) %in% c("CARDIA", "InterAct_germany", "InterAct_italy"))

#temp_opals = temp_opals[-studies_model4]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_4_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedInterActionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath,interaction_term = my_interaction, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/test/model_4_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



####################################################
## Stratified analyses by sex (men, women) if significant

ds.subset(x = 'A_OBJ', subset = 'A_OBJ_men', logicalOperator = 'SEX==', threshold = 0, datasources = opals)
#ds.subset(x = 'A_OBJ_SELF', subset = 'A_OBJ_SELF_men', logicalOperator = 'SEX==', threshold = 0, datasources = opals)

ds.subset(x = 'A_OBJ', subset = 'A_OBJ_women', logicalOperator = 'SEX==', threshold = 1, datasources = opals)
#ds.subset(x = 'A_OBJ_SELF', subset = 'A_OBJ_SELF_women', logicalOperator = 'SEX==', threshold = 1, datasources = opals)

# Men

# Also need to choose between outcome OBJ or OBJ_SELF
#my_exposure = c('TOTAL')
my_exposure = c('SERVINGS')

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')

my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")
#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", "REGION_CH")


ref_table =  'A_OBJ_men'
#ref_table =  'A_OBJ_SELF_men'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run from model 2

studies_model4 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model4]

# exclude studies with women only as will not run model 4 for men
studies_model4 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI"))
temp_opals = temp_opals[-studies_model4]

# exclude studies that will not run model 4 after gender split for SERVINGS
studies_model4 = which( names(temp_opals) %in% c("CARDIA", "InterAct_italy"))
temp_opals = temp_opals[-studies_model4]

# exclude studies that will not run model 4 after gender split for TOTAL, FATTY, LEAN
#studies_model4 = which( names(temp_opals) %in% c("CARDIA", "InterAct_germany", "InterAct_italy"))
#temp_opals = temp_opals[-studies_model4]

# exclude studies that will not run model 4 after gender split for TOTAL, FATTY, LEAN, SERVINGS for secondary
#studies_model4 = which( names(temp_opals) %in% c("InterAct_germany", "InterAct_italy"))
#temp_opals = temp_opals[-studies_model4]

# exclude studies that will not run model 4 after gender split for FRIED

#studies_model4 = which( names(temp_opals) %in% c("CARDIA", "InterAct_italy"))
#temp_opals = temp_opals[-studies_model4]

# exclude studies that will not run model 4 after gender split for NONFISH

#studies_model4 = which( names(temp_opals) %in% c("CARDIA","InterAct_italy"))
#temp_opals = temp_opals[-studies_model4]

temp_opals = rev(temp_opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_4_men_corr_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/test/model_4_men_corr_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


####################################################
# Women


# Also need to choose between outcome OBJ or OBJ_SELF
#my_exposure = c('TOTAL')
my_exposure = c('SERVINGS')

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')

my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")
#my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", "REGION_CH")


ref_table =  'A_OBJ_women'
#ref_table =  'A_OBJ_SELF_women'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run from model 2

studies_model4 = which( names(temp_opals) %in% c( "HOORN", "Zutphen") )

temp_opals = temp_opals[-studies_model4]

# exclude studies with men or women only as will not run model 4

#studies_model4 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI", "PRHHP"))

studies_model4 = which( names(temp_opals) %in% c("PRHHP"))
temp_opals = temp_opals[-studies_model4]

# exclude studies that will not run model 4 after gender split for TOTAL, FATTY, LEAN

#studies_model4 = which( names(temp_opals) %in% c("InterAct_germany", "InterAct_italy"))

#temp_opals = temp_opals[-studies_model4]


# exclude studies that will not run model 4 with SERVINGS

studies_model4 = which( names(temp_opals) %in% c("SUN"))

temp_opals = temp_opals[-studies_model4]

temp_opals = rev(temp_opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_4_women_corr_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/test/model_4_women_corr_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



#__  __               _          _     _  _           
#|  \/  |   ___     __| |   ___  | |   | || |     __ _ 
#| |\/| |  / _ \   / _` |  / _ \ | |   | || |_   / _` |
#| |  | | | (_) | | (_| | |  __/ | |   |__   _| | (_| |
#|_|  |_|  \___/   \__,_|  \___| |_|      |_|    \__,_|


# Exposure: total fish (g/d) at baseline*waist
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, 
#             fibre intake, meat intake, fruit intake, vegetables intake, sugary drinks intake. PLUS WAIST


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SEX", "WAIST")
my_interaction = "SEX"
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run from model 2

studies_model4 = which( names(temp_opals) %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab") )

temp_opals = temp_opals[-studies_model4]

# exclude studies with men or women only as will not run model 4

studies_model4 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI", "PRHHP"))

temp_opals = temp_opals[-studies_model4]

# from interaction models - wont run WAIST

studies_model4 = which( names(temp_opals) %in% c("CKB"))

temp_opals = temp_opals[-studies_model4]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_4_WAIST_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedInterActionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath,interaction_term = my_interaction, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[model_4_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_4_WAIST_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

####################################################
## Stratified analyses by sex (men, women) if significant
# Men
ds.subset(x = 'A_OBJ', subset = 'A_OBJ_men', logicalOperator = 'SEX==', threshold = 0, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "WAIST")

ref_table =  'A_OBJ_men'

#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run, women only

studies_model4 = which( names(temp_opals) %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab") )

temp_opals = temp_opals[-studies_model4]

# exclude studies with women only as will not run model 4

studies_model4 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI"))

temp_opals = temp_opals[-studies_model4]

# won't run model 4

studies_model4 = which( names(temp_opals) %in% c("ELSA","InterAct_italy"))

temp_opals = temp_opals[-studies_model4]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_4_WAIST_men_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[model_4_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_4_WAIST_men_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


####################################################
# Women
ds.subset(x = 'A_OBJ', subset = 'A_OBJ_women', logicalOperator = 'SEX==', threshold = 1, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "WAIST")

ref_table =  'A_OBJ_women'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run, women only

studies_model4 = study_names[! study_names %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab",
                                                   "PRHHP",
                                                   "InterAct_italy",
                                                   "PRHHP")]

temp_opals = temp_opals[studies_model4]

# won't run model 4

studies_model4 = which( names(temp_opals) %in% c("InterAct_netherlands"))

temp_opals = temp_opals[-studies_model4]

# tuned survival version

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_4_WAIST_women_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[model_4_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_4_WAIST_women_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |  ___|
# | .  . | ___   __| | ___| | |___ \ 
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 

# Exposure: total fish (g/d) at baseline*BMI
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, fibre intake, red and processed meat intake, fruit intake, vegetables intake, sugary drinks intake.
# 
# Stratified analyses by BMI (BMI<25, BMI ≥25) if significant

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SEX", "REGION_CH")
my_interaction = "BMI"

ref_table =  'A_OBJ'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model5 = study_names[! study_names %in% c( "HOORN", "Zutphen")]

temp_opals = temp_opals[studies_model5]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_5_CKB_MESA_',my_exposure,'_',ref_table,'.svg'))
model_5 = tunedInterActionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath,interaction_term = my_interaction, studies = temp_opals)
model_5_alltuned = model_5[[1]]
model_5_remtuned = model_5[[2]]
write.csv(x = model_5_alltuned[grep(my_exposure, model_5_alltuned$cov),], file = paste0('~/plots/test/model_5_CKB_MESA_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

####################################################
## Stratified analyses by sex (men, women) if significant
# Men
ds.subset(x = 'A_OBJ', subset = 'A_OBJ_men', logicalOperator = 'SEX==', threshold = 0, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")
my_interaction = "BMI"

ref_table =  'A_OBJ_men'

#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies that won't run

studies_model4 = which( names(temp_opals) %in% c( "HOORN", "Zutphen","ELSA","CARDIA", "InterAct_germany", "InterAct_italy") )

temp_opals = temp_opals[-studies_model4]

# exclude studies with women only as will not run model 4

studies_model4 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI"))

temp_opals = temp_opals[-studies_model4]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_5_men_CKB_MESA_',my_exposure,'_',ref_table,'.svg'))
model_5 = tunedInterActionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath,interaction_term = my_interaction, studies = temp_opals)
model_5_alltuned = model_5[[1]]
model_5_remtuned = model_5[[2]]
write.csv(x = model_5_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/test/model_5_men_CKB_MESA_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



####################################################
# Women
ds.subset(x = 'A_OBJ', subset = 'A_OBJ_women', logicalOperator = 'SEX==', threshold = 1, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "REGION_CH")
my_interaction = "BMI"

ref_table =  'A_OBJ_women'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run, women only

studies_model4 = study_names[! study_names %in% c( "HOORN", "Zutphen","CARDIA",
                                                   "InterAct_italy",
                                                   "PRHHP")]

temp_opals = temp_opals[studies_model4]

# won't run model 4

studies_model4 = which( names(temp_opals) %in% c("InterAct_netherlands", "InterAct_spain", "ARIC"))

temp_opals = temp_opals[-studies_model4]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_5_women_CKB_MESA_',my_exposure,'_',ref_table,'.svg'))
model_5 = tunedInterActionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath,interaction_term = my_interaction, studies = temp_opals)
model_5_alltuned = model_5[[1]]
model_5_remtuned = model_5[[2]]
write.csv(x = model_5_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/test/model_5_women_CKB_MESA_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


####################################################
# BMI < 25
ds.subset(x = 'D8', subset = 'underweight', logicalOperator = 'BMI==', threshold = 0, datasources = opals)
men <- ds.length('underweight$SEX', type = 'split', datasources = opals)
ref_table = 'underweight'
mypath = file.path('~', 'plots', 'model_5_underweight_surv.svg')
model_underweight = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2),  studies = opals_model5)
model_underweight_all = model_underweight[[1]]
model_underweight_rem = model_underweight[[2]]

####################################################
# BMI >= 25
ds.subset(x = 'D8', subset = 'overweight', logicalOperator = 'BMI==', threshold = 1, datasources = opals)
women <- ds.length('overweight$SEX', type = 'split', datasources = opals)
ref_table = 'overweight'
mypath = file.path('~', 'plots', 'model_5_overweight_surv.svg')
model_overweight = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2),  studies = opals_model5)
model_overweight_all = model_overweight[[1]]
model_overweight_rem = model_overweight[[2]]


# ___  ___          _      _    ____ 
# |  \/  |         | |    | |  / ___|
# | .  . | ___   __| | ___| | / /___ 
# | |\/| |/ _ \ / _` |/ _ \ | | ___ \
# | |  | | (_) | (_| |  __/ | | \_/ |
# \_|  |_/\___/ \__,_|\___|_| \_____/



# Exposure: total fish (g/d) at baseline
# Outcome: CASE_OBJ
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities
# To assess the impact of each confounder we will also run models including each confounder separately.

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
#my_outcome = c('CASE_OBJ')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
#my_exit_col = c('newEndDate')
my_exit_col = c('newEndDate_SELF')

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals)

# Present analyses by geographical area (Central area, Eastern area, Western area)
# subset opals list by geographic area then carry out regression for each one on their own.
opals_central = opals[c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                        "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                        "InterAct_denmark","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen")]
opals_western = opals[c("ELSA", "WHI")]
opals_eastern = opals[c("NHAPC", "JPHC", "AusDiab")]
#opals_eastern = opals[c("NHAPC", "AusDiab")]
# Assign country code to each of the studies
# Adding in the weights as described by Dr. Burton
######################################################
# central area
######################################################
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_6_central_surv_SELF.svg')
model_6central = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_central)
model_6central_all = model_6central[[1]]
model_6central_rem = model_6central[[2]]
write.csv(x = model_6central_rem[model_6central_rem$cov==my_exposure,], file = '~/plots/model_6central.csv')

######################################################
# eastern area
######################################################
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_6_eastern_surv_SELF.svg')
model_6eastern = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_eastern)
model_6eastern_all = model_6eastern[[1]]
model_6eastern_rem = model_6eastern[[2]]
write.csv(x = model_6eastern_rem[model_6eastern_rem$cov==my_exposure,], file = '~/plots/model_6eastern.csv')

######################################################
# western area
######################################################
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_6_western_surv_SELF.svg')
model_6western = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_western)
model_6western_all = model_6western[[1]]
model_6western_rem = model_6western[[2]]
write.csv(x = model_6western_rem[model_6western_rem$cov==my_exposure,], file = '~/plots/model_6western.csv')

# Meta regression
# inside of a meta regression you take the regression coefficient values and regress them against another trait (like the geographical area)
# and then look at the coefficients here.
# we can just use the regression coefficients created out of the values and 
# then do local linear regression to see the relationship between the variables.
central_estimates = extractExposureCoefficientGroup(outcome = "CASE_OBJ_SELF", exposure = "TOTAL", data_table = model_6central_all, studies = opals_central)
western_estimates = extractExposureCoefficientGroup(outcome = "CASE_OBJ_SELF", exposure = "TOTAL", data_table = model_6western_all, studies = opals_western)
eastern_estimates = extractExposureCoefficientGroup(outcome = "CASE_OBJ_SELF", exposure = "TOTAL", data_table = model_6eastern_all, studies = opals_eastern)
all_estimates = c(central_estimates, western_estimates, eastern_estimates)
central_codes = rep(1, times = length(opals_central))
western_codes = rep(2, times = length(opals_western))
eastern_codes = rep(3, times = length(opals_eastern))
geo_codes = as.factor(x = c(central_codes, western_codes, eastern_codes))

meta_fmla = as.formula("all_estimates ~  geo_codes")
meta_regression_model = glm(formula = meta_fmla)


#_______  _______  ______   _______  _           ______
#(       )(  ___  )(  __  \ (  ____ \( \        / ___  \ 
#| () () || (   ) || (  \  )| (    \/| (        \/   )  )
#| || || || |   | || |   ) || (__    | |            /  / 
#| |(_)| || |   | || |   | ||  __)   | |           /  /  
#| |   | || |   | || |   ) || (      | |          /  /   
#| )   ( || (___) || (__/  )| (____/\| (____/\   /  /    
#|/     \|(_______)(______/ (_______/(_______/   \_/     


# Model 7: As model 2 but split into <10 years follow up + adj for energy intake, alcohol intake, fibre intake, meat intake, 

#     fruit intake, vegetables intake, sugary drinks intake

# do the subset - less than 10 years FUP

ds.subset(x = 'A_OBJ', subset = 'A_OBJ_less10', logicalOperator = 'FUP_OBJ<', threshold = 10, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_less10'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run from model 2

studies_model7 = which( names(temp_opals) %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab") )

temp_opals = temp_opals[-studies_model7]

# exclude studies with insufficient cases to run from model 7

studies_model7 = which( names(temp_opals) %in% c( "ARIC") )

temp_opals = temp_opals[-studies_model7]

mypath = file.path('~', 'plots/test', paste0('model_7_',my_exposure,'_',ref_table,'.svg'))
model_7 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_7_alltuned = model_7[[1]]
model_7_remtuned = model_7[[2]]
write.csv(x = model_7_alltuned[model_7_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_7_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

##################################################

# do the subset - more than 10 years FUP

ds.subset(x = 'A_OBJ', subset = 'A_OBJ_more10', logicalOperator = 'FUP_OBJ>=', threshold = 10, datasources = opals)

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ_more10'

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model7 = which( names(temp_opals) %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab") )

temp_opals = temp_opals[-studies_model7]

# exclude studies with insufficient cases to run

studies_model7 = which( names(temp_opals) %in% c("CKB", "Golestan", "MESA", "JPHC", "NOWAC", "ELSA",
                                                 "SMC", "WHI", "InterAct_denmark", "InterAct_france",
                                                 "InterAct_germany", "InterAct_italy", "InterAct_netherlands"
                                                 , "InterAct_sweden", "InterAct_uk"))

temp_opals = temp_opals[-studies_model7]

mypath = file.path('~', 'plots/test', paste0('model_7_',my_exposure,'_',ref_table,'.svg'))
model_7 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_7_alltuned = model_7[[1]]
model_7_remtuned = model_7[[2]]
write.csv(x = model_7_alltuned[model_7_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_7_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

