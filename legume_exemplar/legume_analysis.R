## Analysis script for Legume Exemplar Analysis
## Author: Tom Bishop
##		   
## Date: 14/02/2018

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

setwd("/home/l_trpb2/git/exemplar_analyses/")

# Source in the Extra functions for analysis

source("variable_functions.R")
#source("helperFunctions.R")
source("survival_analysis_dsFunctions.R")

# Retrieve Credential Details
source("creds/leg_exemplar_creds.R")

# complete cases per study only on variables known to be not all missing
# the _vars.csv file stores information about which variables a study is expected to have and
# which variables are completely missing for all participants
# also get time bucket information (bespoke to study and exemplar)
# the buckets file contains defined buckets for the piecewise poisson analysis
setwd("/home/l_trpb2/git/exemplar_analyses/legume_exemplar")
filter_csv = read.csv(file = 'leg_opal_vars.csv',  header=TRUE, row.names = 1 )
source("leg_buckets.R")
setwd("~")


# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals = opals)

#all the variables in the analysis
myvars = c(
  'AGE_BASE', 'TYPE_DIAB', 'PREV_DIAB', 'CASE_OBJ_SELF', 'CASE_OBJ', 'FUP_OBJ', 'FUP_OBJ_SELF', 'PBCL',
  'SOY', 'NUTS_SEEDS', 'ISOFLAVONES', 'TOTAL', 'SEX', 'BMI', 'EDUCATION', 'SMOKING', 'PA', 'ALCOHOL',
  'FAM_DIAB', 'COMORBIDITY', 'E_INTAKE', 'COV_FRUIT', 'COV_VEG', 'COV_FIBER', 'COV_MEAT', 'COV_SUG_BEVS', 'WAIST',
  'REGION_CH', 'BMI_CAT', 'CONSUMER', 'COV_DAIRY', 'COV_FISH'
)


# need to make changes here if using workspaces

opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/fish')

# if using workspaces
logindata_ws = logindata_all[c(2,9),]

opals <- datashield.login(logins=logindata_ws, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/fish')


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
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB!=', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# Remove CKB and ELSA_UK (no energy intake data)
opals_temp = opals
opals_temp["CKB"] = NULL
opals_temp["ELSA_UK"] = NULL

# In order to deal with the intake subsets stratified by sex we will have to create subsets of sex,
# do the intake subset and then rbind the groups back together. What follows is DataSHIELD magic
ds.asNumeric("E2$SEX", newobj = "sexNumbers", opals)
ds.assign(toAssign="(sexNumbers*300)+E2$E_INTAKE", newobj = "adjustedLowerBound", opals)
ds.assign(toAssign="(sexNumbers*700)+E2$E_INTAKE", newobj = "adjustedUpperBound", opals)
ds.cbind(x=c("adjustedLowerBound", "E2"), newobj = "L1", opals)
ds.cbind(x=c("adjustedUpperBound", "L1"), newobj = "L2", opals)
# remove participants with too little and excessive consumption of calories
ds.subset(x = 'L2', subset = 'E3', logicalOperator = 'adjustedUpperBound<=', threshold = 4200, datasources = opals)
under3500cal <- ds.length('E3$SEX', type = 'split', opals)
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'adjustedLowerBound>=', threshold = 800, datasources = opals)
afterIntake <- ds.length('E4$SEX', type = 'split', opals)

rm(opals_temp)

#exception for CKB and ELSA_UK - assume all ok
ds.assign(toAssign="E2", newobj = "E4", opals["CKB"])
ds.assign(toAssign="E2", newobj = "E4", opals["ELSA_UK"])
#generate under3500cal, afterIntake
under3500cal["CKB"] = noType1["CKB"]
afterIntake["CKB"] = noType1["CKB"]
under3500cal["ELSA_UK"] = noType1["ELSA_UK"]
afterIntake["ELSA_UK"] = noType1["ELSA_UK"]


#variables not to be used in complete cases
# stops loss of missing for these variables during complete cases, when they are not used in most models

none_cc_vars = c('tid.f','CENSOR', "WAIST", "FAM_DIAB")
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
  if(length(list_variables)>0){
    for (j in 1:length(list_variables)){
      
      var_name <- list_variables[j]
      #to_eval = paste0("ds.subset(x='D$",var_name,"', subset='", var_name,"', rows = c(1:", size, "), datasources = opals[",i,"])")
      to_eval2 = paste0("datashield.assign(opals[",i,"],'",var_name,"', quote(TRIM$",var_name,"))")
      eval(parse(text=to_eval2))
    }
    ds.cbind(x=c(list_variables, "E6"), newobj = "E7", opals[i])
  }
  else {
    datashield.assign(opals[i],"E7",quote(E6))
  }
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

### Optionally, do some saving of workspaces. HOORN and Whitehall don't work

# temp_opals = opals
# old_opal = which( names(temp_opals) %in% c("HOORN","Whitehall") )
# temp_opals = temp_opals[-old_opal]
# 
# datashield.workspace_save(opal=temp_opals, save = "full_non_sensitivity")
# rm(temp_opals)


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

### Optionally, do some saving of workspaces

temp_opals = opals
old_opal = which( names(temp_opals) %in% c("HOORN","Whitehall") )
temp_opals = temp_opals[-old_opal]

datashield.workspace_save(opal=temp_opals, save = "lexis_non_sensitivity")
rm(temp_opals)

# if using workspaces, option to load them. In this case we have done Whitehall and HOORN
# 27 is ELSA_UK which does not have objective measures
logindata_ws_all = logindata_all[-c(2,9,27),]

opals_ws <- datashield.login(logins=logindata_ws_all, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/fish', restore = "lexis_non_sensitivity")

opals = c(opals, opals_ws)
rm(opals_ws)

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

### Optionally, do some saving of workspaces

temp_opals = opals
old_opal = which( names(temp_opals) %in% c("HOORN","Whitehall") )
temp_opals = temp_opals[-old_opal]

datashield.workspace_save(opal=temp_opals, save = "lexis_non_sensitivity_self")
rm(temp_opals)




# ___  ___          _      _   __  
# |  \/  |         | |    | | /  | 
# | .  . | ___   __| | ___| | `| | 
# | |\/| |/ _ \ / _` |/ _ \ |  | | 
# | |  | | (_) | (_| |  __/ | _| |_
# \_|  |_/\___/ \__,_|\___|_| \___/

# Exposure: total legumes (g/d) at baseline
# Outcome: CASE_OBJ
# Sociodemographic, economic and lifestyle variables: age, sex, education, smoking, physical activity, alcohol intake, energy intake

# Also need to choose between outcome OBJ or OBJ_SELF
#my_exposure = c('TOTAL')
my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH")
#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "ALCOHOL")
#my_covariate =  c("AGE_BASE", "SEX")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  #studies_model1 = which( names(temp_opals) %in% c("HOORN","KOGES_ASAS", "Zutphen") )
  studies_model1 = which( names(temp_opals) %in% c("HOORN","KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model1]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ'){
  # exclusions for PBCL
  studies_model1 = which( names(temp_opals) %in% c("KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model1]
}

if (my_exposure == 'NUTS_SEEDS' & my_outcome == 'CASE_OBJ'){
  # exclusions for NUTS_SEEDS
  studies_model1 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model1]
}

if (my_exposure == 'SOY' & my_outcome == 'CASE_OBJ'){
  # exclusions for SOY
  studies_model1 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model1]
}

if (my_exposure == 'CONSUMER' & my_outcome == 'CASE_OBJ'){
  # exclusions for CONSUMER
  studies_model1 = which( names(temp_opals) %in% c("AusDiab", "HOORN", "KOGES_CAVAS", "KOGES_ASAS", "ARIC", "Zutphen") )
  temp_opals = temp_opals[-studies_model1]
}

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for TOTAL SELF
  studies_model1 = which( names(temp_opals) %in% c("HOORN", "Zutphen") )
  temp_opals = temp_opals[-studies_model1]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for PBCL SELF
  studies_model1 = which( names(temp_opals) %in% c("Zutphen") )
  temp_opals = temp_opals[-studies_model1]
  
}

# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/legumes/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


# ___  ___          _      _   _____ 
# |  \/  |         | |    | | / __  \
# | .  . | ___   __| | ___| | `' / /'
# | |\/| |/ _ \ / _` |/ _ \ |   / /  
# | |  | | (_) | (_| |  __/ | ./ /___
# \_|  |_/\___/ \__,_|\___|_| \_____/
# Model 2: As model 1 + adj for BMI


# Also need to choose between outcome OBJ or OBJ_SELF
#my_exposure = c('TOTAL')
my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI")


my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)


if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model2 = which( names(temp_opals) %in% c("HOORN","KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model2]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ'){
  # exclusions for PBCL
  studies_model2 = which( names(temp_opals) %in% c("KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model2]
}

if (my_exposure == 'NUTS_SEEDS' & my_outcome == 'CASE_OBJ'){
  # exclusions for NUTS_SEEDS
  studies_model2 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model2]
}

if (my_exposure == 'SOY' & my_outcome == 'CASE_OBJ'){
  # exclusions for SOY
  studies_model2 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS",  "Zutphen") )
  temp_opals = temp_opals[-studies_model2]
}

if (my_exposure == 'CONSUMER' & my_outcome == 'CASE_OBJ'){
  # exclusions for CONSUMER
  studies_model2 = which( names(temp_opals) %in% c("AusDiab", "HOORN", "KOGES_CAVAS", "KOGES_ASAS", "ARIC", "Zutphen") )
  temp_opals = temp_opals[-studies_model2]
}

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for TOTAL SELF
  studies_model2 = which( names(temp_opals) %in% c("HOORN", "Zutphen") )
  temp_opals = temp_opals[-studies_model2]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for PBCL SELF
  studies_model2 = which( names(temp_opals) %in% c("Zutphen") )
  temp_opals = temp_opals[-studies_model2]
}

mypath = file.path('~', 'plots/legumes', paste0('model_2_',my_exposure,'_',ref_table,'.svg'))
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = paste0('~/plots/legumes/model_2_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |____ |
# | .  . | ___   __| | ___| |     / /
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | .___/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 


####################################################
# Model 3: As model 2 + adj for family history of diabetes and comorbidity

####################################################

#my_exposure = c('TOTAL')
my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI","COMORBIDITY")


my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model3 = which( names(temp_opals) %in% c("HOORN","KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model3]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ'){
  # exclusions for PBCL
  studies_model3 = which( names(temp_opals) %in% c("KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model3]
}

if (my_exposure == 'NUTS_SEEDS' & my_outcome == 'CASE_OBJ'){
  # exclusions for NUTS_SEEDS
  studies_model3 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model3]
}

if (my_exposure == 'SOY' & my_outcome == 'CASE_OBJ'){
  # exclusions for SOY
  studies_model3 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS",  "Zutphen") )
  temp_opals = temp_opals[-studies_model3]
}

if (my_exposure == 'CONSUMER' & my_outcome == 'CASE_OBJ'){
  # exclusions for CONSUMER
  studies_model3 = which( names(temp_opals) %in% c("AusDiab", "HOORN", "KOGES_CAVAS", "KOGES_ASAS", "ARIC", "Zutphen") )
  temp_opals = temp_opals[-studies_model3]
}

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for TOTAL SELF
  studies_model3 = which( names(temp_opals) %in% c("HOORN", "Zutphen") )
  temp_opals = temp_opals[-studies_model3]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for PBCL SELF
  studies_model3 = which( names(temp_opals) %in% c("Zutphen") )
  temp_opals = temp_opals[-studies_model3]
}

mypath = file.path('~', 'plots/legumes', paste0('model_3_',my_exposure,'_',ref_table,'.svg'))
model_3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_3_alltuned = model_3[[1]]
model_3_remtuned = model_3[[2]]
write.csv(x = model_3_alltuned[model_3_alltuned$cov==my_exposure,], file = paste0('~/plots/legumes/model_3_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

# ___  ___          _      _     ___ 
# |  \/  |         | |    | |   /   |
# | .  . | ___   __| | ___| |  / /| |
# | |\/| |/ _ \ / _` |/ _ \ | / /_| |
# | |  | | (_) | (_| |  __/ | \___  |
# \_|  |_/\___/ \__,_|\___|_|     |_/


# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, 
#             fibre intake, fruit intake, vegetables intake, sugary drinks intake.


# Also need to choose between outcome OBJ or OBJ_SELF
#my_exposure = c('TOTAL')
my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY", "COV_FISH")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY", "COV_FISH")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model4 = which( names(temp_opals) %in% c("HOORN","KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model4]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ'){
  # exclusions for PBCL
  studies_model4 = which( names(temp_opals) %in% c("KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model4]
}

if (my_exposure == 'NUTS_SEEDS' & my_outcome == 'CASE_OBJ'){
  # exclusions for NUTS_SEEDS
  studies_model4 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "Zutphen") )
  temp_opals = temp_opals[-studies_model4]
}

if (my_exposure == 'SOY' & my_outcome == 'CASE_OBJ'){
  # exclusions for SOY
  studies_model4 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS",  "Zutphen") )
  temp_opals = temp_opals[-studies_model4]
}

if (my_exposure == 'CONSUMER' & my_outcome == 'CASE_OBJ'){
  # exclusions for CONSUMER
  studies_model4 = which( names(temp_opals) %in% c("AusDiab", "HOORN", "KOGES_CAVAS", "KOGES_ASAS", "ARIC", "Zutphen") )
  temp_opals = temp_opals[-studies_model4]
}

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for TOTAL SELF
  studies_model4 = which( names(temp_opals) %in% c("HOORN", "Zutphen") )
  temp_opals = temp_opals[-studies_model4]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for PBCL SELF
  studies_model4 = which( names(temp_opals) %in% c("Zutphen") )
  temp_opals = temp_opals[-studies_model4]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_4_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[model_4_alltuned$cov==my_exposure,], file = paste0('~/plots/legumes/model_4_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |  ___|
# | .  . | ___   __| | ___| | |___ \ 
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 

###### Model 4 with MEAT - sensitivity analysis

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
#my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                  "COV_FISH", "COV_MEAT")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ'){
  # exclusions for PBCL
  studies_model5 = which( names(temp_opals) %in% c("KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'NUTS_SEEDS' & my_outcome == 'CASE_OBJ'){
  # exclusions for NUTS_SEEDS
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "Zutphen","KOGES_ASAS", "CoLaus") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'SOY' & my_outcome == 'CASE_OBJ'){
  # exclusions for SOY
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "CoLaus","KOGES_ASAS",  "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'CONSUMER' & my_outcome == 'CASE_OBJ'){
  # exclusions for CONSUMER
  studies_model5 = which( names(temp_opals) %in% c("AusDiab", "HOORN", "KOGES_CAVAS", "KOGES_ASAS", "ARIC", "Zutphen",
                                                   "CoLaus") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for TOTAL SELF
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for PBCL SELF
  studies_model5 = which( names(temp_opals) %in% c("Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_5_filter_cases_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_5_filter_cases_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |  ___|
# | .  . | ___   __| | ___| | |___ \    __ _ 
# | |\/| |/ _ \ / _` |/ _ \ |     \ \  / _` |
# | |  | | (_) | (_| |  __/ | /\__/ / | (_| |
# \_|  |_/\___/ \__,_|\___|_| \____/   \__,_|


# Exposure: total legumes (g/d) at baseline*sex
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, 
#             fibre intake, meat intake, fruit intake, vegetables intake, sugary drinks intake.


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                  "COV_FISH", "COV_MEAT")

my_interaction = "SEX"

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with men or women only as will not run with SEX

studies_model4 = which( names(temp_opals) %in% c("InterAct_france", "NOWAC", "WHI", "PRHHP"))
temp_opals = temp_opals[-studies_model4]

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}



# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_5a_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedInterActionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath,interaction_term = my_interaction, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_5a_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |  ___| ___
# | .  . | ___   __| | ___| | |___ \  | |__  
# | |\/| |/ _ \ / _` |/ _ \ |     \ \ | '_ \  
# | |  | | (_) | (_| |  __/ | /\__/ / | |_) |
# \_|  |_/\___/ \__,_|\___|_| \____/  |_.__/



# Exposure: total legumes (g/d) at baseline*bmi_cat
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, 
#             fibre intake, meat intake, fruit intake, vegetables intake, sugary drinks intake.


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                 "BMI_CAT", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI_CAT", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                  "COV_FISH", "COV_MEAT")

my_interaction = "BMI_CAT"

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_5b_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedInterActionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath,interaction_term = my_interaction, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_5b_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


# ___  ___          _      _    ____ 
# |  \/  |         | |    | |  / ___|
# | .  . | ___   __| | ___| | / /___ 
# | |\/| |/ _ \ / _` |/ _ \ | | ___ \
# | |  | | (_) | (_| |  __/ | | \_/ |
# \_|  |_/\___/ \__,_|\___|_| \_____/



# Exposure: total legumes (g/d) at baseline - sensitivity analysis for family history of diabetes
# Outcome: CASE_OBJ
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT", "FAM_DIAB")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                  "COV_FISH", "COV_MEAT", "FAM_DIAB")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model6 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model6]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_6_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_6_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

# Now WITHOUT family history for comparison (for model 7 too)


#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                  "COV_FISH", "COV_MEAT")

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model6 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model6]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_6_7_baseline_no_fib_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_6_7_baseline_no_fib_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

#_______  _______  ______   _______  _           ______
#(       )(  ___  )(  __  \ (  ____ \( \        / ___  \ 
#| () () || (   ) || (  \  )| (    \/| (        \/   )  )
#| || || || |   | || |   ) || (__    | |            /  / 
#| |(_)| || |   | || |   | ||  __)   | |           /  /  
#| |   | || |   | || |   ) || (      | |          /  /   
#| )   ( || (___) || (__/  )| (____/\| (____/\   /  /    
#|/     \|(_______)(______/ (_______/(_______/   \_/     




# Exposure: total legumes (g/d) at baseline - sensitivity analysis for waist
# Outcome: CASE_OBJ
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT", "WAIST")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                  "COV_FISH", "COV_MEAT", "WAIST")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model6 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model6]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_7_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_7_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


# __  __               _          _      ___  
#|  \/  |             | |        | |    / _ \ 
#| \  / |   ___     __| |   ___  | |   | (_) |
#| |\/| |  / _ \   / _` |  / _ \ | |    > _ < 
#| |  | | | (_) | | (_| | |  __/ | |   | (_) |
#|_|  |_|  \___/   \__,_|  \___| |_|    \___/ 
                         

###### Model 5 without fish and dairy - sensitivity analysis

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
#my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_MEAT")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ'){
  # exclusions for PBCL
  studies_model5 = which( names(temp_opals) %in% c("KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'NUTS_SEEDS' & my_outcome == 'CASE_OBJ'){
  # exclusions for NUTS_SEEDS
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "Zutphen","KOGES_ASAS", "CoLaus") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'SOY' & my_outcome == 'CASE_OBJ'){
  # exclusions for SOY
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "CoLaus","KOGES_ASAS",  "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'CONSUMER' & my_outcome == 'CASE_OBJ'){
  # exclusions for CONSUMER
  studies_model5 = which( names(temp_opals) %in% c("AusDiab", "HOORN", "KOGES_CAVAS", "KOGES_ASAS", "ARIC", "Zutphen",
                                                   "CoLaus") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for TOTAL SELF
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for PBCL SELF
  studies_model5 = which( names(temp_opals) %in% c("Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_8_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_8_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



# __  __               _          _      ___  
#|  \/  |             | |        | |    / _ \ 
#| \  / |   ___     __| |   ___  | |   | (_) |
#| |\/| |  / _ \   / _` |  / _ \ | |    \__, |
#| |  | | | (_) | | (_| | |  __/ | |      / / 
#|_|  |_|  \___/   \__,_|  \___| |_|     /_/  

###### Model 5 with fibre - sensitivity analysis

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
#my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS", "COV_DAIRY",
#                  "COV_FISH", "COV_MEAT")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                  "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                  "COV_FISH", "COV_MEAT", "COV_FIBER")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ'){
  # exclusions for TOTAL OBJ
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ'){
  # exclusions for PBCL
  studies_model5 = which( names(temp_opals) %in% c("KOGES_ASAS", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'NUTS_SEEDS' & my_outcome == 'CASE_OBJ'){
  # exclusions for NUTS_SEEDS
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "Zutphen","KOGES_ASAS", "CoLaus") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'SOY' & my_outcome == 'CASE_OBJ'){
  # exclusions for SOY
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "CoLaus","KOGES_ASAS",  "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'CONSUMER' & my_outcome == 'CASE_OBJ'){
  # exclusions for CONSUMER
  studies_model5 = which( names(temp_opals) %in% c("AusDiab", "HOORN", "KOGES_CAVAS", "KOGES_ASAS", "ARIC", "Zutphen",
                                                   "CoLaus") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'TOTAL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for TOTAL SELF
  studies_model5 = which( names(temp_opals) %in% c("HOORN", "CoLaus", "Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}

if (my_exposure == 'PBCL' & my_outcome == 'CASE_OBJ_SELF'){
  # exclusions for PBCL SELF
  studies_model5 = which( names(temp_opals) %in% c("Zutphen") )
  temp_opals = temp_opals[-studies_model5]
}


# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_9_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_9_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)



# _____           _                                  _                     _                  
#|_   _|         | |                                | |                   | |                 
#  | |    _ __   | |_    ___   _ __    __ _    ___  | |_      ___  __  __ | |_   _ __    __ _ 
#  | |   | '_ \  | __|  / _ \ | '__|  / _` |  / __| | __|    / _ \ \ \/ / | __| | '__|  / _` |
# _| |_  | | | | | |_  |  __/ | |    | (_| | | (__  | |_    |  __/  >  <  | |_  | |    | (_| |
#|_____| |_| |_|  \__|  \___| |_|     \__,_|  \___|  \__|    \___| /_/\_\  \__| |_|     \__,_|

myvars = c(
  'AGE_BASE', 'TYPE_DIAB', 'PREV_DIAB', 'CASE_OBJ_SELF', 'CASE_OBJ', 'FUP_OBJ', 'FUP_OBJ_SELF', 'PBCL',
  'SOY', 'NUTS_SEEDS', 'ISOFLAVONES', 'TOTAL', 'SEX', 'BMI', 'EDUCATION', 'SMOKING', 'PA', 'ALCOHOL',
  'FAM_DIAB', 'COMORBIDITY', 'E_INTAKE', 'COV_FRUIT', 'COV_VEG', 'COV_FIBER', 'COV_MEAT', 'COV_SUG_BEVS', 'WAIST',
  'REGION_CH', 'BMI_CAT', 'CONSUMER', 'COV_DAIRY', 'COV_FISH', 'COV_RICE', 'COV_POTATO', 'COV_CEREALFIBRE'
)



# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('TOTAL')
#my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')
#my_exposure = c('SOY')
#my_exposure = c('CONSUMER')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
                 "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
                 "COV_FISH", "COV_MEAT", 'COV_RICE', 'COV_POTATO', 'COV_CEREALFIBRE')

# my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL", "REGION_CH",
#                   "BMI", "COMORBIDITY", "COV_FRUIT", "COV_VEG", "COV_SUG_BEVS", "COV_DAIRY",
#                   "COV_FISH", "COV_MEAT")


my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
if (my_outcome == 'CASE_OBJ'){
  ref_table =  'A_OBJ'
}
if (my_outcome == 'CASE_OBJ_SELF'){
  ref_table =  'A_OBJ_SELF'
}

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# tuned survival version

mypath = file.path('~', 'plots/legumes', paste0('model_IA_Extra_',my_exposure,'_',ref_table,'.svg'))
model_4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_4_alltuned = model_4[[1]]
model_4_remtuned = model_4[[2]]
write.csv(x = model_4_alltuned[grep(my_exposure, model_4_alltuned$cov),], file = paste0('~/plots/legumes/model_IA_Extra_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

### Temporary code to investigate cutting off cases in the first 2 years of follow up.

ds.cbind(x=c('burtonWeights','E10'), newobj='F1')
ds.asNumeric("F1$CASE_OBJ", newobj = "caseNumbers", opals)
ds.assign(toAssign="(1-caseNumbers)*1000+caseNumbers", newobj = "FUP_ADJ", opals)
ds.cbind(x=c("FUP_ADJ", "F1"), newobj = "F2", opals)

# remove participants with too little and excessive consumption of calories
ds.subset(x = 'F2', subset = 'F1', logicalOperator = 'FUP_ADJ>', threshold = 2, datasources = opals)

