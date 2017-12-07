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
source("fish_exemplar/helperFunctions.R")
source("fish_exemplar/survival_analysis_dsFunctions_new.R")
# Retrieve Credential Details
source("creds/fish_exemplar_creds.R")

# complete cases per study only on variables not known to be all missing
setwd("/home/l_trpb2/git/exemplar_analyses/fish_exemplar")
filter_csv = read.csv(file = 'fish_opal_vars.csv',  header=TRUE, row.names = 1 )
setwd("~")

# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals)

#all the variables in the analysis
myvars = c(
  'AGE_BASE', 'TYPE_DIAB', 'PREV_DIAB', 'CASE_OBJ_SELF', 'CASE_OBJ', 'FUP_OBJ', 'FUP_OBJ_SELF', 'FATTY',
  'FRESH', 'FRIED', 'LEAN', 'NONFISH', 'SALT', 'SSD', 'TOTAL', 'SEX', 'BMI', 'EDUCATION', 'SMOKING', 'PA',
  'ALCOHOL', 'FAM_DIAB', 'E_INTAKE', 'FRUIT', 'VEG', 'FIBER', 'SUG_BEVS', 'WAIST', 'SUPPLEMENTS', 'COMORBID',
  'MEAT'
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
none_cc_vars = c("WAIST","SUPPLEMENTS", "FAM_DIAB")

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
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['HOORN'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['NHAPC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['NOWAC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['SMC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['ELSA'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['Whitehall'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['Zutphen'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['AusDiab'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['JPHC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['WHI'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['CARDIA'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['ARIC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['PRHHP'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['MESA'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['FMC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['Golestan'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['CKB'])
ds.cbind(x=c('burtonWeights','E10'), newobj='F1')

###############################################################################
########################### GENERATE TIME BUCKETS  ############################
###############################################################################

# Since we have fixed the studies and their variables, it is only necessary to generate
# the time buckets once

# For OBJ follow up time

my_outcome = c('CASE_OBJ')
my_exit_col = c('newEndDate')
ref_table = 'F1'
bucket_table =  'A_OBJ'

for (study in c(1:length(opals))) {
  tunedLexisB(ref_table_in = ref_table, my_outcome_in =  my_outcome, my_exit_col_in = my_exit_col, study = opals[study], new_table = bucket_table)
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
ref_table = 'F1'
bucket_table =  'A_OBJ_SELF'

for (study in c(1:length(opals))) {
  tunedLexisB(ref_table_in = ref_table, my_outcome_in =  my_outcome, my_exit_col_in = my_exit_col, study = opals[study], new_table = bucket_table)
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

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_',my_exposure,'_',ref_table,'.csv'))
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

mypath = file.path('~', 'plots/test', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

####################################
#Regional split 

regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                               "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB"))
regions[['oceania']] = data.frame("study" = c("AusDiab"))
regions[['middle']] = data.frame("study" = c("Golestan"))

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('~', 'plots/test', paste0('model_1_', names(regions[z]), '_', my_exposure,'_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=4.5 * length(my_exposure), 
      height=3.5 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  do_REM(coeffs = temp_data$Estimate, s_err = temp_data$`Std. Error`, labels = temp_data$study,fmla = "see main plot", out_family = 'poisson', variable = my_exposure, ref_table = ref_table)
  dev.off()
  
}



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

#  ______    _   _         
# |  ____|  | | | |        
# | |__ __ _| |_| |_ _   _ 
# |  __/ _` | __| __| | | |
# | | | (_| | |_| |_| |_| |
# |_|  \__,_|\__|\__|\__, |
#                     __/ |
#                    |___/ 


# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FATTY')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)


####################################################
## Stratified analyses by quartile

quartile_studies = study_names[! study_names %in% c("Zutphen", "Whitehall", "InterAct_france", "CARDIA", "InterAct_netherlands", "HOORN", "AusDiab", "ELSA", "NHAPC")]
opals_quartiles = opals[quartile_studies]
rev(opals_quartiles)

rm(x)
temp_quants = ds.quantileMean(x=paste0("D8$", my_exposure), type = 'split', datasources = opals_quartiles)
quant_df = t(as.data.frame(temp_quants))
for (x in c(1:length(row.names(quant_df)))) {
  print(x)
  ds.subset(x='D8',subset = 'D8_l50', logicalOperator = paste0(my_exposure, '<='), threshold = quant_df[x,'50%'], datasources = opals_quartiles[x])
  ds.subset(x='D8',subset = 'D8_h50', logicalOperator = paste0(my_exposure, '>'), threshold = quant_df[x,'50%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_l50',subset = 'D8_l25', logicalOperator = paste0(my_exposure, '<='), threshold = quant_df[x,'25%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_l50',subset = 'D8_h25', logicalOperator = paste0(my_exposure, '>'), threshold = quant_df[x,'25%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_h50',subset = 'D8_l75', logicalOperator = paste0(my_exposure, '<='), threshold = quant_df[x,'75%'], datasources = opals_quartiles[x])
  ds.subset(x='D8_h50',subset = 'D8_h75', logicalOperator = paste0(my_exposure, '>'), threshold = quant_df[x,'75%'], datasources = opals_quartiles[x])
}
rm(x)

for (x in c(1:length(row.names(quant_df)))) {
  print(paste0(my_exposure, '<='))
  print(quant_df[x,'50%'])
  
}
rm(x)

ref_table = 'D8_l25'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q1_fatty.svg')
model_1_Q1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q1_alltuned = model_1_Q1[[1]]
model_1_Q1_remtuned = model_1_Q1[[2]]
write.csv(x = model_1_Q1_alltuned[model_1_Q1_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q1_survivaltuned_fatty.csv')

ref_table = 'D8_h25'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q2_fatty.svg')
model_1_Q2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q2_alltuned = model_1_Q2[[1]]
model_1_Q2_remtuned = model_1_Q2[[2]]
write.csv(x = model_1_Q2_alltuned[model_1_Q2_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q2_survivaltuned_fatty.csv')

ref_table = 'D8_l75'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q3_fatty.svg')
model_1_Q3 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q3_alltuned = model_1_Q3[[1]]
model_1_Q3_remtuned = model_1_Q3[[2]]
write.csv(x = model_1_Q3_alltuned[model_1_Q3_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q3_survivaltuned_fatty.csv')

ref_table = 'D8_h75'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_Q4_fatty.svg')
model_1_Q4 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_quartiles )
model_1_Q4_alltuned = model_1_Q4[[1]]
model_1_Q4_remtuned = model_1_Q4[[2]]
write.csv(x = model_1_Q4_alltuned[model_1_Q4_alltuned$cov==my_exposure,], file = '~/plots/model_1_Q4_survivaltuned_fatty.csv')


#  ______             _     
# |  ____|           | |    
# | |__ _ __ ___  ___| |__  
# |  __| '__/ _ \/ __| '_ \ 
# | |  | | |  __/\__ \ | | |
# |_|  |_|  \___||___/_| |_|

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FRESH')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

#  ______    _          _ 
# |  ____|  (_)        | |
# | |__ _ __ _  ___  __| |
# |  __| '__| |/ _ \/ _` |
# | |  | |  | |  __/ (_| |
# |_|  |_|  |_|\___|\__,_|

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FRIED')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

#  _                      
# | |                     
# | |     ___  __ _ _ __  
# | |    / _ \/ _` | '_ \ 
# | |___|  __/ (_| | | | |
# |______\___|\__,_|_| |_|
# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FRIED')

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'


#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)
#  _   _             ______ _     _     
# | \ | |           |  ____(_)   | |    
# |  \| | ___  _ __ | |__   _ ___| |__  
# | . ` |/ _ \| '_ \|  __| | / __| '_ \ 
# | |\  | (_) | | | | |    | \__ \ | | |
# |_| \_|\___/|_| |_|_|    |_|___/_| |_|
# 
nonfish_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "HOORN", "SMC", "Whitehall", "InterAct_germany")]
opals_nonfish = opals[nonfish_studies]

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('NONFISH')
#my_outcome = c('CASE_OBJ')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
#my_exit_col = c('newEndDate')
my_exit_col = c('newEndDate_SELF')

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_nonfish)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_nonfish)
length_complete_split_nonfish = ds.length("D8$SEX", type = "split", datasources = opals_nonfish)

# # Simple Regression Model For Testing Quickly
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_1_nonfishnormal_regression.svg')
# model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_nonfish )
# model_1reg_all = model_1reg_results[[1]]
# model_1reg_REM = model_1reg_results[[2]]
# 
# # survival version with lexis b
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_1_nonfishsurvival.svg')
# model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_nonfish)
# model_1_all = model_1[[1]]
# model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_nonfishsurvivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_nonfish)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_nonfishsurvivaltuned.csv')


#   _____       _ _
#  / ____|     | | |
# | (___   __ _| | |_
#  \___ \ / _` | | __|
#  ____) | (_| | | |_
# |_____/ \__,_|_|\__|

salt_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "HOORN", "JPHC", "Zutphen", "NOWAC", "SMC", "Whitehall", "WHI", "InterAct_spain",
                                                "InterAct_france", "InterAct_france", "InterAct_uk","InterAct_netherlands",
                                                "InterAct_germany", "InterAct_sweden", "InterAct_denmark", "InterAct_italy", "WHI", "CARDIA", "ARIC", "MESA", "PRHHP")]
opals_salt = opals[salt_studies]

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('SALT')
#my_outcome = c('CASE_OBJ')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
#my_exit_col = c('newEndDate')
my_exit_col = c('newEndDate_SELF')

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_salt)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_salt)
length_complete_split_salt = ds.length("D8$SEX", type = "split", datasources = opals_salt)

# # Simple Regression Model For Testing Quickly
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_1_saltnormal_regression.svg')
# model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_salt )
# model_1reg_all = model_1reg_results[[1]]
# model_1reg_REM = model_1reg_results[[2]]
# 
# # survival version with lexis b
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_1_saltsurvival.svg')
# model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_salt)
# model_1_all = model_1[[1]]
# model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_saltsurvivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_salt)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_saltsurvivaltuned.csv')


#   _____ _____ _____
#  / ____/ ____|  __ \
# | (___| (___ | |  | |
#  \___ \\___ \| |  | |
#  ____) |___) | |__| |
# |_____/_____/|_____/

ssd_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "HOORN", "NOWAC", "SMC", "Whitehall", "WHI", "InterAct_spain",
                                               "InterAct_france", "InterAct_france", "InterAct_uk","InterAct_netherlands",
                                               "InterAct_germany", "InterAct_sweden", "InterAct_denmark", "InterAct_italy", "WHI", "CARDIA", "ARIC", "MESA")]
opals_ssd = opals[ssd_studies]

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('SSD')
#my_outcome = c('CASE_OBJ')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
#my_exit_col = c('newEndDate')
my_exit_col = c('newEndDate_SELF')

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_ssd)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_ssd)
length_complete_split_ssd = ds.length("D8$SEX", type = "split", datasources = opals_ssd)

# # Simple Regression Model For Testing Quickly
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_1_ssdnormal_regression.svg')
# model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_ssd )
# model_1reg_all = model_1reg_results[[1]]
# model_1reg_REM = model_1reg_results[[2]]
# 
# # survival version with lexis b
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_1_ssdsurvival.svg')
# model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_ssd)
# model_1_all = model_1[[1]]
# model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_ssdsurvivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_ssd)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_ssdsurvivaltuned.csv')

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

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
my_vars_check = c(my_exposure, my_outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

# exclude studies with insufficient cases to run

studies_model2 = study_names[! study_names %in% c( "HOORN", "Zutphen","CARDIA","Whitehall","NHAPC", "AusDiab")]

temp_opals = temp_opals[studies_model2]

# tuned survival version

mypath = file.path('~', 'plots/test', paste0('model_2_',my_exposure,'_',ref_table,'.svg'))
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = temp_opals)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = paste0('~/plots/test/model_2_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)

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



#  ______    _   _         
# |  ____|  | | | |        
# | |__ __ _| |_| |_ _   _ 
# |  __/ _` | __| __| | | |
# | | | (_| | |_| |_| |_| |
# |_|  \__,_|\__|\__|\__, |
#                     __/ |
#                    |___/ 


fatty_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "Zutphen", "NHAPC", "ELSA", "CARDIA", "PRHHP")]
opals_fatty = opals[fatty_studies]

# Change order to check troublesome studies first
#opals_fatty <- opals_fatty[c("JPHC","WHI", "InterAct_france", "InterAct_denmark",  "InterAct_italy", "InterAct_netherlands",  "InterAct_spain", "InterAct_sweden", "InterAct_uk", "InterAct_germany", "NOWAC")]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')
#my_exit_col = c('newEndDate_SELF')

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fatty)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fatty)
length_complete_split_fatty = ds.length("D8$SEX", type = "split", datasources = opals_fatty)

# # Simple Regression Model For Testing Quickly 
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_fatty_normal_regression.svg')
# model_2reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fatty )
# model_2reg_all = model_2reg_results[[1]]
# model_2reg_REM = model_2reg_results[[2]]
# 
# # survival version with lexis b
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_fattysurvival.svg')
# model_2 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_fatty)
# model_2_all = model_2[[1]]
# model_2_rem = model_2[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_fattysurvivaltuned_obj.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fatty)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_fattysurvivaltuned_obj.csv')




regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                               "InterAct_denmark","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "AusDiab"))

for_RMA = model_2_alltuned[model_2_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('~', 'plots', paste0('model_2_fattysurvivaltuned_', names(regions[z]), '_SELF.svg'))
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

#  ______    _          _ 
# |  ____|  (_)        | |
# | |__ _ __ _  ___  __| |
# |  __| '__| |/ _ \/ _` |
# | |  | |  | |  __/ (_| |
# |_|  |_|  |_|\___|\__,_|

fried_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "Zutphen", "NHAPC", "JPHC", "NOWAC", "InterAct_france", "ARIC", "PRHHP","Whitehall", "CARDIA")]
opals_fried = opals[fried_studies]

# Change order to check troublesome studies first
#opals_fried <- opals_fried[c("AusDiab", "ELSA", "WHI", "InterAct_denmark",  "InterAct_italy", "InterAct_netherlands",  "InterAct_spain", "InterAct_sweden", "InterAct_uk")]

my_exposure = c('FRIED')
my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')
#my_exit_col = c('newEndDate_SELF')

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fried)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fried)
length_complete_split_fried = ds.length("D8$SEX", type = "split", datasources = opals_fried)

# # Simple Regression Model For Testing Quickly 
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_friednormal_regression.svg')
# model_2reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fried )
# model_2reg_all = model_2reg_results[[1]]
# model_2reg_REM = model_2reg_results[[2]]
# 
# # survival version with lexis b
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_friedsurvival.svg')
# model_2 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_fried)
# model_2_all = model_2[[1]]
# model_2_rem = model_2[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_friedsurvivaltuned_obj.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fried)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_friedsurvivaltuned_obj.csv')

regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                               "InterAct_denmark","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "AusDiab"))

for_RMA = model_2_alltuned[model_2_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('~', 'plots', paste0('model_2_friedsurvivaltuned_', names(regions[z]), '_SELF.svg'))
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

#  _                      
# | |                     
# | |     ___  __ _ _ __  
# | |    / _ \/ _` | '_ \ 
# | |___|  __/ (_| | | | |
# |______\___|\__,_|_| |_|


lean_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "NHAPC", "Zutphen", "ELSA", "InterAct_germany","CARDIA","Whitehall", "PRHHP", "ARIC")]
opals_lean = opals[lean_studies]

# Change order to check troublesome studies first
#opals_lean <- opals_lean[c("JPHC","WHI", "InterAct_france", "InterAct_denmark",  "InterAct_italy", "InterAct_netherlands",  "InterAct_spain", "InterAct_sweden", "InterAct_uk", "NOWAC")]

my_exposure = c('LEAN')
my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')
#my_exit_col = c('newEndDate_SELF')

# To limit the loss of participants we will only look variables we are investigating (from Silvia)

my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_lean)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_lean)
length_complete_split_lean = ds.length("D8$SEX", type = "split", datasources = opals_lean)

# # Simple Regression Model For Testing Quickly 
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_leannormal_regression.svg')
# model_2reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_lean )
# model_2reg_all = model_2reg_results[[1]]
# model_2reg_REM = model_2reg_results[[2]]
# 
# # survival version with lexis b
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_leansurvival.svg')
# model_2 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_lean)
# model_2_all = model_2[[1]]
# model_2_rem = model_2[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_leansurvivaltuned_obj.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_lean)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_leansurvivaltuned_obj.csv')

regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                               "InterAct_denmark","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "AusDiab"))

for_RMA = model_2_alltuned[model_2_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('~', 'plots', paste0('model_2_leansurvivaltuned_', names(regions[z]), '_SELF.svg'))
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
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "newEndDate", "newStartDate", "burtonWeights","FAM_DIAB")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D9', completeCases = TRUE, datasources = opals)
studies_model3 = study_names[! study_names %in% c("NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA")]
opals_model3 = opals[studies_model3]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", 
                  "FAM_DIAB")
ref_table = 'D9'
mypath = file.path('~', 'plots', 'model_3a_survival.svg')
model_3a = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_model3)
model_3a_all = model_3a[[1]]
model_3a_rem = model_3a[[2]]

####################################################
# Model 3b: As model 2 + adj for waist circumference
####################################################
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "newEndDate", "newStartDate", "burtonWeights","WAIST")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D10', completeCases = TRUE, datasources = opals)

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", 
                  "WAIST")

ref_table = 'D10'
mypath = file.path('~', 'plots', 'model_3b_survival.svg')
model_3b = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_model3)
model_3b_all = model_3b[[1]]
model_3b_rem = model_3b[[2]]

####################################################
# Model 3c: As model 2 + adj for fish oil supplements
####################################################
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS","newEndDate", "newStartDate", "burtonWeights",  "SUPPLEMENTS")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D11', completeCases = TRUE, datasources = opals)

studies_model3c = study_names[! study_names %in% c("NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA", "AusDiab")]
opals_model3c = opals[studies_model3c]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", 
                  "SUPPLEMENTS")

ref_table = 'D11'
mypath = file.path('~', 'plots', 'model_3c_survival.svg')
model_3c = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_model3c)
model_3c_all = model_3c[[1]]
model_3c_rem = model_3c[[2]]


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

my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "newEndDate", "newStartDate", "burtonWeights","SUG_BEVS")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D12', completeCases = TRUE, datasources = opals)

studies_no_singleGender = study_names[! study_names %in% c("InterAct_france", "NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA")]
opals_no_SG = opals[studies_no_singleGender]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SEX")
my_interaction = "SEX"

ref_table = 'D12'
mypath = file.path('~', 'plots', 'model_4_surv.svg')
model_4 = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, opals_no_SG)
model_4_all = model_4[[1]]
model_4_rem = model_4[[2]]

####################################################
## Stratified analyses by sex (men, women) if significant
# Men
ds.subset(x = 'D12', subset = 'D12_men', logicalOperator = 'SEX==', threshold = 0, datasources = opals)
men <- ds.length('D12_men$SEX', type = 'split', datasources = opals)
ref_table = 'D12_men'
mypath = file.path('~', 'plots', 'model_4_men_surv.svg')
model_4men = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_no_SG)
model_4men_all = model_4men[[1]]
model_4men_rem = model_4men[[2]]

####################################################
# Women
ds.subset(x = 'D12', subset = 'D12_women', logicalOperator = 'SEX==', threshold = 1, datasources = opals)
women <- ds.length('D12_women$SEX', type = 'split', datasources = opals)
ref_table = 'D12_women'
mypath = file.path('~', 'plots', 'model_4_women_surv.svg')
model_4women = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_no_SG)
model_4women_all = model_4women[[1]]
model_4women_rem = model_4women[[2]]

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

# Studies involved in model 5
studies_model5 = study_names[! study_names %in% c("NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA")]
opals_model5 = opals[studies_model5]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_interaction = "BMI"

ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_5_surv.svg')
model_5 = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_model5)
model_5_all = model_5[[1]]
model_5_rem = model_5[[2]]


my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

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


