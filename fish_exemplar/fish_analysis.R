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
setwd("/home/l_pms69/exemplar_analyses/")
#setwd("/home/l_trpb2/git/exemplar_analyses/")

# Source in the Extra functions for analysis
source("fish_exemplar/helperFunctions.R")
source("fish_exemplar/survival_analysis_dsFunctions.R")
# Retrieve Credential Details
source("creds/fish_exemplar_creds.R")
setwd("~")

# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals)
myvars = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
           "FUP_OBJ", "FUP_OBJ_SELF", "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           "FAM_DIAB", "E_INTAKE", "FRUIT", "VEG",  "FIBER", "SUG_BEVS", "WAIST", "SUPPLEMENTS", 
           "AGE_END_OBJ_SELF", "AGE_END_OBJ", "AGE_BASE", "MEAT", "COMORBID")
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish') # for all available variables

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# all participants
all_participants <- ds.length('D$TOTAL')
all_participants_split <- ds.length('D$TOTAL',type = 'split')

# Set studynames and numstudies
temp <- ds.summary('D$TOTAL')
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

# remove participants with prevalent diabetes and type 1
ds.subset(x = 'D', subset = 'E1', logicalOperator = 'PREV_DIAB==', threshold = 0)
noPrevalence <- ds.length('E1$SEX', type = 'split')
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB==', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# In order to deal with the intake subsets stratified by sex we will have to create subsets of sex,
# do the intake subset and then rbind the groups back together. What follows is DataSHIELD magic
ds.asNumeric("E2$SEX", newobj = "sexNumbers")
ds.assign(toAssign="(sexNumbers*300)+E2$E_INTAKE", newobj = "adjustedLowerBound")
ds.assign(toAssign="(sexNumbers*700)+E2$E_INTAKE", newobj = "adjustedUpperBound")
ds.cbind(x=c("adjustedLowerBound", "E2"), newobj = "L1")
ds.cbind(x=c("adjustedUpperBound", "L1"), newobj = "L2")
# remove participants with too little and excessive consumption of calories
ds.subset(x = 'L2', subset = 'E3', logicalOperator = 'adjustedUpperBound<=', threshold = 4200)
under3500cal <- ds.length('E3$SEX', type = 'split')
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'adjustedLowerBound>=', threshold = 800)
afterIntake <- ds.length('E4$SEX', type = 'split')

# Setup an additional proxy ID column for each study 
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('ID','E4'), newobj='D2')

# Zeros, new start date and end date
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
rm(i)
ds.cbind(x=c('newStartDate','D2'), newobj='D3')
ds.assign(toAssign = 'D3$FUP_OBJ', newobj = 'newEndDate')
ds.cbind(x=c('newEndDate','D3'), newobj='D5')

# Adding in the weights as described by Dr. Burton
ds.asNumeric('D5$CASE_OBJ', newobj = "caseNums")
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
ds.cbind(x=c('burtonWeights','D5'), newobj='D6')

# put in any dummy columns for the studies with completely missing columns
# italy missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['InterAct_italy'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['InterAct_italy'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['InterAct_italy'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['InterAct_italy'])
# spain missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['InterAct_spain'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['InterAct_spain'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['InterAct_spain'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['InterAct_spain'])
# nowac missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['NOWAC'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['NOWAC'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['NOWAC'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['NOWAC'])

# Loop to produce E4 and model_all_len for descriptive stats
# Note that this doesnt actually handle well if a study has lost all its participants before this section
my_vars_all = c("AGE_BASE", "CASE_OBJ", "TOTAL",
                "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL", "COMORBID", "E_INTAKE", "FRUIT",
                "VEG", "FIBER", "MEAT", "SUG_BEVS", "FAM_DIAB", "WAIST", "SUPPLEMENTS","newEndDate", "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all)
ds.subset(x = 'D7', subset = 'D4', completeCases = TRUE)
length_complete = ds.length('D4$SEX')
length_complete_split = ds.length("D4$SEX", type = "split")

## Dataframe to hold length figures
# model_all_len <- data.frame()
# model_all_len <- rbind(model_all_len, all_participants_split, noPrevalence, noType1, under3500cal, afterIntake)
# for (i in 2:length(my_vars_all)){
#   print(my_vars_all[1:i])
#   ds.subset(x = 'D6', subset = 'E6', cols =  my_vars_all[1:i])
#   ds.subset(x = 'E6', subset = 'E7', completeCases = TRUE)
#   thingToBind = vector("numeric")
#   print(i)
#   for (k in 1:num_studies){
#     lengthNum = ds.length('E7$ID', datasources = opals[k])
#     thingToBind = c(thingToBind, lengthNum)
#     print(thingToBind)
#   }
#   thingToBind = unlist(unname(thingToBind))
#   print("this is thingtobind unlistedunnamed")
#   print(k)
#   print(thingToBind)
#   model_all_len = rbind(model_all_len, thingToBind)
# }
# rownames = c("ALL", "PREV_DIAB", "TYPE_DIAB", "under3500cal", "afterIntake", my_vars_all[2:length(my_vars_all)])
# row.names(model_all_len) <- rownames



# ___  ___          _      _   __  
# |  \/  |         | |    | | /  | 
# | .  . | ___   __| | ___| | `| | 
# | |\/| |/ _ \ / _` |/ _ \ |  | | 
# | |  | | (_) | (_| |  __/ | _| |_
# \_|  |_/\___/ \__,_|\___|_| \___/

# To limit the loss of participants we will only look variables we are investigating (from Silvia)
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "newEndDate", "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE)

                                 
# Exposure: total fish (g/d) at baseline
# Outcome: CASE_OBJ
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities
# To assess the impact of each confounder we will also run models including each confounder separately.
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")


# Simple Regression Model For Testing Quickly 
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_normal_regression.svg')
model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath)
model_1reg_all = model_1reg_results[[1]]
model_1reg_REM = model_1reg_results[[2]]

# survival version with lexis b
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survival.svg')
model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2))
model_1_all = model_1[[1]]
model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]

# # incremental model 1
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_incremental')
model_1_inc = runIncrementalSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2))

#  ______    _   _         
# |  ____|  | | | |        
# | |__ __ _| |_| |_ _   _ 
# |  __/ _` | __| __| | | |
# | | | (_| | |_| |_| |_| |
# |_|  \__,_|\__|\__|\__, |
#                     __/ |
#                    |___/ 

fatty_studies = study_names[! study_names %in% c("Ausdiab", "ELSA", "NHAPC", "SMC", "Whitehall")]
opals_fatty = opals[fatty_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

# Simple Regression Model For Testing Quickly 
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_normal_regression.svg')
model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fatty )
model_1reg_all = model_1reg_results[[1]]
model_1reg_REM = model_1reg_results[[2]]

# survival version with lexis b
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survival.svg')
model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_fatty)
model_1_all = model_1[[1]]
model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fatty)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]

#  ______             _     
# |  ____|           | |    
# | |__ _ __ ___  ___| |__  
# |  __| '__/ _ \/ __| '_ \ 
# | |  | | |  __/\__ \ | | |
# |_|  |_|  \___||___/_| |_|

fresh_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "NHAPC", "HOORN", "NOWAC", "SMC", "Whitehall", "WHI", "InterAct_spain",
                                                 "InterAct_france", "InterAct_france", "InterAct_uk","InterAct_netherlands", 
                                                 "InterAct_germany", "InterAct_sweden", "InterAct_denmark")]
opals_fresh = opals[fresh_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

# Simple Regression Model For Testing Quickly 
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_normal_regression.svg')
model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fresh )
model_1reg_all = model_1reg_results[[1]]
model_1reg_REM = model_1reg_results[[2]]

# survival version with lexis b
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survival.svg')
model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_fresh)
model_1_all = model_1[[1]]
model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fresh)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]


#  ______    _          _ 
# |  ____|  (_)        | |
# | |__ _ __ _  ___  __| |
# |  __| '__| |/ _ \/ _` |
# | |  | |  | |  __/ (_| |
# |_|  |_|  |_|\___|\__,_|

fried_studies = study_names[! study_names %in% c("HOORN", "JPHC", "NOWAC", "NHAPC", "SMC", "Whitehall")]
opals_fried = opals[fried_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

# Simple Regression Model For Testing Quickly 
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_normal_regression.svg')
model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fried )
model_1reg_all = model_1reg_results[[1]]
model_1reg_REM = model_1reg_results[[2]]

# survival version with lexis b
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survival.svg')
model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_fried)
model_1_all = model_1[[1]]
model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_fried)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]


#  _                      
# | |                     
# | |     ___  __ _ _ __  
# | |    / _ \/ _` | '_ \ 
# | |___|  __/ (_| | | | |
# |______\___|\__,_|_| |_|

lean_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "HOORN","NHAPC", "SMC", "Whitehall")]
opals_lean = opals[lean_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

# Simple Regression Model For Testing Quickly 
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_normal_regression.svg')
model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_lean )
model_1reg_all = model_1reg_results[[1]]
model_1reg_REM = model_1reg_results[[2]]

# survival version with lexis b
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survival.svg')
model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2), studies = opals_lean)
model_1_all = model_1[[1]]
model_1_rem = model_1[[2]]

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survivaltuned.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_lean)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]


#  _   _             ______ _     _     
# | \ | |           |  ____(_)   | |    
# |  \| | ___  _ __ | |__   _ ___| |__  
# | . ` |/ _ \| '_ \|  __| | / __| '_ \ 
# | |\  | (_) | | | | |    | \__ \ | | |
# |_| \_|\___/|_| |_|_|    |_|___/_| |_|


#   _____       _ _   
#  / ____|     | | |  
# | (___   __ _| | |_ 
#  \___ \ / _` | | __|
#  ____) | (_| | | |_ 
# |_____/ \__,_|_|\__|

#   _____ _____ _____  
#  / ____/ ____|  __ \ 
# | (___| (___ | |  | |
#  \___ \\___ \| |  | |
#  ____) |___) | |__| |
# |_____/_____/|_____/ 


# ___  ___          _      _   _____ 
# |  \/  |         | |    | | / __  \
# | .  . | ___   __| | ___| | `' / /'
# | |\/| |/ _ \ / _` |/ _ \ |   / /  
# | |  | | (_) | (_| |  __/ | ./ /___
# \_|  |_/\___/ \__,_|\___|_| \_____/
# Model 2a: As model 1 + adj for energy intake, alcohol intake, fibre intake, meat intake, 

#     fruit intake, vegetables intake, sugary drinks intake

studies_model2 = study_names[! study_names %in% c("HOORN", "ELSA", "NHAPC", "Zutphen")]

opals_model2 = opals[studies_model2]


my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

# Survival Model
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2a_survival.svg')
model_2a = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,3,2,2,2), studies = opals_model2)
model_2a_all = model_2a[[1]]
model_2a_rem = model_2a[[2]]

# Normal Regression for Error Checking
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_normal_regression.svg')
model_2reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_model2)
model_2reg_all = model_2reg_results[[1]]
model_2reg_REM = model_2reg_results[[2]]

# # Incremental Model
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_incremental')
# model_2_inc = runIncrementalSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), studies = opals_model2)

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

# Present analyses by geographical area (Central area, Eastern area, Western area)
# subset opals list by geographic area then carry out regression for each one on their own.
opals_central = opals[c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                      "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                      "InterAct_denmark", "NOWAC", "SMC", "Whitehall")]
opals_western = opals["elsa"]
opals_eastern = opals["NHAPC", "JPHC"]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

# Assign country code to each of the studies
# Adding in the weights as described by Dr. Burton
######################################################
# central area
######################################################
length_central = ds.length('D4$SEX', datasources = opals_central)
for(i in 1:length(opals_central)){
  ds.assign(toAssign = "newStartDate + 1", newobj = "geocode", datasources = opals_central[i])
  ds.asFactor(x = "geocode", newobj = "geocode", datasources = opals_central[i])
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('geocode','D4'), newobj='D4')

ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_6_central_surv.svg')
model_6central = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_central)
model_6central_all = model_6central[[1]]
model_6central_rem = model_6central[[2]]

######################################################
# eastern area
######################################################
length_eastern = ds.length('D4$SEX', datasources = opals_eastern)
for(i in 1:length(opals_eastern)){
  ds.assign(toAssign = "newStartDate + 1", newobj = "geocode", datasources = opals_eastern[i])
  ds.asFactor(x = "geocode", newobj = "geocode", datasources = opals_eastern[i])
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('geocode','D4'), newobj='D4')


ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_6_eastern_surv.svg')
model_6eastern = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_eastern)
model_6eastern_all = model_6eastern[[1]]
model_6eastern_rem = model_6eastern[[2]]

######################################################
# western area
######################################################
length_central = ds.length('D4$SEX', datasources = opals_western)
for(i in 1:length(opals_western)){
  ds.assign(toAssign = "newStartDate + 1", newobj = "geocode", datasources = opals_western[i])
  ds.asFactor(x = "geocode", newobj = "geocode", datasources = opals_western[i])
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('geocode','D4'), newobj='D4')

ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_6_eastern_surv.svg')
model_6western = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_western)
model_6western_all = model_6western[[1]]
model_6western_rem = model_6western[[2]]

# Meta regression
# inside of a meta regression you take the regression coefficient values and regress them against another trait (like the geographical area)
# and then look at the coefficients here.
# we can just use the regression coefficients created out of the values and 
# then do local linear regression to see the relationship between the variables.
study_regression_coefficients = c(model_6central_all, model_6western_all, model_6eastern_all)
central_codes = rep(1, times = length(opals_central))
western_codes = rep(2, times = length(opals_western))
eastern_codes = rep(3, times = length(opals_eastern))
geocodes = as.factor(x = c(central_codes, western_codes, eastern_codes))

meta_fmla = "total_coeffs wrt censor ~  geocodes"
meta_regression_model = lm(formula = meta_fmla)


