## For Portugal
## Author: Paul Scherer
##		   Tom Bishop
## Date: 04/09/2017

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
rm(opals_model2)
rm(opals_fatty)
rm(opals_fresh)
rm(opals_fried)
rm(opals_lean)
rm(opals_nonfish)
rm(opals_salt)
rm(opals_ssd)
rm(opals_central)
rm(opals_eastern)
rm(opals_western)
rm(opals_quartiles)

myvars = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
           "FUP_OBJ", "FUP_OBJ_SELF", "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           "FAM_DIAB", "E_INTAKE", "FRUIT", "VEG",  "FIBER", "SUG_BEVS", "WAIST", "SUPPLEMENTS", 
           "AGE_END_OBJ_SELF", "AGE_END_OBJ", "AGE_BASE", "MEAT", "COMORBID", "BMI_CAT")
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish') # for all available variables

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# all participants
all_participants <- ds.length('D$TOTAL', datasources = opals)
all_participants_split <- ds.length('D$TOTAL',type = 'split', datasources = opals)

# Set studynames and numstudies
temp <- ds.summary('D$TOTAL', datasources = opals)
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

# remove participants with prevalent diabetes and type 1
ds.subset(x = 'D', subset = 'E1', logicalOperator = 'PREV_DIAB==', threshold = 0, datasources = opals)
noPrevalence <- ds.length('E1$SEX', type = 'split', datasources = opals)
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB==', threshold = 1, datasources = opals)
noType1 <- ds.length('E2$SEX', type = 'split', datasources = opals)

# In order to deal with the intake subsets stratified by sex we will have to create subsets of sex,
# do the intake subset and then rbind the groups back together. What follows is DataSHIELD magic
ds.asNumeric("E2$SEX", newobj = "sexNumbers", datasources = opals)
ds.assign(toAssign="(sexNumbers*300)+E2$E_INTAKE", newobj = "adjustedLowerBound", datasources = opals)
ds.assign(toAssign="(sexNumbers*700)+E2$E_INTAKE", newobj = "adjustedUpperBound", datasources = opals)
ds.cbind(x=c("adjustedLowerBound", "E2"), newobj = "L1", datasources = opals)
ds.cbind(x=c("adjustedUpperBound", "L1"), newobj = "L2", datasources = opals)
# remove participants with too little and excessive consumption of calories
ds.subset(x = 'L2', subset = 'E3', logicalOperator = 'adjustedUpperBound<=', threshold = 4200, datasources = opals)
under3500cal <- ds.length('E3$SEX', type = 'split', datasources = opals)
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'adjustedLowerBound>=', threshold = 800, datasources = opals)
afterIntake <- ds.length('E4$SEX', type = 'split', datasources = opals)

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

ds.cbind(x=c('newStartDate','D2'), newobj='D3', datasources = opals)
ds.assign(toAssign = 'D3$FUP_OBJ', newobj = 'newEndDate', datasources = opals)
ds.assign(toAssign = 'D3$FUP_OBJ_SELF', newobj = 'newEndDate_SELF', datasources = opals)
ds.cbind(x=c('newEndDate', 'newEndDate_SELF','D3'), newobj='D5', datasources = opals)

# Adding in the weights as described by Prof. Burton
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
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['CARDIA'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['ARIC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['PRHHP'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['MESA'])
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


# Model 1 and Model 2:

# TOTAL ~ OBJ
# TOTAL ~ OBJ (by geog region)
# TOTAL ~ OBJ (by quartile)
# TOTAL ~ SELF
# FATTY ~ OBJ
# FATTY ~ SELF
# LEAN ~ OBJ
# LEAN ~ SELF
# FRIED ~ OBJ
# FRIED ~ SELF


# ___  ___          _      _   __    _____     _        _ _____ _     _       
# |  \/  |         | |    | | /  |  |_   _|   | |      | |  _  | |   (_)      
# | .  . | ___   __| | ___| | `| |    | | ___ | |_ __ _| | | | | |__  _       
# | |\/| |/ _ \ / _` |/ _ \ |  | |    | |/ _ \| __/ _` | | | | | '_ \| |      
# | |  | | (_) | (_| |  __/ | _| |_   | | (_) | || (_| | \ \_/ / |_) | |      
# \_|  |_/\___/ \__,_|\___|_| \___/   \_/\___/ \__\__,_|_|\___/|_.__/| |      
#                                                                   _/ |      
#                                                                  |__/       

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate')

# Subset
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals)
length_complete_split_total = ds.length("D8$SEX", type = "split", datasources = opals)

# Analysis Tuned
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_obj.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_survivaltuned_obj.csv')


# ___  ___          _      _   __    _____     _        _ _____      _  __    
# |  \/  |         | |    | | /  |  |_   _|   | |      | /  ___|    | |/ _|   
# | .  . | ___   __| | ___| | `| |    | | ___ | |_ __ _| \ `--.  ___| | |_    
# | |\/| |/ _ \ / _` |/ _ \ |  | |    | |/ _ \| __/ _` | |`--. \/ _ \ |  _|   
# | |  | | (_) | (_| |  __/ | _| |_   | | (_) | || (_| | /\__/ /  __/ | |     
# \_|  |_/\___/ \__,_|\___|_| \___/   \_/\___/ \__\__,_|_\____/ \___|_|_|     
                                                                            
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate_SELF')

# Subset
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals)
length_complete_split_total = ds.length("D8$SEX", type = "split", datasources = opals)

# Analysis Tuned
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survivaltuned_self.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_survivaltuned_self.csv')


# ___  ___          _      _   __    _____                                    
# |  \/  |         | |    | | /  |  |  __ \                                   
# | .  . | ___   __| | ___| | `| |  | |  \/ ___  ___                          
# | |\/| |/ _ \ / _` |/ _ \ |  | |  | | __ / _ \/ _ \                         
# | |  | | (_) | (_| |  __/ | _| |_ | |_\ \  __/ (_) |                        
# \_|  |_/\___/ \__,_|\___|_| \___/  \____/\___|\___/                         
                                                                            
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate_SELF')

# Subset
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals)
length_complete_split_total = ds.length("D8$SEX", type = "split", datasources = opals)

regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                       "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                       "InterAct_denmark","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
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



# ___  ___          _      _   __    _____                  _   _ _           
# |  \/  |         | |    | | /  |  |  _  |                | | (_) |          
# | .  . | ___   __| | ___| | `| |  | | | |_   _  __ _ _ __| |_ _| | ___      
# | |\/| |/ _ \ / _` |/ _ \ |  | |  | | | | | | |/ _` | '__| __| | |/ _ \     
# | |  | | (_) | (_| |  __/ | _| |_ \ \/' / |_| | (_| | |  | |_| | |  __/     
# \_|  |_/\___/ \__,_|\___|_| \___/  \_/\_\\__,_|\__,_|_|   \__|_|_|\___|     
                                                                            

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


# ___  ___          _      _   __   ______    _   _         ___________   ___ 
# |  \/  |         | |    | | /  |  |  ___|  | | | |       |  _  | ___ \ |_  |
# | .  . | ___   __| | ___| | `| |  | |_ __ _| |_| |_ _   _| | | | |_/ /   | |
# | |\/| |/ _ \ / _` |/ _ \ |  | |  |  _/ _` | __| __| | | | | | | ___ \   | |
# | |  | | (_) | (_| |  __/ | _| |_ | || (_| | |_| |_| |_| \ \_/ / |_/ /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \___/ \_| \__,_|\__|\__|\__, |\___/\____/\____/ 
#                                                      __/ |                  
#                                                     |___/                   

fatty_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "NHAPC", "PRHHP")]
opals_fatty = opals[fatty_studies]

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fatty)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fatty)
length_complete_split_total = ds.length("D8$SEX", type = "split", datasources = opals_fatty)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_fattysurvivaltuned_obj.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fatty)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_fattysurvivaltuned_obj.csv')


# ___  ___          _      _   __   ______    _   _         _____      _  __  
# |  \/  |         | |    | | /  |  |  ___|  | | | |       /  ___|    | |/ _| 
# | .  . | ___   __| | ___| | `| |  | |_ __ _| |_| |_ _   _\ `--.  ___| | |_  
# | |\/| |/ _ \ / _` |/ _ \ |  | |  |  _/ _` | __| __| | | |`--. \/ _ \ |  _| 
# | |  | | (_) | (_| |  __/ | _| |_ | || (_| | |_| |_| |_| /\__/ /  __/ | |   
# \_|  |_/\___/ \__,_|\___|_| \___/ \_| \__,_|\__|\__|\__, \____/ \___|_|_|   
#                                                      __/ |                  
#                                                     |___/                   

fatty_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "NHAPC", "PRHHP")]
opals_fatty = opals[fatty_studies]

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate_SELF')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fatty)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fatty)
length_complete_split_total = ds.length("D8$SEX", type = "split", datasources = opals_fatty)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_fattysurvivaltuned_SELF.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fatty)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_fattysurvivaltuned_SELF.csv')



# ___  ___          _      _   __    _                      ___________   ___ 
# |  \/  |         | |    | | /  |  | |                    |  _  | ___ \ |_  |
# | .  . | ___   __| | ___| | `| |  | |     ___  __ _ _ __ | | | | |_/ /   | |
# | |\/| |/ _ \ / _` |/ _ \ |  | |  | |    / _ \/ _` | '_ \| | | | ___ \   | |
# | |  | | (_) | (_| |  __/ | _| |_ | |___|  __/ (_| | | | \ \_/ / |_/ /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \___/ \_____/\___|\__,_|_| |_|\___/\____/\____/ 
                                                                            
lean_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "NHAPC", "InterAct_germany", "PRHHP")]
opals_lean = opals[lean_studies]

my_exposure = c('LEAN')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_lean)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_lean)
length_complete_split_lean = ds.length("D8$SEX", type = "split", datasources = opals_lean)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_leansurvivaltuned_SELF.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_lean)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_leansurvivaltuned.csv')                                   



# ___  ___          _      _   __    _                      _____      _  __  
# |  \/  |         | |    | | /  |  | |                    /  ___|    | |/ _| 
# | .  . | ___   __| | ___| | `| |  | |     ___  __ _ _ __ \ `--.  ___| | |_  
# | |\/| |/ _ \ / _` |/ _ \ |  | |  | |    / _ \/ _` | '_ \ `--. \/ _ \ |  _| 
# | |  | | (_) | (_| |  __/ | _| |_ | |___|  __/ (_| | | | /\__/ /  __/ | |   
# \_|  |_/\___/ \__,_|\___|_| \___/ \_____/\___|\__,_|_| |_\____/ \___|_|_|   
                                                                            
lean_studies = study_names[! study_names %in% c("AusDiab", "ELSA", "NHAPC", "InterAct_germany", "PRHHP")]
opals_lean = opals[lean_studies]

my_exposure = c('LEAN')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate_SELF')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_lean)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_lean)
length_complete_split_lean = ds.length("D8$SEX", type = "split", datasources = opals_lean)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_leansurvivaltuned_SELF.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_lean)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_leansurvivaltuned.csv')                                                                             


# ___  ___          _      _   __   ______    _          _ _____ _     _      
# |  \/  |         | |    | | /  |  |  ___|  (_)        | |  _  | |   (_)     
# | .  . | ___   __| | ___| | `| |  | |_ _ __ _  ___  __| | | | | |__  _      
# | |\/| |/ _ \ / _` |/ _ \ |  | |  |  _| '__| |/ _ \/ _` | | | | '_ \| |     
# | |  | | (_) | (_| |  __/ | _| |_ | | | |  | |  __/ (_| \ \_/ / |_) | |     
# \_|  |_/\___/ \__,_|\___|_| \___/ \_| |_|  |_|\___|\__,_|\___/|_.__/| |     
#                                                                    _/ |     
#                                                                   |__/      

fried_studies = study_names[! study_names %in% c("JPHC", "NOWAC", "NHAPC", "InterAct_france", "ARIC", "PRHHP")]
opals_fried = opals[fried_studies]

my_exposure = c('FRIED')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate')


# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fried)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fried)
length_complete_split_fried = ds.length("D8$SEX", type = "split", datasources = opals_fried)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_friedsurvivaltuned_SELF.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fried)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_friedsurvivaltuned.csv')


# ___  ___          _      _   __   ______    _          _ _____      _  __   
# |  \/  |         | |    | | /  |  |  ___|  (_)        | /  ___|    | |/ _|  
# | .  . | ___   __| | ___| | `| |  | |_ _ __ _  ___  __| \ `--.  ___| | |_   
# | |\/| |/ _ \ / _` |/ _ \ |  | |  |  _| '__| |/ _ \/ _` |`--. \/ _ \ |  _|  
# | |  | | (_) | (_| |  __/ | _| |_ | | | |  | |  __/ (_| /\__/ /  __/ | |    
# \_|  |_/\___/ \__,_|\___|_| \___/ \_| |_|  |_|\___|\__,_\____/ \___|_|_|    
                                                                            
fried_studies = study_names[! study_names %in% c("JPHC", "NOWAC", "NHAPC", "InterAct_france", "ARIC", "PRHHP")]
opals_fried = opals[fried_studies]

# Also need to choose between outcome OBJ or OBJ_SELF
my_exposure = c('FRIED')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")
my_exit_col = c('newEndDate_SELF')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fried)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fried)
length_complete_split_fried = ds.length("D8$SEX", type = "split", datasources = opals_fried)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_friedsurvivaltuned_SELF.svg')
model_1 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fried)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = '~/plots/model_1_friedsurvivaltuned.csv')                                                                            







######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################







# ___  ___          _      _   _____   _____     _        _ _____ _     _       
# |  \/  |         | |    | | / __  \ |_   _|   | |      | |  _  | |   (_)      
# | .  . | ___   __| | ___| | `' / /'   | | ___ | |_ __ _| | | | | |__  _       
# | |\/| |/ _ \ / _` |/ _ \ |   / /     | |/ _ \| __/ _` | | | | | '_ \| |      
# | |  | | (_) | (_| |  __/ | ./ /___   | | (_) | || (_| | \ \_/ / |_) | |      
# \_|  |_/\___/ \__,_|\___|_| \_____/   \_/\___/ \__\__,_|_|\___/|_.__/| |      
#                                                                     _/ |      
#                                                                    |__/       
studies_model2 = study_names[! study_names %in% c( "HOORN", "Zutphen","CARDIA","Whitehall")]
opals_model2 = opals[studies_model2]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')
#my_exit_col = c('newEndDate_SELF')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_model2)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_model2)
length_complete_split_ssd = ds.length("D8$SEX", type = "split", datasources = opals_model2)

# Analysis
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_survivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_model2)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_survivaltuned.csv')

# ___  ___          _      _   _____   _____     _        _ _____      _  __    
# |  \/  |         | |    | | / __  \ |_   _|   | |      | /  ___|    | |/ _|   
# | .  . | ___   __| | ___| | `' / /'   | | ___ | |_ __ _| \ `--.  ___| | |_    
# | |\/| |/ _ \ / _` |/ _ \ |   / /     | |/ _ \| __/ _` | |`--. \/ _ \ |  _|   
# | |  | | (_) | (_| |  __/ | ./ /___   | | (_) | || (_| | /\__/ /  __/ | |     
# \_|  |_/\___/ \__,_|\___|_| \_____/   \_/\___/ \__\__,_|_\____/ \___|_|_|     

studies_model2 = study_names[! study_names %in% c( "HOORN", "Zutphen","CARDIA","Whitehall")]
opals_model2 = opals[studies_model2]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate_SELF')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_model2)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_model2)
length_complete_split_ssd = ds.length("D8$SEX", type = "split", datasources = opals_model2)

# Analysis
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_survivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_model2)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_survivaltuned.csv') 

# ___  ___          _      _   _____   _____                                    
# |  \/  |         | |    | | / __  \ |  __ \                                   
# | .  . | ___   __| | ___| | `' / /' | |  \/ ___  ___                          
# | |\/| |/ _ \ / _` |/ _ \ |   / /   | | __ / _ \/ _ \                         
# | |  | | (_) | (_| |  __/ | ./ /___ | |_\ \  __/ (_) |                        
# \_|  |_/\___/ \__,_|\___|_| \_____/  \____/\___|\___/                         
                                                                              
studies_model2 = study_names[! study_names %in% c( "HOORN", "Zutphen","CARDIA","Whitehall")]
opals_model2 = opals[studies_model2]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')


# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_model2)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_model2)
length_complete_split_ssd = ds.length("D8$SEX", type = "split", datasources = opals_model2)

regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                       "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                       "InterAct_denmark","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
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


# ___  ___          _      _   _____   _____                  _   _ _           
# |  \/  |         | |    | | / __  \ |  _  |                | | (_) |          
# | .  . | ___   __| | ___| | `' / /' | | | |_   _  __ _ _ __| |_ _| | ___      
# | |\/| |/ _ \ / _` |/ _ \ |   / /   | | | | | | |/ _` | '__| __| | |/ _ \     
# | |  | | (_) | (_| |  __/ | ./ /___ \ \/' / |_| | (_| | |  | |_| | |  __/     
# \_|  |_/\___/ \__,_|\___|_| \_____/  \_/\_\\__,_|\__,_|_|   \__|_|_|\___|     
                                                                              
# Objective type
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')


quartile_studies = study_names[! study_names %in% c("CARDIA", "Zutphen", "HOORN", "Whitehall", "InterAct_france",
                                                    "NHAPC", "AusDiab")]
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
                                                                             



# ___  ___          _      _   _____  ______    _   _         ___________   ___ 
# |  \/  |         | |    | | / __  \ |  ___|  | | | |       |  _  | ___ \ |_  |
# | .  . | ___   __| | ___| | `' / /' | |_ __ _| |_| |_ _   _| | | | |_/ /   | |
# | |\/| |/ _ \ / _` |/ _ \ |   / /   |  _/ _` | __| __| | | | | | | ___ \   | |
# | |  | | (_) | (_| |  __/ | ./ /___ | || (_| | |_| |_| |_| \ \_/ / |_/ /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \_____/ \_| \__,_|\__|\__|\__, |\___/\____/\____/ 
#                                                        __/ |                  
#                                                       |___/                   


fatty_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "Zutphen", "NHAPC", "ELSA")]
opals_fatty = opals[fatty_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')

# subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fatty)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fatty)
length_complete_split_fatty = ds.length("D8$SEX", type = "split", datasources = opals_fatty)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_fattysurvivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fatty)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_fattysurvivaltuned.csv')


# ___  ___          _      _   _____  ______    _   _         _____      _  __  
# |  \/  |         | |    | | / __  \ |  ___|  | | | |       /  ___|    | |/ _| 
# | .  . | ___   __| | ___| | `' / /' | |_ __ _| |_| |_ _   _\ `--.  ___| | |_  
# | |\/| |/ _ \ / _` |/ _ \ |   / /   |  _/ _` | __| __| | | |`--. \/ _ \ |  _| 
# | |  | | (_) | (_| |  __/ | ./ /___ | || (_| | |_| |_| |_| /\__/ /  __/ | |   
# \_|  |_/\___/ \__,_|\___|_| \_____/ \_| \__,_|\__|\__|\__, \____/ \___|_|_|   
#                                                        __/ |                  
#                                                       |___/                   
fatty_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "Zutphen", "NHAPC", "ELSA")]
opals_fatty = opals[fatty_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate_SELF')

# subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fatty)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fatty)
length_complete_split_fatty = ds.length("D8$SEX", type = "split", datasources = opals_fatty)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_fattysurvivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fatty)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_fattysurvivaltuned.csv')


# ___  ___          _      _   _____   _                      ___________   ___ 
# |  \/  |         | |    | | / __  \ | |                    |  _  | ___ \ |_  |
# | .  . | ___   __| | ___| | `' / /' | |     ___  __ _ _ __ | | | | |_/ /   | |
# | |\/| |/ _ \ / _` |/ _ \ |   / /   | |    / _ \/ _` | '_ \| | | | ___ \   | |
# | |  | | (_) | (_| |  __/ | ./ /___ | |___|  __/ (_| | | | \ \_/ / |_/ /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \_____/ \_____/\___|\__,_|_| |_|\___/\____/\____/ 
                                                                              
lean_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "NHAPC", "Zutphen", "ELSA", "InterAct_germany")]
opals_lean = opals[lean_studies]

my_exposure = c('LEAN')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_lean)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_lean)
length_complete_split_lean = ds.length("D8$SEX", type = "split", datasources = opals_lean)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_leansurvivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_lean)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_leansurvivaltuned.csv')                                                                             


# ___  ___          _      _   _____   _                      _____      _  __  
# |  \/  |         | |    | | / __  \ | |                    /  ___|    | |/ _| 
# | .  . | ___   __| | ___| | `' / /' | |     ___  __ _ _ __ \ `--.  ___| | |_  
# | |\/| |/ _ \ / _` |/ _ \ |   / /   | |    / _ \/ _` | '_ \ `--. \/ _ \ |  _| 
# | |  | | (_) | (_| |  __/ | ./ /___ | |___|  __/ (_| | | | /\__/ /  __/ | |   
# \_|  |_/\___/ \__,_|\___|_| \_____/ \_____/\___|\__,_|_| |_\____/ \___|_|_|   
                                                                              
lean_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "NHAPC", "Zutphen", "ELSA", "InterAct_germany")]
opals_lean = opals[lean_studies]

my_exposure = c('LEAN')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate_SELF')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_lean)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_lean)
length_complete_split_lean = ds.length("D8$SEX", type = "split", datasources = opals_lean)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_leansurvivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_lean)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_leansurvivaltuned.csv')                                                                                   

# ___  ___          _      _   _____  ______    _          _ _____ _     _      
# |  \/  |         | |    | | / __  \ |  ___|  (_)        | |  _  | |   (_)     
# | .  . | ___   __| | ___| | `' / /' | |_ _ __ _  ___  __| | | | | |__  _      
# | |\/| |/ _ \ / _` |/ _ \ |   / /   |  _| '__| |/ _ \/ _` | | | | '_ \| |     
# | |  | | (_) | (_| |  __/ | ./ /___ | | | |  | |  __/ (_| \ \_/ / |_) | |     
# \_|  |_/\___/ \__,_|\___|_| \_____/ \_| |_|  |_|\___|\__,_|\___/|_.__/| |     
#                                                                      _/ |     
#                                                                     |__/      

fatty_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "NHAPC", "Zutphen", "ELSA", "InterAct_germany")]
opals_fatty = opals[fatty_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fatty)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fatty)
length_complete_split_lean = ds.length("D8$SEX", type = "split", datasources = opals_fatty)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_fattysurvivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fatty)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_fattysurvivaltuned.csv')  



# ___  ___          _      _   _____  ______    _          _ _____      _  __   
# |  \/  |         | |    | | / __  \ |  ___|  (_)        | /  ___|    | |/ _|  
# | .  . | ___   __| | ___| | `' / /' | |_ _ __ _  ___  __| \ `--.  ___| | |_   
# | |\/| |/ _ \ / _` |/ _ \ |   / /   |  _| '__| |/ _ \/ _` |`--. \/ _ \ |  _|  
# | |  | | (_) | (_| |  __/ | ./ /___ | | | |  | |  __/ (_| /\__/ /  __/ | |    
# \_|  |_/\___/ \__,_|\___|_| \_____/ \_| |_|  |_|\___|\__,_\____/ \___|_|_|    
                                                                              
fatty_studies = study_names[! study_names %in% c("AusDiab", "HOORN", "NHAPC", "Zutphen", "ELSA", "InterAct_germany")]
opals_fatty = opals[fatty_studies]

my_exposure = c('FATTY')
my_outcome = c('CASE_OBJ_SELF')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_exit_col = c('newEndDate_SELF')

# Subsetting
my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all)
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals_fatty)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals_fatty)
length_complete_split_lean = ds.length("D8$SEX", type = "split", datasources = opals_fatty)

# tuned survival version
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_fattysurvivaltuned_SELF.svg')
model_2 = tunedSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, my_exit_col, studies = opals_fatty)
model_2_alltuned = model_2[[1]]
model_2_remtuned = model_2[[2]]
write.csv(x = model_2_alltuned[model_2_alltuned$cov==my_exposure,], file = '~/plots/model_2_fattysurvivaltuned.csv')                                                                                  
