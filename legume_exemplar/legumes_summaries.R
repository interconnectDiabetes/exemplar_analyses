# This file serves to run all the summary code for the fish exemplar, to seperate the task of running summaries
# and performing the complex models

## Author: Paul Scherer
##		   Tom Bishop
## Date: 06/06/2017

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
source("creds/leg_exemplar_creds.R")
# complete cases per study only on variables not known to be all missing
setwd("/home/l_trpb2/git/exemplar_analyses/legume_exemplar")
filter_csv = read.csv(file = 'leg_opal_vars.csv',  header=TRUE, row.names = 1 )
setwd("~")

# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals)

#all the variables in the analysis
myvars = c(
  'AGE_BASE', 'TYPE_DIAB', 'PREV_DIAB', 'CASE_OBJ_SELF', 'CASE_OBJ', 'FUP_OBJ', 'FUP_OBJ_SELF', 'PBCL',
  'SOY', 'NUTS_SEEDS', 'ISOFLAVONES', 'TOTAL', 'SEX', 'BMI', 'EDUCATION', 'SMOKING', 'PA', 'ALCOHOL',
  'FAM_DIAB', 'COMORBIDITY', 'E_INTAKE', 'COV_FRUIT', 'COV_VEG', 'COV_FIBER', 'COV_MEAT', 'COV_SUG_BEVS', 'WAIST'
)


opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/fish')


# # To include all possible variables uncomment this line and and comment out previus line
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish')

# Set studynames and numstudies
temp <- ds.summary('D$TOTAL', datasources = opals)
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)



########################################################################
#### CHECK MISSINGS BEFORE FILTERING ###################################
########################################################################

#check the variables are there
all_vars = ds.summary('D', opals)
all_vars = as.data.frame(lapply(X=all_vars,FUN = function(x){
  temp = sort(x[[4]])
}))

fullNum = ds.length('D$AGE_BASE', type = 'split') 

pre_missings_table = data.frame(cbind(fullNum))
types_table = data.frame(cbind(study_names))

for (j in 1:length(myvars)){
  
  missing_vect = ds.numNA(paste0('D$',myvars[j]))
  pre_missings_table = cbind(pre_missings_table,unlist(missing_vect))
  type_vect = ds.class(paste0('D$',myvars[j]))
  types_table = cbind(types_table,unlist(type_vect))
  
}

colnames(pre_missings_table) <- c('Total in Study', myvars)
pre_missings_table = t(pre_missings_table)
pre_missings_table = as.data.frame(pre_missings_table)

types_table = types_table[,-1]
colnames(types_table) = myvars
types_summary = apply(X = types_table, MARGIN = 2, FUN = function(x){names(sort(table(x),decreasing=TRUE)[1])})

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




#variables not to be used in complete cases
none_cc_vars = c("WAIST","SUPPLEMENTS", "FAM_DIAB")

for (i in c(1:num_studies)) {
  my_name = names(opals[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  ds.subset(x = 'E4', subset = 'E5', cols =  list_variables, datasources = opals[i])
  ds.subset(x = 'E5', subset = 'E6', completeCases = TRUE, datasources = opals[i])
}
rm(i)
length_complete = ds.length("E6$AGE_BASE", type = "split", datasources = opals)

model_all_len <- data.frame()
model_all_len <- rbind(model_all_len, all_participants_split, noPrevalence, noType1, under3500cal, afterIntake,length_complete)


###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
summaryContExp <- function(column, study_names, num_studies) {
  # given a table$column combination as a string, return the summary table 
  # for the continous variable
  summary_column_temp = ds.summary(column)
  summary_column = data.frame(matrix(unlist(summary_column_temp), nrow = num_studies, ncol=10, byrow=TRUE))
  rownames(summary_column) = study_names
  colnames(summary_column) = c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
  summary_column = summary_column[,c(2,6,5,7)]
  rm(summary_column_temp)
  return(summary_column)
}

summaryBinExp <- function(column, study_names, num_studies) {
  # given a table$column combination as a string, return the summary
  # table for the binary variable
  summary_column_temp = ds.summary(column)
  summary_column = data.frame(matrix(unlist(summary_column_temp), nrow = num_studies, ncol=6, byrow=TRUE))
  rownames(summary_column) <- study_names
  colnames(summary_column) <- c('type', 'n', '0', '1', 'No', 'Yes')
  rm(summary_column_temp)
  return(summary_column)
}

summaryCatExp <- function (column, study_names, num_studies, levels = 2){
  # given a table$column combination as a string, return the overall summary for categorical
  # variables set in the levels parameter.
  summary_column_temp = ds.summary(column)
  summary_column = data.frame(matrix(unlist(summary_column_temp), nrow=num_studies, ncol=(2+(2*levels)), byrow = TRUE))
  rownames(summary_column) <- study_names
  colnames(summary_column) = c('type', 'n')
  rm(summary_column_temp)
  return(summary_column)
}


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


post_missings_table = data.frame(cbind(study_names, length_complete))

for (j in 1:length(myvars)){
  
  missing_vect = ds.numNA(paste0('E7$',myvars[j]))
  post_missings_table = cbind(post_missings_table,unlist(missing_vect))
}

colnames(post_missings_table) <- c('Study Name', 'Total in Study', myvars)
post_missings_table = t(post_missings_table)
post_missings_table = as.data.frame(post_missings_table)




#---------------------------------------------------------
# Summaries for exposures
summary_nuts_seeds = summaryContExp('E7$NUTS_SEEDS', study_names, num_studies)
summary_soy = summaryContExp('E7$SOY', study_names, num_studies)
summary_pbcl = summaryContExp('E7$PBCL', study_names, num_studies)
summary_isoflavones = summaryContExp('E7$ISOFLAVONES', study_names, num_studies)
summary_total = summaryContExp('E7$TOTAL', study_names, num_studies)
                               
#---------------------------------------------------------
# Summaries for outcomes
summary_objective_case = summaryBinExp('E7$CASE_OBJ', study_names, num_studies)
summary_self_case = summaryBinExp("E7$CASE_OBJ_SELF", study_names, num_studies)

summary_age_base = summaryContExp("E7$AGE_BASE", study_names, num_studies)

summary_prevalence = summaryBinExp("E7$PREV_DIAB", study_names, num_studies)
summary_type_diab = summaryBinExp("E7$TYPE_DIAB", study_names, num_studies)

summary_fup_self = summaryContExp("E7$FUP_OBJ_SELF", study_names, num_studies)
summary_fup_obj = summaryContExp("E7$FUP_OBJ", study_names, num_studies)

#---------------------------------------------------------
# Summaries for covariates and confounders
# education
summary_education = summaryCatExp('E7$EDUCATION', study_names, num_studies)
summary_smoking = summaryBinExp('E7$SMOKING', study_names, num_studies)

# # mi, stroke, cancer, hypertension
summary_comorbid = summaryBinExp('E7$COMORBIDITY', study_names, num_studies)

# # Continous covariates
summary_pa = summaryContExp('E7$PA', study_names, num_studies)
summary_alc = summaryContExp('E7$ALCOHOL', study_names, num_studies)
summary_eintake = summaryContExp('E7$E_INTAKE', study_names, num_studies)
summary_meat = summaryContExp('E7$COV_MEAT', study_names, num_studies)
summary_fruit = summaryContExp('E7$COV_FRUIT', study_names, num_studies)
summary_veg = summaryContExp('E7$COV_VEG', study_names, num_studies)
summary_fiber = summaryContExp('E7$COV_FIBER', study_names, num_studies)
summary_sugardrinks = summaryContExp('E7$COV_SUG_BEVS', study_names, num_studies)

# Other covariates
summary_bmi = summaryBinExp('E7$BMI', study_names, num_studies)
summary_sex = summaryCatExp('E7$SEX', study_names, num_studies)
