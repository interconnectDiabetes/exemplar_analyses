# This file serves to run all the analysis code for the birthweight exemplar, to seperate the task of running summaries
# and performing the complex models

## Author: 
##		   Tom Bishop
## Date: 06/02/2019

###############################################################################
########################### Dependencies   ####################################
###############################################################################
library(opal)
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(metafor)

###############################################################################
########################### SET UP SERVERS  ###################################
###############################################################################
# Set working directory to source our credentials
#setwd("/home/l_pms69/exemplar_analyses/")
setwd("/home/l_trpb2/git/exemplar_analyses/")

# Source in the Extra functions for analysis

source("variable_functions.R")
# Retrieve Credential Details
source("creds/bw_exemplar_creds.R")

# complete cases per study only on variables not known to be all missing
setwd("/home/l_trpb2/git/exemplar_analyses/bw_exemplar")
filter_csv = read.csv(file = 'bw_opal_vars_new.csv',  header=TRUE, row.names = 1 )
setwd("~")


# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals = opals)

#all the variables in the analysis
myvars = c(
  'WEIGHT','HEIGHT', 'TOTAL_FAT', 'TOTAL_FAT_CON', 
  'ABDOMINAL_FAT', 'WC_CORRECT', 'WC', 'MACROSOMIA_4000',
  'MACROSOMIA_4500', 'PONDERAL_INDEX', 'PUBERTY', 'AGE', 'MATERNAL_BMI',
  'MATERNAL_OB', 'ETHNICITY', 'GDM', 'SEX', 'SMOKING', 'GESTATIONAL_AGE', 'PREECLAMPSIA',
  'BIRTHWEIGHT', 'MATERNAL_EDU', "BODY_FAT", "AB_TO_TOTAL", "WC_TO_TOTAL"
)

# myvars = c(
#   'WEIGHT7_9', 'HEIGHT7_9', 'TOTAL_FAT_CON7_9', 'ABDOMINAL_FAT7_9', 'WC7_9', 'WC_CORRECT7_9',
#   'MACROSOMIA_4000','MACROSOMIA_4500', 'PONDERAL_INDEX', 'PUBERTY', 'AGE7_9', 'TOTAL_FAT7_9',
#   'MATERNAL_BMI', 'MATERNAL_OB', 'ETHNICITY', 'GDM', 'SEX', 'SMOKING',
#   'GESTATIONAL_AGE', 'PREECLAMPSIA', 'BIRTHWEIGHT', 'MATERNAL_EDU'
# )

# myvars = c(
#   'WEIGHT4_6', 'WEIGHT7_9', 'HEIGHT4_6', 'HEIGHT7_9', 'TOTAL_FAT4_6', 'TOTAL_FAT_CON4_6', 'TOTAL_FAT_CON7_9',
#   'ABDOMINAL_FAT4_6', 'ABDOMINAL_FAT7_9', 'WC_CORRECT4_6', 'WC4_6', 'WC7_9', 'WC_CORRECT7_9', 'MACROSOMIA_4000',
#   'MACROSOMIA_4500', 'PONDERAL_INDEX', 'PUBERTY', 'AGE4_6', 'AGE7_9', 'TOTAL_FAT7_9', 'MATERNAL_BMI',
#   'MATERNAL_OB', 'ETHNICITY', 'GDM', 'SEX', 'SMOKING', 'GESTATIONAL_AGE', 'PREECLAMPSIA',
#   'BIRTHWEIGHT', 'MATERNAL_EDU'
# )



#opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/pa')

#opals <- datashield.login(logins=logindata_4_6, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/pa')

#opals <- datashield.login(logins=logindata_7_9, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/pa')

opals <- datashield.login(logins=logindata_GEN_SWS_4_6, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/pa')

#opals <- datashield.login(logins=logindata_GEN_SWS_7_9, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/pa')

## define outcomes

ds.assign(toAssign="D$TOTAL_FAT_CON/(D$HEIGHT*D$HEIGHT)", newobj = "BODY_FAT", opals)
ds.assign(toAssign="D$ABDOMINAL_FAT/D$TOTAL_FAT_CON", newobj = "AB_TO_TOTAL", opals)
ds.assign(toAssign="D$WC_CORRECT/D$TOTAL_FAT_CON", newobj = "WC_TO_TOTAL", opals)
ds.cbind(x=c("D", "BODY_FAT", "AB_TO_TOTAL", "WC_TO_TOTAL"), newobj = "D", opals)


# Set studynames and numstudies
temp <- ds.summary('D$SEX', datasources = opals)
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

fullNum = ds.length('D$SEX', type = 'split') 

pre_missings_table = data.frame(cbind(fullNum))
types_table = data.frame(cbind(study_names))

for (j in 1:length(myvars)){
  print(paste0(j," start"))
  missing_vect = ds.numNA(paste0('D$',myvars[j]))
  pre_missings_table = cbind(pre_missings_table,unlist(missing_vect))
  type_vect = ds.class(paste0('D$',myvars[j]))
  types_table = cbind(types_table,unlist(type_vect))
  print(paste0(j," end"))
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
# basic counts
all_infants <- ds.length('D$SEX', type = 'split')

# remove preterm <37w
ds.subset(x = 'D', subset = 'D1', logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37)
no_preterm <- ds.length('D1$SEX', type = 'split')

# remove preeclampsia
ds.subset(x = 'D1', subset = 'D2', logicalOperator = 'PREECLAMPSIA==', threshold = 0)
no_preecl <- ds.length('D2$SEX', type = 'split')


## COMPLETE CASES


#variables not to be used in complete cases according to analysis plan
#none_cc_vars = c( 'ABDOMINAL_FAT' 'WC', 'WC_CORRECT')
#none_cc_vars = c( 'ABDOMINAL_FAT')
none_cc_vars = c("BODY_FAT", "AB_TO_TOTAL", "WC_TO_TOTAL")


for (i in c(1:num_studies)) {
  my_name = names(opals[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  ds.subset(x = 'D2', subset = 'D2a', cols =  list_variables, datasources = opals[i])
  ds.subset(x = 'D2a', subset = 'D2b', completeCases = TRUE, datasources = opals[i])
}
rm(i)
length_complete = ds.length("D2b$SEX", type = "split", datasources = opals)

model_all_len <- data.frame()
#model_all_len <- rbind(model_all_len, no_preterm, all_infants, no_preecl,no_hi_weight_4_6, no_lo_weight_4_6,
#                       no_hi_height_4_6, no_lo_height_4_6, no_hi_bw_4_6, no_lo_bw_4_6, no_hi_fat_4_6,
#                       no_lo_fat_4_6, length_complete)

model_all_len <- rbind(model_all_len, all_infants,no_preterm,no_preecl, length_complete)


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
    ds.cbind(x=c(list_variables, "D2b"), newobj = "D2c", opals[i])
  }
  else {
    datashield.assign(opals[i],"D2c",quote(D2b))
  }
}


post_missings_table = data.frame(cbind(study_names, length_complete))

for (j in 1:length(myvars)){
  
  missing_vect = ds.numNA(paste0('D2c$',myvars[j]))
  post_missings_table = cbind(post_missings_table,unlist(missing_vect))
}

colnames(post_missings_table) <- c('Study Name', 'Total in Study', myvars)
post_missings_table = t(post_missings_table)
post_missings_table = as.data.frame(post_missings_table)



### Find SDs for 4-6 studies


### WEIGHT

pre_weight_filter = ds.length('D2$SEX', type = 'split', opals)

weight_mean = ds.mean(x='D2$WEIGHT', type = 'split', datasources = opals)
weight_var = ds.var(x='D2$WEIGHT', type = 'split', datasources = opals)

weight_mean_unlist = unlist(weight_mean)
weight_4sd_unlist = 4*(unlist(weight_var))^0.5
weight_upper = weight_mean_unlist+weight_4sd_unlist
weight_lower = weight_mean_unlist-weight_4sd_unlist

no_hi_weight=numeric()
no_lo_weight=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D2c', subset = 'D3', logicalOperator = 'WEIGHT<=', threshold = weight_upper[[i]], datasources = opals[i])
  no_hi_weight[i] <- ds.length('D3$SEX', type = 'split', opals[i])[1]
  ds.subset(x = 'D3', subset = 'D4', logicalOperator = 'WEIGHT>', threshold = weight_lower[[i]], datasources = opals[i])
  no_lo_weight[i] <- ds.length('D4$SEX', type = 'split', opals[i])[1]
}

## HEIGHT

height_mean = ds.mean(x='D2$HEIGHT', type = 'split', datasources = opals)
height_var = ds.var(x='D2$HEIGHT', type = 'split', datasources = opals)

height_mean_unlist = unlist(height_mean)
height_4sd_unlist = 4*(unlist(height_var))^0.5
height_upper = height_mean_unlist+height_4sd_unlist
height_lower = height_mean_unlist-height_4sd_unlist

no_hi_height=numeric()
no_lo_height=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D4', subset = 'D5', logicalOperator = 'HEIGHT<=', threshold = height_upper[[i]], datasources = opals[i])
  no_hi_height[i] <- ds.length('D5$SEX', type = 'split', opals[i])[1]
  ds.subset(x = 'D5', subset = 'D6', logicalOperator = 'HEIGHT>', threshold = height_lower[[i]], datasources = opals[i])
  no_lo_height[i] <- ds.length('D6$SEX', type = 'split', opals[i])[1]
}

## BIRTHWEIGHT

bw_mean = ds.mean(x='D2$BIRTHWEIGHT', type = 'split', datasources = opals)
bw_var = ds.var(x='D2$BIRTHWEIGHT', type = 'split', datasources = opals)

bw_mean_unlist = unlist(bw_mean)
bw_4sd_unlist = 4*(unlist(bw_var))^0.5
bw_upper = bw_mean_unlist+bw_4sd_unlist
bw_lower = bw_mean_unlist-bw_4sd_unlist

no_hi_bw=numeric()
no_lo_bw=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D6', subset = 'D7', logicalOperator = 'BIRTHWEIGHT<=', threshold = bw_upper[[i]], datasources = opals[i])
  no_hi_bw[i] <- ds.length('D7$SEX', type = 'split', opals[i])[1]
  ds.subset(x = 'D7', subset = 'D8', logicalOperator = 'BIRTHWEIGHT>', threshold = bw_lower[[i]], datasources = opals[i])
  no_lo_bw[i] <- ds.length('D8$SEX', type = 'split', opals[i])[1]
}

## FAT

fat_mean = ds.mean(x='D2$TOTAL_FAT_CON', type = 'split', datasources = opals)
fat_var = ds.var(x='D2$TOTAL_FAT_CON', type = 'split', datasources = opals)

fat_mean_unlist = unlist(fat_mean)
fat_4sd_unlist = 4*(unlist(fat_var))^0.5
fat_upper = fat_mean_unlist+fat_4sd_unlist
fat_lower = fat_mean_unlist-fat_4sd_unlist

no_hi_fat=numeric()
no_lo_fat=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D8', subset = 'D9', logicalOperator = 'TOTAL_FAT_CON<=', threshold = fat_upper[[i]], datasources = opals[i])
  no_hi_fat[i] <- ds.length('D9$SEX', type = 'split', opals[i])[1]
  ds.subset(x = 'D9', subset = 'D10', logicalOperator = 'TOTAL_FAT_CON>', threshold = fat_lower[[i]], datasources = opals[i])
  no_lo_fat[i] <- ds.length('D10$SEX', type = 'split', opals[i])[1]
}


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

my_exposure = c('BIRTHWEIGHT')
#my_exposure = c('MACROSOMIA_4000')
#my_exposure = c('MACROSOMIA_4500')
#my_exposure = c('PONDERAL_INDEX')

my_covariate =  c("AGE", "SEX", "EDUCATION", "SMOKING", "ETHNICITY", "ALCOHOL", "HEIGHT")


my_outcome = c('BODY_FAT')
#my_outcome = c('ABDOMINAL_FAT')
#my_outcome = c('WC_CORRECT')
#my_outcome = c('AB_TO_TOTAL')
#my_outcome = c('WC_TO_TOTAL')

ref_table =  'D10'

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


# tuned survival version

mypath = file.path('~', 'plots/bw/', paste0('model_1_',my_exposure,'_',ref_table,'.svg'))
model_1 = runRegModel(ref_table = ref_table, my_exposure = my_exposure, my_outcome = my_outcome, my_covariate = my_covariate, mypath, studies = temp_opals)
model_1_alltuned = model_1[[1]]
model_1_remtuned = model_1[[2]]
write.csv(x = model_1_alltuned[model_1_alltuned$cov==my_exposure,], file = paste0('~/plots/bw/model_1_',my_exposure,'_',ref_table,'.csv'))
rm(temp_opals)




