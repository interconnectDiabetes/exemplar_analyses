# This file serves to run all the summary code for the legumes exemplar, to seperate the task of running summaries
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
  'BIRTHWEIGHT', 'MATERNAL_EDU'
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

opals <- datashield.login(logins=logindata_7_9, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/pa')



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
none_cc_vars = c()


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


###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
summaryContExp_med <- function(column, num_studies, source_opals) {
  # given a table$column combination as a string, return the summary table 
  # for the continous variable
  summary_column_temp = ds.summary(x=column, datasources=source_opals)
  summary_column = data.frame(matrix(unlist(summary_column_temp), nrow = num_studies, ncol=10, byrow=TRUE))
  rownames(summary_column) = names(summary_column_temp)
  colnames(summary_column) = c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
  summary_column = summary_column[,c(2,6,5,7)]
  rm(summary_column_temp)
  return(summary_column)
}

summaryContExp_mean <- function(column, num_studies, source_opals) {
  
  mean_val = ds.mean(x=column, type = 'split', datasources = source_opals)
  variance = ds.var(x=column, type = 'split',datasources = source_opals)
  
  mean_unlist = unlist(mean_val)
  sd_unlist = (unlist(variance))^0.5
  summary_column = data.frame("mean" = mean_unlist, "sd" = sd_unlist)
  return(summary_column)
}

### 4-6 summaries

summary_tot_fat_mean = summaryContExp_mean('D10$TOTAL_FAT_CON', num_studies, source_opals = opals)
summary_tot_fat_med = summaryContExp_med('D10$TOTAL_FAT_CON', num_studies, source_opals = opals)
summary_height_mean = summaryContExp_mean('D10$HEIGHT', num_studies, source_opals = opals)
summary_weight_mean = summaryContExp_mean('D10$WEIGHT', num_studies, source_opals = opals)
summary_bw_mean = summaryContExp_mean('D10$BIRTHWEIGHT', num_studies, source_opals = opals)
summary_bw_med = summaryContExp_med('D10$BIRTHWEIGHT', num_studies, source_opals = opals)
summary_pi_mean = summaryContExp_mean('D10$PONDERAL_INDEX', num_studies, source_opals = opals)
summary_pi_med = summaryContExp_med('D10$PONDERAL_INDEX', num_studies, source_opals = opals)
summary_wc_corr_mean = summaryContExp_mean('D10$WC_CORRECT',  num_studies, source_opals = opals)
summary_age_mean = summaryContExp_mean('D10$AGE',  num_studies, source_opals = opals)
summary_ab_mean = summaryContExp_mean('D10$ABDOMINAL_FAT', num_studies, source_opals = opals)
summary_ab_med = summaryContExp_med('D10$ABDOMINAL_FAT', num_studies, source_opals = opals)

## additional ratio variables

# Central fat : total fat - can only run with central fat
datashield.assign(opal = opals, symbol = "AB_TO_TOTAL_RATIO", value = quote(D10$ABDOMINAL_FAT/D10$TOTAL_FAT_CON))

# WC / total fat
datashield.assign(opal = opals, symbol = "WC_TO_TOTAL_RATIO", value = quote(D10$WC_CORRECT/D10$TOTAL_FAT_CON))

# child BMI
# WC / total fat
datashield.assign(opal = opals, symbol = "CHILD_BMI", value = quote(D10$WEIGHT*10^4/(D10$HEIGHT*D10$HEIGHT)))

# Glue new columns on

ds.cbind(x=c('AB_TO_TOTAL_RATIO', "D10"), newobj = "D11", opals)

ds.cbind(x=c('WC_TO_TOTAL_RATIO','CHILD_BMI', "D10"), newobj = "D11", opals)

