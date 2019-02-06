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
filter_csv = read.csv(file = 'bw_opal_vars.csv',  header=TRUE, row.names = 1 )
setwd("~")


# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals = opals)

#all the variables in the analysis
myvars = c(
  'WEIGHT4_6', 'HEIGHT4_6', 'TOTAL_FAT4_6', 'TOTAL_FAT_CON4_6', 
  'ABDOMINAL_FAT4_6',  'WC_CORRECT4_6', 'WC4_6', 'MACROSOMIA_4000',
  'MACROSOMIA_4500', 'PONDERAL_INDEX', 'PUBERTY', 'AGE4_6', 'MATERNAL_BMI',
  'MATERNAL_OB', 'ETHNICITY', 'GDM', 'SEX', 'SMOKING', 'GESTATIONAL_AGE', 'PREECLAMPSIA',
  'BIRTHWEIGHT', 'MATERNAL_EDU'
)

myvars = c(
  'WEIGHT4_6', 'HEIGHT4_6', 'TOTAL_FAT4_6', 'TOTAL_FAT_CON4_6', 
  'ABDOMINAL_FAT4_6',  'WC_CORRECT4_6', 'WC4_6', 'MACROSOMIA_4000',
  'MACROSOMIA_4500', 'PONDERAL_INDEX', 'PUBERTY', 'AGE4_6', 'MATERNAL_BMI',
  'MATERNAL_OB', 'ETHNICITY', 'GDM', 'SEX', 'SMOKING', 'GESTATIONAL_AGE', 'PREECLAMPSIA',
  'BIRTHWEIGHT', 'MATERNAL_EDU'
)

rm(opals_4_6)
rm(opals_7_9)
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, opts = list(ssl.verifypeer=0,ssl.verifyhost=0), directory = '/home/shared/certificates/pa')



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
none_cc_vars = c( 'ABDOMINAL_FAT4_6', 'ABDOMINAL_FAT7_9', 'WC_CORRECT4_6', 'WC4_6', 'WC7_9', 'WC_CORRECT7_9')


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
# 
# opals_4_6 = opals
# studies_4_6 = which( names(opals_4_6) %in% c("ABCD", "GECKO", "GEN_R", "HSS", "ROLO", "SWS") )
# opals_4_6 = opals_4_6[studies_4_6]
# 
# num_studies_4_6 = length(opals_4_6)

### WEIGHT

pre_weight_filter = ds.length('D2c$SEX', type = 'split', opals)

weight_mean = ds.mean(x='D2c$WEIGHT4_6', type = 'split', datasources = opals)
weight_var = ds.var(x='D2c$WEIGHT4_6', type = 'split', datasources = opals)

weight_mean_unlist = unlist(weight_mean)
weight_4sd_unlist = 4*(unlist(weight_var))^0.5
weight_upper = weight_mean_unlist+weight_4sd_unlist
weight_lower = weight_mean_unlist-weight_4sd_unlist

no_hi_weight=numeric()
no_lo_weight=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D2b', subset = 'D3', logicalOperator = 'WEIGHT4_6<=', threshold = weight_upper[[i]], datasources = opals[i])
  no_hi_weight[i] <- ds.length('D3$SEX', type = 'split', opals[i])[1]
  ds.subset(x = 'D3', subset = 'D4', logicalOperator = 'WEIGHT4_6>', threshold = weight_lower[[i]], datasources = opals[i])
  no_lo_weight[i] <- ds.length('D4$SEX', type = 'split', opals[i])[1]
}

## HEIGHT

height_mean = ds.mean(x='D2$HEIGHT4_6', type = 'split', datasources = opals)
height_var_4_6 = ds.var(x='D2$HEIGHT4_6', type = 'split', datasources = opals_4_6)

height_mean_4_6_unlist = unlist(height_mean_4_6)
height_4sd_4_6_unlist = 4*(unlist(height_var_4_6))^0.5
height_4_6_upper = height_mean_4_6_unlist+height_4sd_4_6_unlist
height_4_6_lower = height_mean_4_6_unlist-height_4sd_4_6_unlist

no_hi_height_4_6=numeric()
no_lo_height_4_6=numeric()

for (i in c(1:num_studies_4_6)) {
  ds.subset(x = 'D4_4_6', subset = 'D5_4_6', logicalOperator = 'HEIGHT4_6<=', threshold = height_4_6_upper[[i]], datasources = opals_4_6[i])
  no_hi_height_4_6[i] <- ds.length('D5_4_6$SEX', type = 'split', opals_4_6[i])[1]
  ds.subset(x = 'D5_4_6', subset = 'D6_4_6', logicalOperator = 'HEIGHT4_6>', threshold = height_4_6_lower[[i]], datasources = opals_4_6[i])
  no_lo_height_4_6[i] <- ds.length('D6_4_6$SEX', type = 'split', opals_4_6[i])[1]
}

## BIRTHWEIGHT

bw_mean_4_6 = ds.mean(x='D2$BIRTHWEIGHT', type = 'split', datasources = opals_4_6)
bw_var_4_6 = ds.var(x='D2$BIRTHWEIGHT', type = 'split', datasources = opals_4_6)

bw_mean_4_6_unlist = unlist(bw_mean_4_6)
bw_4sd_4_6_unlist = 4*(unlist(bw_var_4_6))^0.5
bw_4_6_upper = bw_mean_4_6_unlist+bw_4sd_4_6_unlist
bw_4_6_lower = bw_mean_4_6_unlist-bw_4sd_4_6_unlist

no_hi_bw_4_6=numeric()
no_lo_bw_4_6=numeric()

for (i in c(1:num_studies_4_6)) {
  ds.subset(x = 'D6_4_6', subset = 'D7_4_6', logicalOperator = 'BIRTHWEIGHT<=', threshold = bw_4_6_upper[[i]], datasources = opals_4_6[i])
  no_hi_bw_4_6[i] <- ds.length('D7_4_6$SEX', type = 'split', opals_4_6[i])[1]
  ds.subset(x = 'D7_4_6', subset = 'D8_4_6', logicalOperator = 'BIRTHWEIGHT>', threshold = bw_4_6_lower[[i]], datasources = opals_4_6[i])
  no_lo_bw_4_6[i] <- ds.length('D8_4_6$SEX', type = 'split', opals_4_6[i])[1]
}

## FAT

fat_mean_4_6 = ds.mean(x='D2$TOTAL_FAT_CON4_6', type = 'split', datasources = opals_4_6)
fat_var_4_6 = ds.var(x='D2$TOTAL_FAT_CON4_6', type = 'split', datasources = opals_4_6)

fat_mean_4_6_unlist = unlist(fat_mean_4_6)
fat_4sd_4_6_unlist = 4*(unlist(fat_var_4_6))^0.5
fat_4_6_upper = fat_mean_4_6_unlist+fat_4sd_4_6_unlist
fat_4_6_lower = fat_mean_4_6_unlist-fat_4sd_4_6_unlist

no_hi_fat_4_6=numeric()
no_lo_fat_4_6=numeric()

for (i in c(1:num_studies_4_6)) {
  ds.subset(x = 'D8_4_6', subset = 'D9_4_6', logicalOperator = 'TOTAL_FAT_CON4_6<=', threshold = fat_4_6_upper[[i]], datasources = opals_4_6[i])
  no_hi_fat_4_6[i] <- ds.length('D9_4_6$SEX', type = 'split', opals_4_6[i])[1]
  ds.subset(x = 'D9_4_6', subset = 'D10_4_6', logicalOperator = 'TOTAL_FAT_CON4_6>', threshold = fat_4_6_lower[[i]], datasources = opals_4_6[i])
  no_lo_fat_4_6[i] <- ds.length('D10_4_6$SEX', type = 'split', opals_4_6[i])[1]
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

summary_tot_fat_4_6_mean = summaryContExp_mean('D10_4_6$TOTAL_FAT_CON4_6', num_studies_4_6, source_opals = opals_4_6)
summary_tot_fat_4_6_med = summaryContExp_med('D10_4_6$TOTAL_FAT_CON4_6', num_studies_4_6, source_opals = opals_4_6)
summary_height_4_6_mean = summaryContExp_mean('D10_4_6$HEIGHT4_6', num_studies_4_6, source_opals = opals_4_6)
summary_weight_4_6_mean = summaryContExp_mean('D10_4_6$WEIGHT4_6', num_studies_4_6, source_opals = opals_4_6)
summary_bw_4_6_mean = summaryContExp_mean('D10_4_6$BIRTHWEIGHT', num_studies_4_6, source_opals = opals_4_6)
summary_bw_4_6_med = summaryContExp_med('D10_4_6$BIRTHWEIGHT', num_studies_4_6, source_opals = opals_4_6)
summary_pi_4_6_mean = summaryContExp_mean('D10_4_6$PONDERAL_INDEX', num_studies_4_6, source_opals = opals_4_6)
summary_pi_4_6_med = summaryContExp_med('D10_4_6$PONDERAL_INDEX', num_studies_4_6, source_opals = opals_4_6)

