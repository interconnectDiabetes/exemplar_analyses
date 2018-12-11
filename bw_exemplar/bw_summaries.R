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
  'WEIGHT4_6', 'WEIGHT7_9', 'HEIGHT4_6', 'HEIGHT7_9', 'TOTAL_FAT4_6', 'TOTAL_FAT_CON4_6', 'TOTAL_FAT_CON7_9',
  'ABDOMINAL_FAT4_6', 'ABDOMINAL_FAT7_9', 'WC_CORRECT4_6', 'WC4_6', 'WC7_9', 'WC_CORRECT7_9', 'MACROSOMIA_4000',
  'MACROSOMIA_4500', 'PONDERAL_INDEX', 'PUBERTY', 'AGE4_6', 'AGE7_9', 'TOTAL_FAT7_9', 'MATERNAL_BMI',
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




### Find SDs for 4-6 studies

opals_4_6 = opals
studies_4_6 = which( names(opals_4_6) %in% c("ABCD", "GECKO", "GEN_R", "HSS", "ROLO", "SWS") )
opals_4_6 = opals_4_6[studies_4_6]

num_studies = length(opals_4_6)

### WEIGHT

pre_weight_4_6_filter = ds.length('D2b$SEX', type = 'split', opals_4_6)

weight_mean_4_6 = ds.mean(x='D2b$WEIGHT4_6', type = 'split', datasources = opals_4_6)
weight_var_4_6 = ds.var(x='D2b$WEIGHT4_6', type = 'split', datasources = opals_4_6)

weight_mean_4_6_unlist = unlist(weight_mean_4_6)
weight_4sd_4_6_unlist = 4*(unlist(weight_var_4_6))^0.5
weight_4_6_upper = weight_mean_4_6_unlist+weight_4sd_4_6_unlist
weight_4_6_lower = weight_mean_4_6_unlist-weight_4sd_4_6_unlist

no_hi_weight_4_6=numeric()
no_lo_weight_4_6=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D2b', subset = 'D3_4_6', logicalOperator = 'WEIGHT4_6<=', threshold = weight_4_6_upper[[i]], datasources = opals_4_6[i])
no_hi_weight_4_6[i] <- ds.length('D3_4_6$SEX', type = 'split', opals_4_6[i])[1]
ds.subset(x = 'D3_4_6', subset = 'D4_4_6', logicalOperator = 'WEIGHT4_6>', threshold = weight_4_6_lower[[i]], datasources = opals_4_6[i])
no_lo_weight_4_6[i] <- ds.length('D4_4_6$SEX', type = 'split', opals_4_6[i])[1]
}

## HEIGHT

height_mean_4_6 = ds.mean(x='D2$HEIGHT4_6', type = 'split', datasources = opals_4_6)
height_var_4_6 = ds.var(x='D2$HEIGHT4_6', type = 'split', datasources = opals_4_6)

height_mean_4_6_unlist = unlist(height_mean_4_6)
height_4sd_4_6_unlist = 4*(unlist(height_var_4_6))^0.5
height_4_6_upper = height_mean_4_6_unlist+height_4sd_4_6_unlist
height_4_6_lower = height_mean_4_6_unlist-height_4sd_4_6_unlist

no_hi_height_4_6=numeric()
no_lo_height_4_6=numeric()

for (i in c(1:num_studies)) {
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

for (i in c(1:num_studies)) {
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

for (i in c(1:num_studies)) {
  ds.subset(x = 'D8_4_6', subset = 'D9_4_6', logicalOperator = 'TOTAL_FAT_CON4_6<=', threshold = fat_4_6_upper[[i]], datasources = opals_4_6[i])
  no_hi_fat_4_6[i] <- ds.length('D9_4_6$SEX', type = 'split', opals_4_6[i])[1]
  ds.subset(x = 'D9_4_6', subset = 'D10_4_6', logicalOperator = 'TOTAL_FAT_CON4_6>', threshold = fat_4_6_lower[[i]], datasources = opals_4_6[i])
  no_lo_fat_4_6[i] <- ds.length('D10_4_6$SEX', type = 'split', opals_4_6[i])[1]
}

### Find SDs for 7-9 studies

opals_7_9 = opals
studies_7_9 = which( names(opals_7_9) %in% c("ALSPAC", "GEN_R", "REPRO", "SWS") )
opals_7_9 = opals_7_9[studies_7_9]

num_studies = length(opals_7_9)

### WEIGHT

pre_weight_7_9_filter = ds.length('D2$SEX', type = 'split', opals_7_9)

weight_mean_7_9 = ds.mean(x='D2$WEIGHT7_9', type = 'split', datasources = opals_7_9)
weight_var_7_9 = ds.var(x='D2$WEIGHT7_9', type = 'split', datasources = opals_7_9)

weight_mean_7_9_unlist = unlist(weight_mean_7_9)
weight_4sd_7_9_unlist = 4*(unlist(weight_var_7_9))^0.5
weight_7_9_upper = weight_mean_7_9_unlist+weight_4sd_7_9_unlist
weight_7_9_lower = weight_mean_7_9_unlist-weight_4sd_7_9_unlist

no_hi_weight_7_9=numeric()
no_lo_weight_7_9=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D2', subset = 'D3_7_9', logicalOperator = 'WEIGHT7_9<=', threshold = weight_7_9_upper[[i]], datasources = opals_7_9[i])
  no_hi_weight_7_9[i] <- ds.length('D3_7_9$SEX', type = 'split', opals_7_9[i])[1]
  ds.subset(x = 'D3_7_9', subset = 'D4_7_9', logicalOperator = 'WEIGHT7_9>', threshold = weight_7_9_lower[[i]], datasources = opals_7_9[i])
  no_lo_weight_7_9[i] <- ds.length('D4_7_9$SEX', type = 'split', opals_7_9[i])[1]
}

## HEIGHT

height_mean_7_9 = ds.mean(x='D2$HEIGHT7_9', type = 'split', datasources = opals_7_9)
height_var_7_9 = ds.var(x='D2$HEIGHT7_9', type = 'split', datasources = opals_7_9)

height_mean_7_9_unlist = unlist(height_mean_7_9)
height_4sd_7_9_unlist = 4*(unlist(height_var_7_9))^0.5
height_7_9_upper = height_mean_7_9_unlist+height_4sd_7_9_unlist
height_7_9_lower = height_mean_7_9_unlist-height_4sd_7_9_unlist

no_hi_height_7_9=numeric()
no_lo_height_7_9=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D4_7_9', subset = 'D5_7_9', logicalOperator = 'HEIGHT7_9<=', threshold = height_7_9_upper[[i]], datasources = opals_7_9[i])
  no_hi_height_7_9[i] <- ds.length('D5_7_9$SEX', type = 'split', opals_7_9[i])[1]
  ds.subset(x = 'D5_7_9', subset = 'D6_7_9', logicalOperator = 'HEIGHT7_9>', threshold = height_7_9_lower[[i]], datasources = opals_7_9[i])
  no_lo_height_7_9[i] <- ds.length('D6_7_9$SEX', type = 'split', opals_7_9[i])[1]
}

## BIRTHWEIGHT

bw_mean_7_9 = ds.mean(x='D2$BIRTHWEIGHT', type = 'split', datasources = opals_7_9)
bw_var_7_9 = ds.var(x='D2$BIRTHWEIGHT', type = 'split', datasources = opals_7_9)

bw_mean_7_9_unlist = unlist(bw_mean_7_9)
bw_4sd_7_9_unlist = 4*(unlist(bw_var_7_9))^0.5
bw_7_9_upper = bw_mean_7_9_unlist+bw_4sd_7_9_unlist
bw_7_9_lower = bw_mean_7_9_unlist-bw_4sd_7_9_unlist

no_hi_bw_7_9=numeric()
no_lo_bw_7_9=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D6_7_9', subset = 'D7_7_9', logicalOperator = 'BIRTHWEIGHT<=', threshold = bw_7_9_upper[[i]], datasources = opals_7_9[i])
  no_hi_bw_7_9[i] <- ds.length('D7_7_9$SEX', type = 'split', opals_7_9[i])[1]
  ds.subset(x = 'D7_7_9', subset = 'D8_7_9', logicalOperator = 'BIRTHWEIGHT>', threshold = bw_7_9_lower[[i]], datasources = opals_7_9[i])
  no_lo_bw_7_9[i] <- ds.length('D8_7_9$SEX', type = 'split', opals_7_9[i])[1]
}

## FAT

fat_mean_7_9 = ds.mean(x='D2$TOTAL_FAT_CON7_9', type = 'split', datasources = opals_7_9)
fat_var_7_9 = ds.var(x='D2$TOTAL_FAT_CON7_9', type = 'split', datasources = opals_7_9)

fat_mean_7_9_unlist = unlist(fat_mean_7_9)
fat_4sd_7_9_unlist = 4*(unlist(fat_var_7_9))^0.5
fat_7_9_upper = fat_mean_7_9_unlist+fat_4sd_7_9_unlist
fat_7_9_lower = fat_mean_7_9_unlist-fat_4sd_7_9_unlist

no_hi_fat_7_9=numeric()
no_lo_fat_7_9=numeric()

for (i in c(1:num_studies)) {
  ds.subset(x = 'D8_7_9', subset = 'D9_7_9', logicalOperator = 'TOTAL_FAT_CON7_9<=', threshold = fat_7_9_upper[[i]], datasources = opals_7_9[i])
  no_hi_fat_7_9[i] <- ds.length('D9_7_9$SEX', type = 'split', opals_7_9[i])[1]
  ds.subset(x = 'D9_7_9', subset = 'D10_7_9', logicalOperator = 'TOTAL_FAT_CON7_9>', threshold = fat_7_9_lower[[i]], datasources = opals_7_9[i])
  no_lo_fat_7_9[i] <- ds.length('D10_7_9$SEX', type = 'split', opals_7_9[i])[1]
}





