## Summary script for third trimester PAIP analysis
## Author: Tom Bishop
##         Paul Scherer
## Date: 29/07/2016

## Datasets:
## DNBC
## GECKO
## HSS
## REPRO
## SWS

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
setwd("/home/l_pms69/exemplar_analyses/")
#setwd("/home/l_trpb2/git/exemplar_analyses/")

# Sourcing the credentials sets values for the following variables:
# server
# url
# table
# password 
# user
# logindata_all

source("creds/pa_exemplar_3_creds.R")
setwd("~")
datashield.logout(opals)
myvars = list('MOD_VIG_3_filt', 'LTPA_DUR_3_filt', 'LTPA_EE_3_filt', 'VIG_3_filt','BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA', 'BIRTH_WEIGHT_SGA',
              'GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING','ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 
              'GDM', 'MATERNAL_BMI', 'MATERNAL_OB', 'PREECLAMPSIA')
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/pa')

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

#variables for model 1
#need to generate a 'temp' variable with no missings and add this in,
#because the ds.subset command needs at least 2 columns to work with (a bug, I think)
for(i in 1:length(opals)){
  work1 <- no_preecl[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'temp', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('temp','D2'), newobj='D2a')

#for GECKO only dummy variable for LTPA_DUR_3_filt since this does not exist
work1 <- no_preecl$GECKO
work2 <- paste0("datashield.assign(opals[\"GECKO\"],'LTPA_DUR_3_filt', quote(rep(1,",work1,")))")
eval(parse(text=work2))
ds.cbind(x=c('temp','D2','LTPA_DUR_3_filt'), newobj='D2a', datasource=opals["GECKO"])

# Filter out missing values
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

# Variables used within analysis
my_exp_all = c('MOD_VIG_3_filt', 'LTPA_DUR_3_filt', 'LTPA_EE_3_filt', 'VIG_3_filt')
my_outcome_all = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA', 'BIRTH_WEIGHT_SGA')
my_cov_all = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
               'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM', 'MATERNAL_BMI', 'MATERNAL_OB')

# Loop to produce E4 and model_all_len for descriptive stats
# my_vars_all <- c('temp', my_exp_all, my_outcome_all, my_cov_all)
# model_all_len <- data.frame()
# 
# for (i in 2:length(my_vars_all)){
#   ds.subset(x = 'D2a', subset = 'E3', cols =  c(my_vars_all[1:i]))
#   ds.subset(x = 'E3', subset = 'E4', completeCases = TRUE)
#   model_all_len <- rbind(model_all_len,ds.length('E4$temp', type = 'split'))
# }
# 
# row.names(model_all_len) <- my_vars_all[2:length(my_vars_all)]

# Generate E4 without the loop, doesnt produce model_all_len
my_vars_all <- c(my_exp_all, my_outcome_all, my_cov_all)
ds.subset(x = 'D2a', subset = 'E3', cols =  my_vars_all)
ds.subset(x = 'E3', subset = 'E4', completeCases = TRUE)


# NOTE ON MODVIG GREATER THAN ZERO
ds.subset(x = 'E4', subset = 'E5', logicalOperator = 'MOD_VIG_filt>', threshold = 0)
ds.summary('E5$MOD_VIG')


###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
#---------------------------------------------------------
# Summaries for covariates and confounders

# Sex
summary_sex_temp <- ds.summary('E4$SEX')
summary_sex <- data.frame(matrix(unlist(summary_sex_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_sex) <- study_names
colnames(summary_sex) <- c("type", "N", "male", "female", "count0", "count1")
rm(summary_sex_temp)


# gestational age by sex - most of the code is to sort out formatting
summary_ga_temp <- ds.meanByClass(x='E4$GESTATIONAL_AGE~E4$SEX', type = 'split')
summary_ga <- matrix(unlist(summary_ga_temp))
summary_ga <- substr(summary_ga, 1, nchar(summary_ga)-1)
summary_ga <- summary_ga[seq(2, nrow(summary_ga), by = 2),]
summary_ga <- do.call('rbind', strsplit(as.character(summary_ga),'(',fixed=TRUE))
summary_ga <- cbind(summary_ga[seq(1, nrow(summary_ga), by = 2),],summary_ga[seq(2, nrow(summary_ga), by = 2),])
summary_ga <- data.frame(t(apply(summary_ga, 1, as.numeric)))
rownames(summary_ga) <- study_names
colnames(summary_ga) <- c("mean_0", "sd_0", "mean_1", "sd_1")
rm(summary_ga_temp)

#mothers'characteristics
#BMI
mean_bmi <- data.frame(matrix(unlist(ds.mean("E4$MATERNAL_BMI", type = 'split')), nrow = num_studies, ncol = 1, byrow=TRUE))
var_bmi <- data.frame(matrix(unlist(datashield.aggregate(opals, as.symbol("varDS(E4$MATERNAL_BMI)"))), nrow = num_studies, ncol = 1, byrow=TRUE))
summary_bmi <- cbind(mean_bmi,sqrt(var_bmi))
rownames(summary_bmi) <- study_names
colnames(summary_bmi) <- c("mean", "sd")
rm(mean_bmi, var_bmi)

#Age
mean_age <- data.frame(matrix(unlist(ds.mean("E4$MATERNAL_AGE", type = 'split')), nrow = num_studies, ncol = 1, byrow=TRUE))
var_age <- data.frame(matrix(unlist(datashield.aggregate(opals, as.symbol("varDS(E4$MATERNAL_AGE)"))), nrow = num_studies, ncol = 1, byrow=TRUE))
summary_age <- cbind(mean_age,sqrt(var_age))
rownames(summary_age) <- study_names
colnames(summary_age) <- c("mean", "sd")
rm(mean_age, var_age)

#parity
without <- opals
without[which(names(without) %in% c("GECKO","REPRO"))] <- NULL
studynames_without <- study_names[!study_names %in% c("GECKO", "REPRO")]
summary_parity_temp <- ds.summary('E4$PARITY', datasources=without)
summary_parity <- data.frame(matrix(unlist(summary_parity_temp), nrow = (num_studies - 2), ncol=10, byrow=TRUE))
rownames(summary_parity) <- studynames_without
colnames(summary_parity) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_parity <- summary_parity[,c(2,6,5,7)]
rm(summary_parity_temp)

#REPRO PARITY
summary_parity_REPRO <- ds.table1D(x = 'E4$PARITY',datasources = opals["REPRO"])
#GECKO
summary_parity_GECKO <- ds.table1D(x = 'E4$PARITY',datasources = opals["GECKO"])
remove(without)

# binaries
binary_var <- c('SMOKING', 'GDM')
binary_df <- data.frame()
for (bin in binary_var) {
  summary_temp <- ds.summary(paste0('E4$',bin))
  summary_temp <- data.frame(matrix(unlist(summary_temp), nrow = num_studies, ncol=6, byrow=TRUE))
  rownames(summary_temp) <- paste0(study_names,'_',bin)
  binary_df <- rbind(binary_df, summary_temp)
}
colnames(binary_df) <- c('type', 'n', '0', '1', 'No', 'Yes')
binary_df <- binary_df[,c(5,6)]
rm(summary_temp)

#maternal obesity
summary_ob_temp <- ds.summary('E4$MATERNAL_OB')
summary_ob <- data.frame(matrix(unlist(summary_ob_temp), nrow = num_studies, ncol=8, byrow=TRUE))
rownames(summary_ob) <- study_names
colnames(summary_ob) <- c("type", "N", "0", "1", "2", "Normal", "Overweight", "Obese")
summary_ob <- summary_ob[,c(6,7,8)]
rm(summary_ob_temp)

#ethnicity
# Note that for ethnicity REPRO ROLO SWS will not return correct results due to the fact that
# That these final studies do not fully follow the scheme (white, other, black)
# Study indicated them as binary, or in ROLO's case there are not enought people for a particular
# class hence datashield hides the result (which is a good thing)
summary_eth_temp <- ds.summary('E4$ETHNICITY')
for (i in 1:length(summary_eth_temp)){
  if (class(summary_eth_temp[[i]]) == 'character'){
    summary_eth_temp[[i]] <- list(class='factor', length=NA, categories1 =  NA,
                                  categories2 = NA, catergories3 = NA,
                                  count1 = NA, count2 = NA, count3 = NA)
  }
}
summary_eth <- data.frame(matrix(unlist(summary_eth_temp), nrow = num_studies, ncol=8, byrow=TRUE))
rownames(summary_eth) <- study_names
summary_eth <- summary_eth[,c(2,7,8,6)]
colnames(summary_eth) <- c("N", "black", "other", "white")

#education
summary_edu <- list()
#DNBC
summary_edu['DNBC'] <- ds.table1D(x = 'E4$MATERNAL_EDU',datasources = opals['DNBC'])
summary_edu['GECKO'] <- ds.table1D(x = 'E4$MATERNAL_EDU',datasources = opals['GECKO'])
summary_edu['HSS'] <- ds.table1D(x = 'E4$MATERNAL_EDU',datasources = opals['HSS'])
summary_edu['REPRO'] <- ds.table1D(x= 'E4$MATERNAL_EDU',datasource = opals['REPRO'])
summary_edu['SWS'] <- ds.table1D(x = 'E4$MATERNAL_EDU',datasources = opals['SWS'])

#alcohol
summary_alc <- list()
summary_alc['DNBC'] <- ds.summary(x = 'E4$ALCOHOL',datasources = opals['DNBC'])
summary_alc['GECKO'] <- ds.table1D(x = 'E4$ALCOHOL',datasources = opals['GECKO'])
summary_alc['HSS'] <- ds.table1D(x = 'E4$ALCOHOL',datasources = opals['HSS'])
summary_alc['REPRO'] <- ds.table1D(x = 'E4$ALCOHOL',datasources = opals['REPRO'])
summary_alc['SWS'] <- ds.summary(x = 'E4$ALCOHOL',datasources = opals['SWS'])

#smoking
summary_smoke <- list()
summary_smoke['DNBC'] <- ds.summary(x = 'E4$SMOKING',datasources = opals['DNBC'])
summary_smoke['GECKO'] <- ds.table1D(x = 'E4$SMOKING',datasources = opals['GECKO'])
summary_smoke['HSS'] <- ds.table1D(x = 'E4$SMOKING',datasources = opals['HSS'])
summary_smoke['REPRO'] <- ds.table1D(x = 'E4$SMOKING',datasources = opals['REPRO'])
summary_smoke['SWS'] <- ds.table1D(x = 'E4$SMOKING',datasources = opals['SWS'])



#---------------------------------------------------------
# Summaries for exposures

#LTPA
summary_ltpa_temp <- ds.summary('E4$LTPA_DUR_3_filt')
summary_ltpa <- data.frame(matrix(unlist(summary_ltpa_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_ltpa) <- study_names
colnames(summary_ltpa) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_ltpa <- summary_ltpa[,c(2,6,5,7)]
rm(summary_ltpa_temp)

# MOD_VIG_3_filt
summary_mod_temp <- ds.summary('E4$MOD_VIG_3_filt')
summary_mod <- data.frame(matrix(unlist(summary_mod_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_mod) <- study_names
colnames(summary_mod) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_mod <- summary_mod[,c(2,6,5,7)]
rm(summary_mod_temp)

# LTPA_EE_3_filt
summary_ee_temp <- ds.summary(x='E4$LTPA_EE_3_filt')
summary_ee <- data.frame(matrix(unlist(summary_ee_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_ee) <- study_names
colnames(summary_ee) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_ee <- summary_ee[,c(2,6,5,7)]
rm(summary_ee_temp)

# VIG_3_filt
summary_vig <- ds.summary('E4$VIG_3_filt')
summary_vig <- data.frame(matrix(unlist(summary_vig), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_vig) <- study_names
colnames(summary_vig) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_vig <- summary_vig[,c(2,6,5,7)]


#---------------------------------------------------------
# Summaries for outcomes

# Birthweight by sex
summary_bw_temp <- ds.meanByClass(x='E4$BIRTH_WEIGHT~E4$SEX', type = 'split')
summary_bw <- matrix(unlist(summary_bw_temp))
summary_bw <- substr(summary_bw, 1, nchar(summary_bw)-1)
summary_bw <- summary_bw[seq(2, nrow(summary_bw), by = 2),]
summary_bw <- do.call('rbind', strsplit(as.character(summary_bw),'(',fixed=TRUE))
summary_bw <- cbind(summary_bw[seq(1, nrow(summary_bw), by = 2),],summary_bw[seq(2, nrow(summary_bw), by = 2),])
summary_bw <- data.frame(t(apply(summary_bw, 1, as.numeric)))
rownames(summary_bw) <- study_names
colnames(summary_bw) <- c("mean_male", "sd_male", "mean_female", "sd_female")
rm(summary_bw_temp)

# Macrosomia
summary_mac_temp <- ds.summary('E4$MACROSOMIA')
summary_mac <- data.frame(matrix(unlist(summary_mac_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_mac) <- study_names
summary_mac <- summary_mac[,c(1,2,5,6)]
colnames(summary_mac) <- c("class", "length", "No", "Yes")
rm(summary_mac_temp)

# BIRTHWEIGHT_LGA
summary_lga_temp <- ds.summary('E4$BIRTH_WEIGHT_LGA')
summary_lga <- data.frame(matrix(unlist(summary_lga_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_lga) <- study_names
summary_lga <- summary_lga[,c(1,2,5,6)]
colnames(summary_lga) <- c("class", "length", "No", "Yes")
rm(summary_lga_temp)

# BIRTH_WEIGHT_SGA
summary_sga_temp <- ds.summary('E4$BIRTH_WEIGHT_SGA')
summary_sga <- data.frame(matrix(unlist(summary_sga_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_sga) <- study_names
summary_sga <- summary_sga[,c(1,2,5,6)]
colnames(summary_sga) <- c("class", "length", "No", "Yes")
rm(summary_sga_temp)
