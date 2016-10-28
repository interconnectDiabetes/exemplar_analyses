## Analysis script for third trimester PAIP analysis
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
#setwd("/home/l_pms69/exemplar_analyses/")
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
opals <- datashield.login(logins=logindata_all, assign=TRUE)

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

#for GECKO only dummy variable for LTPA_DUR_3 since this does not exist
work1 <- no_preecl$GECKO
work2 <- paste0("datashield.assign(opals[\"GECKO\"],'LTPA_DUR_3', quote(rep(1,",work1,")))")
eval(parse(text=work2))
ds.cbind(x=c('temp','D2','LTPA_DUR_3'), newobj='D2a', datasource=opals["GECKO"])

# Filter out missing values
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

# Variables used within analysis
my_exp_all = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome_all = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
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
#GECKO
summary_edu['GECKO'] <- ds.table1D(x = 'E4$MATERNAL_EDU',datasources = opals['GECKO'])
#HSS
summary_edu['HSS'] <- ds.table1D(x = 'E4$MATERNAL_EDU',datasources = opals['HSS'])
#REPRO
summary_edu['REPRO'] <- ds.table1D(x= 'E4$MATERNAL_EDU',datasource = opals['REPRO'])
#SWS
summary_edu['SWS'] <- ds.table1D(x = 'E4$MATERNAL_EDU',datasources = opals['SWS'])

#alcohol
summary_alc <- list()
#DNBC
summary_alc['DNBC'] <- ds.summary(x = 'E4$ALCOHOL',datasources = opals['DNBC'])
#GECKO
summary_alc['GECKO'] <- ds.table1D(x = 'E4$ALCOHOL',datasources = opals['GECKO'])
#HSS
summary_alc['HSS'] <- ds.table1D(x = 'E4$ALCOHOL',datasources = opals['HSS'])
#REPRO
summary_alc['REPRO'] <- ds.table1D(x = 'E4$ALCOHOL',datasources = opals['REPRO'])
#SWS
summary_alc['SWS'] <- ds.summary(x = 'E4$ALCOHOL',datasources = opals['SWS'])


#---------------------------------------------------------
# Summaries for exposures

#LTPA
summary_ltpa_temp <- ds.summary('E4$LTPA_DUR_3')
summary_ltpa <- data.frame(matrix(unlist(summary_ltpa_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_ltpa) <- study_names
colnames(summary_ltpa) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_ltpa <- summary_ltpa[,c(2,6,5,7)]
rm(summary_ltpa_temp)

# MOD_VIG_3
summary_mod_temp <- ds.summary('E4$MOD_VIG_3')
summary_mod <- data.frame(matrix(unlist(summary_mod_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_mod) <- study_names
colnames(summary_mod) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_mod <- summary_mod[,c(2,6,5,7)]
rm(summary_mod_temp)

# LTPA_EE_3
summary_ee_temp <- ds.summary(x='E4$LTPA_EE_3')
summary_ee <- data.frame(matrix(unlist(summary_ee_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_ee) <- study_names
colnames(summary_ee) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_ee <- summary_ee[,c(2,6,5,7)]
rm(summary_ee_temp)

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


###############################################################################
########################### RUN MODELS  #######################################
###############################################################################

#--------------- FUNCTIONS TO HELP WITH REGRESSIONS AND REM ------------------#

do_reg <- function(my_fmla, study, outcome, out_family){
  
  model <- ds.glm(formula= my_fmla, data = ref_table, family = out_family, datasources=opals[i], maxit = 20)
  model_coeffs <- as.data.frame(model$coefficients)
  model_coeffs$study = study
  model_coeffs$outcome = outcome
  model_coeffs$cov = rownames(model_coeffs)
  for (x in 1:3){ model_coeffs <- model_coeffs[,c(ncol(model_coeffs),1:(ncol(model_coeffs)-1))]}
  rownames(model_coeffs) = NULL
  return(model_coeffs)
}

do_REM <- function(coeffs, s_err, labels, fmla, out_family, variable){
  
  res <- rma(yi = coeffs, sei = s_err, method='DL', slab = labels)
  
  #add the weights to the labels
  res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")
  
  #forest plots
  
  if (out_family == 'gaussian') {
    
    forest(res, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                  .(round(res$QEp,3)),')')),
           xlab=bquote(paste('Test of H'[0]*': true mean association = 0, p = ',
                             .(round(res$pval,3)))))
    usr <- par("usr")
    text(usr[2], usr[4], "Beta [95% CI]", adj = c(1, 4),cex=0.75)
    text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=0.75)
    text(usr[1], usr[3], variable, adj = c( 0, 0 ),cex=0.75)
    
  }
  else if (out_family == 'binomial'){
    
    forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                            .(round(res$QEp,3)),')')),
           xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                             .(round(res$pval,3)))), atransf = exp)
    usr <- par("usr")
    text(usr[2], usr[4], "Relative Risk [95% CI]", adj = c(1, 4),cex=0.75)
    text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=0.75)
    text(usr[1], usr[3], variable, adj = c( 0, 0),cex=0.75)
  }
  
  return(res)
  
}



#  /'\_/`\            /\ \        /\_ \       /' \    
# /\      \    ___    \_\ \     __\//\ \     /\_, \   
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_/\ \  
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_     \ \ \ 
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\     \ \_\
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/      \/_/

# MODEL 1 with incremental covariate addition
my_exp_1 = c('MOD_VIG_3', 'LTPA_DUR_3')
my_outcome_1 = c('BIRTH_WEIGHT_LGA')
my_cov_1 = c('GESTATIONAL_AGE', 'SEX')

model_1_ind = data.frame()

#for each opal server
for (o in 1:length(opals)){
  # For each exposure
  for (i in 1:length(my_exp_1)){
    # we skip LTPA for GECKO
    if (my_exp_1[i] == 'LTPA_DUR_3' & study_names[o] == 'GECKO'){
      next
    }
    # look at each possible outcome
    for (j in 1:length(my_outcome_1)){
      # perform a check on the type of outcome which dictates the data model we assume for
      # linear regression
      if (ds.class(paste0('E4$',my_outcome_1[j]), datasources=opals[o]) == 'factor'){
        dataModel <- 'binomial'
      } else {
        dataModel <- 'gaussian'
      }
      my_cov_1_buildup = vector('character')
      # of which we incrementally add mediators/covariates/modifiers
      for (k in 0:length(my_cov_1)){
        model_1 <- data.frame()

        # create the formula
        # start with simply exposure and create incrementally more complicated
        # formulas
        if (length(my_cov_1_buildup)==0){
          fmla_left <- paste(paste('E4$', my_outcome_1[j], " ~ ", sep=""))
          fmla_right <- paste0('E4$', my_exp_1[i])
          fmla <- paste(fmla_left, fmla_right, sep="")
          fmla <- as.formula(fmla)
        } else {
          fmla_left <- paste(paste('E4$', my_outcome_1[j], " ~ ", sep=""))
          fmla_right_exp <- paste0('E4$', my_exp_1[i])
          fmla_right_cov <- paste0('E4$', my_cov_1_buildup, collapse="+")
          fmla_right <- paste(fmla_right_exp, fmla_right_cov, sep="+")
          fmla <- paste(fmla_left, fmla_right, sep="")
          fmla <- as.formula(fmla)
        }

        # create the model
        model <- ds.glm(formula=fmla, data='E4', family=dataModel, datasources=opals[o])
        model_coeffs <- model$coefficients
        rownames(model_coeffs) <- paste(rownames(model_coeffs), names(opals[o]), sep="_")
        model_1 <- rbind(model_1, model_coeffs)

        # Write in the data into table
        model_1$desc <- paste(my_outcome_1[j] ,paste(my_exp_1[i], paste0(my_cov_1_buildup,collapse="+"), sep="+"), sep="~")
        model_1 <- model_1[,c(ncol(model_1),1:(ncol(model_1)-1))]
        # binomial and gaussian regression have different outputs names which can confuse R's dataframes
        if (dataModel == 'binomial'){
          model_1 <- model_1[-c(8:10)]
          names(model_1)[names(model_1) == 'low0.95CI.LP' ] <- 'low0.95CI'
          names(model_1)[names(model_1) == 'high0.95CI.LP' ] <- 'high0.95CI'
          model_1_ind <- rbind(model_1_ind,model_1)
        } else {
          model_1_ind <- rbind(model_1_ind,model_1)
        }

        # to make sure that my_cov_1_buildup doesnt cause errors
        if (k == length(my_cov_1)){
        } else {
          my_cov_1_buildup <- c(my_cov_1_buildup, my_cov_1[k+1])
        }
      }
    }
  }
}


# model 1
# This runs regressions per outcome/exposure combination, per study with all covariates
# Then it runs random effects models per outcome/exposure combinations
my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome = c( 'BIRTH_WEIGHT','MACROSOMIA','BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX')

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

mypath <- file.path('~','plots','model_1_pi.png')
png(file=mypath, width = 1260*length(my_exposure), height = 940*length(my_outcome), res = 300)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))

for (k in 1:length(my_outcome)){
  
  #!!! Need to check whether there are other outcomes we need to handle !!! 
  out_class = ds.class(paste0(ref_table, '$', my_outcome[k]))[[1]]
  if (out_class == 'factor') {
    outcome_family = 'binomial'
  }
  else if (out_class == 'numeric' | out_class == 'integer') {
    outcome_family = 'gaussian'
  }
  
  for (j in 1:length(my_exposure)){
    estimates = vector()
    s_errors = vector()
    labels = vector()
    #Sys.sleep(300)
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR' & study_names[i] == 'GECKO'){
        # don't do LTPA for GECKO, as the variable doesn't exist
      }
      else if(study_names[i]=='ROLO' & my_exposure[j] == 'PARITY'){
        #omit parity, since it is 1 for all participants in ROLO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'PARITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      if (outcome_family == 'binomial' & length(reg_data) > 0){
        reg_data = reg_data[1:9]
        colnames(reg_data)[8] <- "low0.95CI"
        colnames(reg_data)[9] <- "high0.95CI"
        
      }
      study_regs = rbind(study_regs,reg_data)
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])      
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
      
    }
    
    #meta analysis here
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    }
  }
}

#Store results
dev.off()
model_1_all <- study_regs
model_1_REM <- REM_results



#  /'\_/`\            /\ \        /\_ \        /'___`\   
# /\      \    ___    \_\ \     __\//\ \      /\_\ /\ \  
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \     \/_/// /__ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_      // /_\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\    /\______/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/_____/ 


# MODEL 2 with incremental covariate addition
my_exp_2 = c('MOD_VIG_3', 'LTPA_DUR_3')
my_outcome_2 = c('BIRHT_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_cov_2 = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
             'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

model_2_ind = data.frame()

#for each opal server
for (o in 1:length(opals)){
  # For each exposure
  for (i in 1:length(my_exp_2)){
    # we skip LTPA for GECKO
    if (my_exp_2[i] == 'LTPA_DUR_3' & study_names[o] == 'GECKO'){
      next
    }
    if (study_names[o] == 'REPRO') {
      next
    }
    # look at each possible outcome
    for (j in 1:length(my_outcome_2)){
      if (ds.class(paste0('E4$',my_outcome_2[j]), datasources=opals[o]) == 'factor'){
        dataModel <- 'binomial'
      } else {
        dataModel <- 'gaussian'
      }
      my_cov_2_buildup = vector('character')
      # of which we incrementally add mediators/covariates/modifiers
      for (k in 0:length(my_cov_2)){
        model_2 <- data.frame()

        # create the formula
        # EXCEPTIONS BY STUDY
        if (study_names[o]=='ROLO') {
          my_cov_2_buildup <- my_cov_2_buildup[my_cov_2_buildup != 'PARITY']
        }
        if (study_names[o]=='REPRO') {
          my_cov_2_buildup <- my_cov_2_buildup[my_cov_2_buildup != 'ETHNICITY']
        }
        # start with simply exposure and create incrementally more complicated
        # formulas
        if (length(my_cov_2_buildup)==0){
          fmla_left <- paste(paste('E4$', my_outcome_2[j], " ~ ", sep=""))
          fmla_right <- paste0('E4$', my_exp_2[i])
          fmla <- paste(fmla_left, fmla_right, sep="")
          fmla <- as.formula(fmla)
        } else {
          fmla_left <- paste(paste('E4$', my_outcome_2[j], " ~ ", sep=""))
          fmla_right_exp <- paste0('E4$', my_exp_2[i])
          fmla_right_cov <- paste0('E4$', my_cov_2_buildup, collapse="+")
          fmla_right <- paste(fmla_right_exp, fmla_right_cov, sep="+")
          fmla <- paste(fmla_left, fmla_right, sep="")
          fmla <- as.formula(fmla)
        }

        # create the model
        model <- ds.glm(formula=fmla, data='E4', family=dataModel, datasources=opals[o])
        model_coeffs <- model$coefficients
        rownames(model_coeffs) <- paste(rownames(model_coeffs), names(opals[o]), sep="_")
        model_2 <- rbind(model_2, model_coeffs)

        # Write in the data into table
        model_2$desc <- paste(my_outcome_2[j] ,paste(my_exp_2[i], paste0(my_cov_2_buildup,collapse="+"), sep="+"), sep="~")
        model_2 <- model_2[,c(ncol(model_2),1:(ncol(model_2)-1))]
        # binomial and gaussian regression have different outputs names which can confuse R's dataframes
        if (dataModel == 'binomial'){
          model_2 <- model_2[-c(8:10)]
          names(model_2)[names(model_2) == 'low0.95CI.LP' ] <- 'low0.95CI'
          names(model_2)[names(model_2) == 'high0.95CI.LP' ] <- 'high0.95CI'
          model_2_ind <- rbind(model_2_ind,model_2)
        } else {
          model_2_ind <- rbind(model_2_ind,model_2)
        }

        # to make sure that my_cov_2_buildup doesnt cause errors
        if (k == length(my_cov_2)){
        } else {
          my_cov_2_buildup <- c(my_cov_2_buildup, my_cov_2[k+1])
        }
      }
    }
  }
}

# model 2
# This runs regressions per outcome/exposure combination, per study with all covariates
# Then it runs random effects models per outcome/exposure combinations
my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3','LTPA_EE_3')
#my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA')
#my_outcome = c('MACROSOMIA')
my_outcome = c('BIRTH_WEIGHT','MACROSOMIA','BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

mypath <- file.path('~','plots','model_2.png')
png(file=mypath, width = 1260*length(my_exposure), height = 940*length(my_outcome), res = 300)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

for (k in 1:length(my_outcome)){
  
  #!!! Need to check whether there are other outcomes we need to handle !!! 
  out_class = ds.class(paste0(ref_table, '$', my_outcome[k]))[[1]]
  if (out_class == 'factor') {
    outcome_family = 'binomial'
  }
  else if (out_class == 'numeric' | out_class == 'integer') {
    outcome_family = 'gaussian'
  }
  
  for (j in 1:length(my_exposure)){
    estimates = vector()
    s_errors = vector()
    labels = vector()
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR' & study_names[i] == 'GECKO'){
        # don't do LTPA for GECKO, as the variable doesn't exist
      }
      else if(study_names[i]=='REPRO'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='ROLO'){
        #omit parity, since it is 1 for all participants in ROLO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'PARITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      if (outcome_family == 'binomial' & length(reg_data) > 0){
        reg_data = reg_data[1:9]
        colnames(reg_data)[8] <- "low0.95CI"
        colnames(reg_data)[9] <- "high0.95CI"
      }
      study_regs = rbind(study_regs,reg_data)
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])      
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
      
    }
    #Sys.sleep(300)
    #meta analysis here
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    }
  }
}

#Store results
dev.off()
model_2_all <- study_regs
model_2_REM <- REM_results


#                      __          ___       ______
#  /'\_/`\            /\ \        /\_ \     /\  ___\
# /\      \    ___    \_\ \     __\//\ \    \ \ \__/
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \ \___``\
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_   \/\ \L\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \ \____/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/___/


#------------------------
######## MODEL 5 starts here ########
### This code allows you to look at the significance of the interaction term
### This is done per study (in the _all tables) and for a RMA
### We are looking for it to be p<0.01
### If the interaction term is significant then we will need some more code
### that stratifies the dataset by sex and then investigates models 2, 3 and 4


my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')
my_interaction = 'SEX'

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

for (k in 1:length(my_outcome)){
  #set up plots here because there are plots for interaction terms too
  #sadly have to hard code the levels of the interation term
  mypath <- file.path('~','plots',paste('model_5_', k, '.png',sep=''))
  png(file=mypath, width = 1260*2, height = 940*length(my_exposure), res = 300)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_exposure),2))

  #!!! Need to check whether there are other outcomes we need to handle !!!
  out_class = ds.class(paste0(ref_table, '$', my_outcome[k]))[[1]]
  if (out_class == 'factor') {
    outcome_family = 'binomial'
  }
  else if (out_class == 'numeric' | out_class == 'integer') {
    outcome_family = 'gaussian'
  }
  
  for (j in 1:length(my_exposure)){
    estimates = vector()
    s_errors = vector()
    labels = vector()
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR_3' & study_names[i] == 'GECKO'){
        # don't do LTPA for GECKO, as the variable doesn't exist
      }
      else if(study_names[i]=='REPRO'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='ROLO'){
        #omit parity, since it is 1 for all participants in ROLO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'PARITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      if (outcome_family == 'binomial' & length(reg_data) > 0){
        reg_data = reg_data[1:9]
        colnames(reg_data)[8] <- "low0.95CI"
        colnames(reg_data)[9] <- "high0.95CI"
        
      }
      study_regs = rbind(study_regs,reg_data)
      
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov, ),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
      
    }
    
    #meta analysis here
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    }
  }
  dev.off()
}

#Store results
model_5_all <- study_regs
model_5_REM <- REM_results



#  /'\_/`\            /\ \        /\_ \       /'___\
# /\      \    ___    \_\ \     __\//\ \     /\ \__/
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \ \  _``\
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_   \ \ \L\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \ \____/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/___/

########## MODEL 6 (interaction maternal obesity) starts here #############
### This code allows you to look at the significance of the interaction term
### This is done per study (in the _all tables) and for a RMA
### We are looking for it to be p<0.01
### If the interaction term is significant then we will need some more code
### that stratifies the dataset by maternal obesity and
### then investigates models 2, 3 and 4

my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'MATERNAL_OB')
my_interaction = 'MATERNAL_OB'

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

for (k in 1:length(my_outcome)){
  # Settings for graph image
  mypath <- file.path('~','plots',paste('model_6_', k, '.png',sep=''))
  png(file=mypath, width = 1260*3, height = 940*length(my_exposure), res = 300)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))

  #!!! Need to check whether there are other outcomes we need to handle !!!
  out_class = ds.class(paste0(ref_table, '$', my_outcome[k]))[[1]]
  if (out_class == 'factor') {
    outcome_family = 'binomial'
  }
  else if (out_class == 'numeric' | out_class == 'integer') {
    outcome_family = 'gaussian'
  }
  
  for (j in 1:length(my_exposure)){
    estimates = vector()
    s_errors = vector()
    labels = vector()
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR_3' & study_names[i] == 'GECKO'){
        # don't do LTPA for GECKO, as the variable doesn't exist
      }
      else if(study_names[i]=='REPRO'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='ROLO'){
        #omit parity, since it is 1 for all participants in ROLO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'PARITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      if (outcome_family == 'binomial' & length(reg_data) > 0){
        reg_data = reg_data[1:9]
        colnames(reg_data)[8] <- "low0.95CI"
        colnames(reg_data)[9] <- "high0.95CI"
        
      }
      study_regs = rbind(study_regs,reg_data)
      
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov, ),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
      
    }
    
    #meta analysis here
    for (n in 1:length(variables)){
      # mypath <- file.path('~','plots',paste('model_6_',j,'_',k,'_',n, '.png',sep=''))
      # png(file=mypath, width = 1260, height = 940)
      # REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      # dev.off()
    }
  }
  dev.off()
}

#Store results
model_6_all <- study_regs
model_6_REM <- REM_results



#                      __          ___          ________
#  /'\_/`\            /\ \        /\_ \        /\_____  \
# /\      \    ___    \_\ \     __\//\ \       \/___//'/'
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \          /' /'
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_      /' /'
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\    /\_/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \//



#------------------------
######### MODEL 7 (interaction ethnicity) starts here ######
### This code allows you to look at the significance of the interaction term
### This is done per study (in the _all tables) and for a RMA
### We are looking for it to be p<0.01
### If the interaction term is significant then we will need some more code
### that stratifies the dataset by ethnicity and then investigates models 2, 3 and 4

my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')
my_interaction = 'ETHNICITY'

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

for (k in 1:length(my_outcome)){
  #!!! Need to check whether there are other outcomes we need to handle !!!
  out_class = ds.class(paste0(ref_table, '$', my_outcome[k]))[[1]]
  if (out_class == 'factor') {
    outcome_family = 'binomial'
  }
  else if (out_class == 'numeric' | out_class == 'integer') {
    outcome_family = 'gaussian'
  }
  
  for (j in 1:length(my_exposure)){
    estimates = vector()
    s_errors = vector()
    labels = vector()
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (study_names[i] == 'GECKO'){
        # omit regression since Gecko has 0 black, 14 other, << 14 white and this causes problems in comparisons
      }
      else if(study_names[i]=='DNBC'){
        #omit regression since interaction term contains ethnicity,
        # which is 1 for all participants in REPRO (causes singular matrix
        # that can't be inverted)
        #fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        #reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='REPRO'){
        #omit regression since interaction term contains ethnicity,
        # which is 1 for all participants in REPRO (causes singular matrix
        # that can't be inverted)
        #fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        #reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='ROLO'){
        #omit from regression as problem interaction term
        # some ethnicity classes have <5 participants?
        #fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'PARITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        #reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      if (outcome_family == 'binomial' & length(reg_data) > 0){
        reg_data = reg_data[1:9]
        colnames(reg_data)[8] <- "low0.95CI"
        colnames(reg_data)[9] <- "high0.95CI"
      }
      if (length(reg_data) > 0){
        study_regs = rbind(study_regs,reg_data)
        estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
        s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
        labels = rbind(labels, reg_data[2,1])
        variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
      }
    }
    
    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots',paste('model_7_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results
model_7_all <- study_regs
model_7_REM <- REM_results




#  /'\_/`\            /\ \        /\_ \       /'_ `\
# /\      \    ___    \_\ \     __\//\ \     /\ \L\ \
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_> _ <_
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_    /\ \L\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \ \____/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/___/



#------------------------
######### MODEL 8 (interaction GDM) starts here ############
### This code allows you to look at the significance of the interaction term
### This is done per study (in the _all tables) and for a RMA
### We are looking for it to be p<0.01
### If the interaction term is significant then we will need some more code
### that stratifies the dataset by GDM and then investigates models 2, 3 and 4

my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM')
my_interaction = 'GDM'

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

for (k in 1:length(my_outcome)){
  #!!! Need to check whether there are other outcomes we need to handle !!!
  out_class = ds.class(paste0(ref_table, '$', my_outcome[k]))[[1]]
  if (out_class == 'factor') {
    outcome_family = 'binomial'
  }
  else if (out_class == 'numeric' | out_class == 'integer') {
    outcome_family = 'gaussian'
  }
  
  for (j in 1:length(my_exposure)){
    estimates = vector()
    s_errors = vector()
    labels = vector()
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR_3' & study_names[i] == 'GECKO'){
        # don't do LTPA for GECKO, as the variable doesn't exist
      }
      else if(study_names[i]=='REPRO'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='ROLO'){
        #omit parity, since it is 1 for all participants in ROLO (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'PARITY'])), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+"),"+", ref_table,"$",my_interaction,"*", ref_table,"$",my_exposure[j]))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      if (outcome_family == 'binomial' & length(reg_data) > 0){
        reg_data = reg_data[1:9]
        colnames(reg_data)[8] <- "low0.95CI"
        colnames(reg_data)[9] <- "high0.95CI"
        
      }
      study_regs = rbind(study_regs,reg_data)
      
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov, ),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
      
    }
    
    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots',paste('model_8_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results
model_8_all <- study_regs
model_8_REM <- REM_results
