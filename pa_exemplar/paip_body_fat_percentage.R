# Analysis of Percentage body fat for babies against physical activity

## Datasets:
## HSS
## ROLO
## SWS

## Author: Paul Scherer
##         Tom Bishop
## Date: 30/08/2016

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

# source our login credentials this allocates values to variables:
# server
# url
# table
# password 
# user
# logindata_all

source("creds/paip_body_fat_percentage_creds.R")

datashield.logout(opals)
opals <- datashield.login(logins=logindata_all, assign=TRUE)

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# basic counts
all_infants <- ds.length('D$SEX', type = 'split')

#remove preterm <37w
ds.subset(x = 'D', subset = 'D1', logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37)
no_preterm <- ds.length('D1$SEX', type = 'split')

#remove preeclampsia
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


#-----------------------------------
# code for filtering out missings
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

my_exp_all = c('MOD_VIG', 'LTPA_DUR', 'LTPA_EE')
my_outcome_all = c('NEO_PER_BFAT')
my_cov_all = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
               'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM', 'MATERNAL_BMI', 'MATERNAL_OB')

#--- either the big loop or
my_vars_all <- c('temp', my_exp_all, my_outcome_all, my_cov_all)
model_all_len <- data.frame()

for (i in 2:length(my_vars_all)){
  ds.subset(x = 'D2a', subset = 'E3', cols =  c(my_vars_all[1:i]))
  ds.subset(x = 'E3', subset = 'E4', completeCases = TRUE)
  model_all_len <- rbind(model_all_len,ds.length('E4$temp', type = 'split'))
}

row.names(model_all_len) <- my_vars_all[2:length(my_vars_all)]


###############################################################################
########################### RUN MODELS  #######################################
###############################################################################

#--------------- FUNCTIONS TO HELP WITH REGRESSIONS AND REM ------------------#

#--------------- FUNCTIONS TO HELP WITH REGRESSIONS AND REM ------------------#

do_reg <- function(my_fmla, study, outcome, out_family){
  model <- ds.glm(formula= my_fmla, data = ref_table, family = out_family, datasources=opals[i], maxit = 100)
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
  
  #forest plots_body_fat_1
  
  if (out_family == 'gaussian') {
    usr <- par("usr")
    forest(res, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                  .(round(res$QEp,3)),')')),
           xlab=bquote(paste('Test of H'[0]*': true mean association = 0, p = ',
                             .(round(res$pval,3)))), ilab = cbind(weights.rma.uni(res)),ilab.xpos = c(usr[1]))
    text(usr[2], usr[4], "Beta [95% CI]", adj = c(1, 8))
    #text(usr[1], usr[4], gsub(paste0(ref_table,"\\$"),"", Reduce(paste, deparse(fmla))), adj = c( 0, 8 ))
    text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ))
    text(usr[1], usr[3], variable, adj = c( 0, 0 ))
  }
  else if (out_family == 'binomial'){
    usr <- par("usr")
    forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                            .(round(res$QEp,3)),')')),
           xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                             .(round(res$pval,3)))),ilab = cbind(weights.rma.uni(res)),ilab.xpos = c(usr[1]), atransf = exp)
    text(usr[2], usr[4], "Relative Risk [95% CI]", adj = c(1, 8))
    #text(usr[1], usr[4], gsub(paste0(ref_table,"\\$"),"", Reduce(paste, deparse(fmla))), adj = c( 0, 8 ))
    text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ))
    text(usr[1], usr[3], variable, adj = c( 0, 0))
  }
  
  return(res)
  
}


#  /'\_/`\            /\ \        /\_ \       /' \    
# /\      \    ___    \_\ \     __\//\ \     /\_, \   
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_/\ \  
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_     \ \ \ 
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\     \ \_\
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/      \/_/


#------------------FIRST MODEL BEGINS HERE----------------------------
#######################################################
# new model_1 code incremental addition of covariates etc.
my_exp_1 = c('MOD_VIG', 'LTPA_DUR', 'LTPA_EE')
my_outcome_1 = c('NEO_PER_BFAT')
my_cov_1 = c('GESTATIONAL_AGE', 'SEX')

model_1_ind = data.frame()

#for each opal server
for (o in 1:length(opals)){
  # For each exposure
  for (i in 1:length(my_exp_1)){
    # we skip LTPA for GECKO
    if (my_exp_1[i] == 'LTPA_DUR' & study_names[o] == 'GECKO'){
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
my_exposure = c('MOD_VIG', 'LTPA_DUR', 'LTPA_EE')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX')

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
      mypath <- file.path('~','plots_body_fat_1',paste('model_1_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results

model_1_all <- study_regs
model_1_REM <- REM_results



#  /'\_/`\            /\ \        /\_ \        /'___`\   
# /\      \    ___    \_\ \     __\//\ \      /\_\ /\ \  
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \     \/_/// /__ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_      // /_\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\    /\______/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/_____/ 


#------------------------
######### MODEL 2 starts here #######
# new model_2 code incremental addition of covariates etc.
my_exp_2 = c('MOD_VIG', 'LTPA_DUR')
my_outcome_2 = c('NEO_PER_BFAT')
my_cov_2 = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
             'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

model_2_ind = data.frame()

#for each opal server
for (o in 1:length(opals)){
  # For each exposure
  for (i in 1:length(my_exp_2)){
    # we skip LTPA for GECKO
    if (my_exp_2[i] == 'LTPA_DUR' & study_names[o] == 'GECKO'){
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

my_exposure = c('MOD_VIG', 'LTPA_DUR','LTPA_EE')
#my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA')
#my_outcome = c('MACROSOMIA')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

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
    
    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots_body_fat_1',paste('model_2_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}



#  /'\_/`\            /\ \        /\_ \       /'__`\   
# /\      \    ___    \_\ \     __\//\ \     /\_\L\ \  
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_/_\_<_ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_    /\ \L\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \ \____/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/___/ 



#------------------------
################## MODEL 3 starts here ########################
################## NEED TO FOLLOW GUIDE FOR MEDIATORS ##############
# 1. If there is no overall association between the exposure and outcome
#    then no need to run the following models
# 2. If there is an association, model exposure association with mediator
# 3. Then model mediator association with outcome
# 4. If 2 and 3 were significant then test model with both mediator and exposure
#   looking at their association with outcome
# 5. If the mediator is still significant after controlling for exposure, then
#    mediation is supported. If exposure is no longer significant while mediator is
#    still significant then full mediation is occuring.

#### Step 1 - is covered by model 2 (i.e. test for overall association between 
#### exposure and outcome)


####  Step 2 - Exposure association with mediator
# Slightly confusing as the mediator is now the outcome in the model

my_exposure = c('MOD_VIG', 'LTPA_DUR')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')


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
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in DNBC (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='REPRO'){
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
    
    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots_body_fat_1',paste('model_3_1_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results

model_3_1_all <- study_regs
model_3_1_REM <- REM_results

my_exposure = c('MATERNAL_BMI','GDM')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')


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
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in DNBC (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='REPRO'){
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
    
    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots_body_fat_1',paste('model_3_2_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results
model_3_2_all <- study_regs
model_3_2_REM <- REM_results

####  Step 3 - Mediator association with outcome
# Slightly confusing as the mediator is now the exposure in the model

my_exposure = 'MATERNAL_BMI'
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')


REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

# ####  Step 4 - Mediator association with outcome
# # Slightly confusing as the mediator is now the exposure in the model
#
my_exposure = c('LTPA_EE_3')
my_outcome = c('LTPA_EE_3')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY','MATERNAL_BMI')


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
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in DNBC (causes singular matrix that can't
        # be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='REPRO'){
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
    
    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots_body_fat_1',paste('model_3_3_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}
#
# #Store results

model_3_3_all <- study_regs
model_3_3_REM <- REM_results

####  Step 4 - Mediator association with outcome
# Slightly confusing as the mediator is now the exposure in the model

my_exposure = c('MOD_VIG', 'LTPA_DUR')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'MATERNAL_BMI')


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
    
    #meta analysis here
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    }   
  }
}

#Store results

model_3_4_all <- study_regs
model_3_4_REM <- REM_results


#  /'\_/`\            /\ \        /\_ \     /\ \\ \     
# /\      \    ___    \_\ \     __\//\ \    \ \ \\ \    
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \ \ \\ \_  
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_   \ \__ ,__\
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \/_/\_\_/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/      \/_/  



#------------------------
################## MODEL 4 starts here ########################
################## NEED TO FOLLOW GUIDE FOR MEDIATORS ##############
# 1. If there is no overall association between the exposure and outcome
#    then no need to run the following models
# 2. If there is an association, model exposure association with mediator
# 3. Then model mediator association with outcome
# 4. If 2 and 3 were significant then test model with both mediator and exposure
#   looking at their association with outcome
# 5. If the mediator is still significant after controlling for exposure, then
#    mediation is supported. If exposure is no longer significant while mediator is
#    still significant then full mediation is occuring.

#### Step 1 - is covered by model 2 (i.e. test for overall association between 
#### exposure and outcome)


####  Step 2 - Exposure association with mediator
# Slightly confusing as the mediator is now the outcome in the model
### Note that depending on model 3, the mediator may need to be added in!

my_exposure = c('MOD_VIG', 'LTPA_DUR')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY') #maybe also MATERNAL_BMI


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

    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots_body_fat_1',paste('model_4_2_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results

model_4_2_all <- study_regs
model_4_2_REM <- REM_results

####  Step 3 - Mediator association with outcome
# Slightly confusing as the mediator is now the exposure in the model

my_exposure = 'GDM'
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY') #maybe also MATERNAL_BMI


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
    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots_body_fat_1',paste('model_4_3_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results

model_4_3_all <- study_regs
model_4_3_REM <- REM_results

####  Step 4 - Mediator association with outcome
# Slightly confusing as the mediator is now the exposure in the model

my_exposure = c('MOD_VIG', 'LTPA_DUR')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM') #maybe also MATERNAL_BMI


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

    #meta analysis here
    for (n in 1:length(variables)){
      mypath <- file.path('~','plots_body_fat_1',paste('model_4_4_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results
model_4_4_all <- study_regs
model_4_4_REM <- REM_results



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


my_exposure = c('MOD_VIG', 'LTPA_DUR', 'LTPA_EE')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')
my_interaction = 'SEX'

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
      mypath <- file.path('~','plots_body_fat_1',paste('model_5_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
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



#------------------------
########## MODEL 6 (interaction maternal obesity) starts here #############
### This code allows you to look at the significance of the interaction term
### This is done per study (in the _all tables) and for a RMA
### We are looking for it to be p<0.01
### If the interaction term is significant then we will need some more code
### that stratifies the dataset by maternal obesity and 
### then investigates models 2, 3 and 4

my_exposure = c('MOD_VIG', 'LTPA_DUR', 'LTPA_EE')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')
my_interaction = 'MATERNAL_OB'

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
      mypath <- file.path('~','plots_body_fat_1',paste('model_6_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
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

my_exposure = c('MOD_VIG', 'LTPA_DUR', 'LTPA_EE')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU')
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
      mypath <- file.path('~','plots_body_fat_1',paste('model_7_',j,'_',k,'_',n, '.png',sep=''))
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

my_exposure = c('MOD_VIG', 'LTPA_DUR', 'LTPA_EE')
my_outcome = c('NEO_PER_BFAT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')
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
      mypath <- file.path('~','plots_body_fat_1',paste('model_8_',j,'_',k,'_',n, '.png',sep=''))
      png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
      dev.off()
    }
  }
}

#Store results
model_8_all <- study_regs
model_8_REM <- REM_results
