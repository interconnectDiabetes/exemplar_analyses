## Work in progress seminar 2017
## Live Demo 

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
#Login Details
setwd("~")
datashield.logout(opals) # kill off any previous sessions

server <- c('DNBC','GECKO', 'HSS', 'REPRO','SWS')
url <- c('https://193.163.131.62:8443','https://molgenis34.target.rug.nl:8443','https://interconnect.ucdenver.edu:8443', 'https://212.51.193.40:8443', 'https://152.78.10.99:8443')
table <- c('DNBC_InterConnect.DNBC_harm','gecko.gecko_harm', 'Interconnect.HSS_harm', 'a2.REPRO_harm_view','SWS.SWS_harm')
password <- c('datashield-test-privatekey.pem','datashield-test-privatekey.pem', 'datashield-test-privatekey.pem', 'datashield-test-privatekey.pem', 'datashield-test-privatekey.pem')
user <- c('datashield-test-publickey.pem','datashield-test-publickey.pem','datashield-test-publickey.pem', 'datashield-test-publickey.pem', 'datashield-test-publickey.pem')
logindata_all <- data.frame(server,url,user,password, table)

myvars = list('MOD_VIG_3_filt', 'LTPA_EE_3_filt', 'BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA',
              'GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING','ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 
              'GDM', 'MATERNAL_BMI', 'MATERNAL_OB', 'PREECLAMPSIA', 'BIRTH_WEIGHT_SGA')
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

# some nice descriptives for later processing
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

# Variables used within analysis
my_exp_all = c('MOD_VIG_3_filt', 'LTPA_EE_3_filt')
my_outcome_all = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA', 'BIRTH_WEIGHT_SGA')
my_cov_all = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
               'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM', 'MATERNAL_BMI', 'MATERNAL_OB')

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

# MOD_VIG_3_filt
summary_mod_temp <- ds.summary('E4$MOD_VIG_3_filt')
summary_mod <- data.frame(matrix(unlist(summary_mod_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_mod) <- study_names
colnames(summary_mod) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_mod <- summary_mod[,c(2,6,5,7)]
rm(summary_mod_temp)



###############################################################################
########################### RUN MODELS  #######################################
###############################################################################
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

my_exposure = c('MOD_VIG_3_filt')
my_outcome = c( 'BIRTH_WEIGHT')
my_covariate = c('GESTATIONAL_AGE', 'SEX')


REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

fmla <- as.formula('E4$BIRTH_WEIGHT~E4$MOD_VIG_3_filt+E4$GESTATIONAL_AGE+E4$SEX')

for (k in 1:length(my_outcome)){
  for (j in 1:length(my_exposure)){
    estimates <- vector()
    s_errors <- vector()
    labels <- vector()
    for (i in 1: length(opals)) {
      reg_data <- data.frame()
      reg_data <- do_reg(fmla, names(opals[i]), "BIRTH_WEIGHT", "gaussian")
      
      study_regs = rbind(study_regs,reg_data)
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])      
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
    }
    
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = "gaussian", variable = variables[n])
    }
  }
}

#  /'\_/`\            /\ \        /\_ \        /'___`\   
# /\      \    ___    \_\ \     __\//\ \      /\_\ /\ \  
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \     \/_/// /__ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_      // /_\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\    /\______/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/_____/ 

my_exposure = c('MOD_VIG_3_filt')
my_outcome = c( 'BIRTH_WEIGHT')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')
REM_results = list()
study_regs = data.frame()
ref_table = 'E4'
outcome_family = "gaussian"

fmla <- as.formula('E4$BIRTH_WEIGHT~E4$MOD_VIG_3_filt+E4$GESTATIONAL_AGE+E4$SEX+E4$PARITY+E4$MATERNAL_AGE+E4$SMOKING+E4$ALCOHOL+E4$MATERNAL_EDU+E4$ETHNICITY')

for (k in 1:length(my_outcome)){
  for (j in 1:length(my_exposure)){
    estimates <- vector()
    s_errors <- vector()
    labels <- vector()
    for (i in 1: length(opals)) {
      reg_data <- data.frame()
      if(study_names[i]=='REPRO'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      study_regs = rbind(study_regs,reg_data)
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])      
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
    }
    
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = "gaussian", variable = variables[n])
    }
  }
}

#  /'\_/`\            /\ \        /\_ \       /'__`\   
# /\      \    ___    \_\ \     __\//\ \     /\_\L\ \  
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_/_\_<_ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_    /\ \L\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \ \____/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/___/ FIXER

my_exposure = c('MOD_VIG_3_filt')
my_outcome = c( 'BIRTH_WEIGHT_SGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'
outcome_family = "binomial"

fmla <- as.formula('E4$BIRTH_WEIGHT_SGA~E4$MOD_VIG_3_filt+E4$GESTATIONAL_AGE+E4$SEX+E4$PARITY+E4$MATERNAL_AGE+E4$SMOKING+E4$ALCOHOL+E4$MATERNAL_EDU+E4$ETHNICITY')

for (k in 1:length(my_outcome)){
  for (j in 1:length(my_exposure)){
    estimates <- vector()
    s_errors <- vector()
    labels <- vector()
    for (i in 1: length(opals)) {
      reg_data <- data.frame()
      if(study_names[i]=='REPRO'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else {
        fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      
      study_regs = rbind(study_regs,reg_data)
      estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
      s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
      labels = rbind(labels, reg_data[2,1])      
      variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
    }
    
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = "gaussian", variable = variables[n])
    }
  }
}
