## Work in progress seminar 2017
## Live Demo 
## Hopefully it wont be a repeat of Jobs' first iphone 4 demo

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
server <- c( 'ALSPAC','DNBC','HSS','REPRO', 'ROLO', 'SWS')
url <- c( 'https://opal-dev.mrc-epid.cam.ac.uk:8443','https://193.163.131.62:8443', 'https://interconnect.ucdenver.edu:8443',
          'https://212.51.193.40:8443', 'https://dougal.ucd.ie:8443', 'https://152.78.10.99:8443')
table <- c( 'ALSPAC_PAIP_01.ALSPAC_harm', 'DNBC_InterConnect.DNBC_harm','Interconnect.HSS_harm', 'a2.REPRO_harm_view', 'ROLO.ROLO_harm2', 'SWS.SWS_harm')
password <- c('datashield-test-privatekey.pem','datashield-test-privatekey.pem', 'datashield-test-privatekey.pem', 'datashield-test-privatekey.pem', 'datashield-test-privatekey.pem', 'datashield-test-privatekey.pem')
user <- c('datashield-test-publickey.pem','datashield-test-publickey.pem','datashield-test-publickey.pem', 'datashield-test-publickey.pem', 'datashield-test-publickey.pem', 'datashield-test-publickey.pem')
logindata_all <- data.frame(server,url,user,password, table)

datashield.logout(opals)
myvars = list('MOD_VIG_filt', 'LTPA_DUR_filt', 'LTPA_EE_filt','VIG_filt', 'BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA',
              'GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING','ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 
              'GDM', 'MATERNAL_BMI', 'MATERNAL_OB', 'PREECLAMPSIA', 'BIRTH_WEIGHT_SGA')
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables=myvars, directory='/home/shared/certificates/pa')

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# basic count of all the infants in the study per study
all_infants <- ds.length('D$SEX', type = 'split')
all_infants_combined <- ds.length('D$SEX', type='combined')

# remove preterm <37w
ds.subset(x = 'D', subset = 'D1', logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37)
no_preterm <- ds.length('D1$SEX', type = 'split')

# remove preeclampsia
ds.subset(x = 'D1', subset = 'D2', logicalOperator = 'PREECLAMPSIA==', threshold = 0)
no_preecl <- ds.length('D2$SEX', type = 'split')

# variables for model 1
# need to generate a 'temp' variable with no missings and add this in,
# because the ds.subset command needs at least 2 columns to work with (a ds bug)
for(i in 1:length(opals)){
  work1 <- no_preecl[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'temp', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('temp','D2'), newobj='D2a')

# for GECKO only dummy variable for LTPA_DUR_filt since this does not exist
# comment out if not using GECKO
#work1 <- no_preecl$GECKO
#work2 <- paste0("datashield.assign(opals[\"GECKO\"],'LTPA_DUR_filt', quote(rep(1,",work1,")))")
#eval(parse(text=work2))
#ds.cbind(x=c('temp','D2','LTPA_DUR_filt'), newobj='D2a', datasource=opals["GECKO"])

# Filter out missing values
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)


# Variables used within analysis
my_exp_all = c('MOD_VIG_filt', 'LTPA_DUR_filt', 'LTPA_EE_filt', 'VIG_filt')
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

#LTPA
summary_ltpa_temp <- ds.summary('E4$LTPA_DUR_filt')
summary_ltpa <- data.frame(matrix(unlist(summary_ltpa_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_ltpa) <- study_names
colnames(summary_ltpa) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_ltpa <- summary_ltpa[,c(2,6,5,7)]
rm(summary_ltpa_temp)



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

# model 1
# This runs regressions per outcome/exposure combination, per study with all covariates
# Then it runs random effects models per outcome/exposure combinations
my_exposure = c('MOD_VIG_filt', 'LTPA_DUR_filt', 'VIG_filt')
my_outcome = c( 'BIRTH_WEIGHT', 'MACROSOMIA','BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX')


REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

mypath <- file.path('~','plots','model_1.png')
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
      
      if (my_exposure[j] == 'LTPA_DUR_filt' & study_names[i] == 'GECKO'){
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


# model 2
# This runs regressions per outcome/exposure combination, per study with all covariates
# Then it runs random effects models per outcome/exposure combinations
my_exposure = c('MOD_VIG_filt', 'LTPA_DUR_filt', 'VIG_filt')
my_outcome = c( 'BIRTH_WEIGHT','MACROSOMIA','BIRTH_WEIGHT_LGA')
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
      
      if (my_exposure[j] == 'LTPA_DUR_filt' & study_names[i] == 'GECKO'){
        # don't do LTPA for GECKO, as the variable doesn't exist
      }
      else if(study_names[i]=='REPRO'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='DNBC'){
        #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't be inverted)
        fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
        reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)
      }
      else if(study_names[i]=='ROLO'){
        #omit parity, since it is 1 for all participants in ROLO (causes singular matrix that can't be inverted)
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
    for (n in 1:length(variables)){
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    }
  }
}

#Store results
dev.off()
model_2_all <- study_regs
model_2_REM <- REM_results







# MODEL 2 with incremental covariate addition
my_exp_2 = c('MOD_VIG_filt', 'LTPA_DUR_filt', 'VIG_filt')
my_outcome_2 = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_cov_2 = c( 'GESTATIONAL_AGE', 'SEX','MATERNAL_EDU', 'ETHNICITY', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
             'ALCOHOL')

model_2_ind = data.frame()

#for each opal server
for (o in 1:length(opals)){
  # For each exposure
  for (i in 1:length(my_exp_2)){
    # we skip LTPA for GECKO
    if (my_exp_2[i] == 'LTPA_DUR_filt' & study_names[o] == 'GECKO'){
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
        if (study_names[o]=='DNBC') {
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
