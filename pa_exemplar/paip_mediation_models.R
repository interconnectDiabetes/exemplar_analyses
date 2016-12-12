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

my_exposure = c('LTPA_DUR_3_filt')
my_outcome = 'MATERNAL_BMI'
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')


REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

mypath <- file.path('~','plots','model_3a.png')
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
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR_3_filt' & study_names[i] == 'GECKO'){
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
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    } 
  }
}
#Store results
dev.off()
model_3_2_all <- study_regs
model_3_2_REM <- REM_results


####  Step 3 - Mediator association with outcome
# Slightly confusing as the mediator is now the exposure in the model

my_exposure = 'MATERNAL_BMI'
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

mypath <- file.path('~','plots','model_3b.png')
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
      
      if (my_exposure[j] == 'LTPA_DUR_3_filt' & study_names[i] == 'GECKO'){
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
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    }
  }
}

#Store results
dev.off()
model_3_3_all <- study_regs
model_3_3_REM <- REM_results

####  Step 4 - Mediator association with outcome
# Slightly confusing as the mediator is now the exposure in the model

my_exposure = c('MOD_VIG_3_filt', 'LTPA_DUR_3_filt')
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'MATERNAL_BMI')

mypath <- file.path('~','plots','model_3c.png')
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
      
      if (my_exposure[j] == 'LTPA_DUR_3_filt' & study_names[i] == 'GECKO'){
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
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    }   
  }
}

#Store results
dev.off()
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

my_exposure = c('MOD_VIG_3_filt', 'LTPA_DUR_3_filt')
my_outcome = 'GDM'
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY') #maybe also MATERNAL_BMI

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

mypath <- file.path('~','plots','model_4_2.png')
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
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR_3_filt' & study_names[i] == 'GECKO'){
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
      # mypath <- file.path('~','plots',paste('model_3_',j,'_',k,'_',n, '.png',sep=''))
      # png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    } 
  }
}
#Store results
dev.off()
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

mypath <- file.path('~','plots','model_4_3.png')
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
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR_3_filt' & study_names[i] == 'GECKO'){
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
      # mypath <- file.path('~','plots',paste('model_4_3_',j,'_',k,'_',n, '.png',sep=''))
      # png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    } 
  }
}
#Store results
dev.off()
model_4_3_all <- study_regs
model_4_3_REM <- REM_results

####  Step 4 - Mediator association with outcome
# Slightly confusing as the mediator is now the exposure in the model

my_exposure = c('MOD_VIG_3_filt', 'LTPA_DUR_3_filt')
my_outcome = c('BIRTH_WEIGHT', 'MACROSOMIA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM') #maybe also MATERNAL_BMI

REM_results = list()
study_regs = data.frame()
ref_table = 'E4'

mypath <- file.path('~','plots','model_4_4.png')
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
    for(i in 1:length(opals)) {
      reg_data <- data.frame()
      
      if (my_exposure[j] == 'LTPA_DUR_3_filt' & study_names[i] == 'GECKO'){
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
      # mypath <- file.path('~','plots',paste('model_4_4_',j,'_',k,'_',n, '.png',sep=''))
      # png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    } 
  }
}
#Store results
dev.off()
model_4_4_all <- study_regs
model_4_4_REM <- REM_results