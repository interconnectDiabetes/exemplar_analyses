
#  ___  ___          _      _    ____                _             _        
#  |  \/  |         | |    | |  / ___|              | |           | |       
#  | .  . | ___   __| | ___| | / /___   ______   ___| |_ _ __ __ _| |_ __ _ 
#  | |\/| |/ _ \ / _` |/ _ \ | | ___ \ |______| / __| __| '__/ _` | __/ _` |
#  | |  | | (_) | (_| |  __/ | | \_/ |          \__ \ |_| | | (_| | || (_| |
#  \_|  |_/\___/ \__,_|\___|_| \_____/          |___/\__|_|  \__,_|\__\__,_|
#                                                                           
#                                 
# stratified analysis. Needs to follow the structure of model 2.

#stratify by maternal obesity
ds.subset(x = 'E4', subset = 'E4_6_0', logicalOperator = 'MATERNAL_OB==', threshold = 0)
ds.subset(x = 'E4', subset = 'E4_6_1', logicalOperator = 'MATERNAL_OB==', threshold = 1)
ds.subset(x = 'E4', subset = 'E4_6_2', logicalOperator = 'MATERNAL_OB==', threshold = 2)

my_exposure = c('MOD_VIG_filt', 'LTPA_DUR_filt', 'LTPA_EE_filt')
my_outcome = c('MACROSOMIA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')


# first stratum - 0
REM_results = list()
study_regs = data.frame()
ref_table = 'E4_6_0'

mypath <- file.path('~','plots','model_6_0.png')
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
      
      if (my_exposure[j] == 'LTPA_DUR_filt' & study_names[i] == 'GECKO'){
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
      # mypath <- file.path('~','plots',paste('model_6_0_',j,'_',k,'_',n, '.png',sep=''))
      # png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    } 
  }
}
#Store results
dev.off()
model_6_0_all <- study_regs
model_6_0_REM <- REM_results

# second stratum - 1
REM_results = list()
study_regs = data.frame()
ref_table = 'E4_6_1'

mypath <- file.path('~','plots','model_6_1.png')
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
      
      if (my_exposure[j] == 'LTPA_DUR_filt' & study_names[i] == 'GECKO'){
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
      # mypath <- file.path('~','plots',paste('model_6_1_',j,'_',k,'_',n, '.png',sep=''))
      # png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    } 
  }
}
#Store results
dev.off()
model_6_1_all <- study_regs
model_6_1_REM <- REM_results


# third stratum - 2

REM_results = list()
study_regs = data.frame()
ref_table = 'E4_6_2'

mypath <- file.path('~','plots','model_6_2.png')
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
      
      if (my_exposure[j] == 'LTPA_DUR_filt' & study_names[i] == 'GECKO'){
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
      # mypath <- file.path('~','plots',paste('model_6_2_',j,'_',k,'_',n, '.png',sep=''))
      # png(file=mypath, width = 1260, height = 940)
      REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n])
    } 
  }
}
#Store results
dev.off()
model_6_2_all <- study_regs
model_6_2_REM <- REM_results