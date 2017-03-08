datashield.logout(opals) # kill off any previous sessions

#----predefined variables -----#

# some useful values for later processing - how many studies and their names
num_studies <- 5
study_names <- c("DNBC", "GECKO", "HSS", "REPRO", "SWS")

# List of variables for the analysis

myvars = c('MOD_VIG_3_filt', 'LTPA_EE_3_filt', 'BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA',
              'GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING','ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 
              'GDM', 'MATERNAL_BMI', 'MATERNAL_OB', 'PREECLAMPSIA', 'BIRTH_WEIGHT_SGA')

covariates_mod_1 = c('GESTATIONAL_AGE', 'SEX')

covariates_mod_2 = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                     'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

covariates_mod_3 = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                     'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

#--------------- FUNCTIONS TO HELP WITH REGRESSIONS AND REM ------------------#

do_reg <- function(my_fmla, study, outcome, out_family, current_study, data_table){
  
  model <- ds.glm(formula= my_fmla, data = data_table, family = out_family, datasources = current_study, maxit = 100)
  model_coeffs <- as.data.frame(model$coefficients)
  model_coeffs$study = study
  model_coeffs$outcome = outcome
  model_coeffs$cov = rownames(model_coeffs)
  for (x in 1:3){ model_coeffs <- model_coeffs[,c(ncol(model_coeffs),1:(ncol(model_coeffs)-1))]}
  rownames(model_coeffs) = NULL
  return(model_coeffs)
}

do_REM <- function(coeffs, s_err, labels, fmla, out_family, variable, data_table){
  
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
    text(usr[1], usr[4], paste0(gsub(paste0(data_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=0.75)
    text(usr[1], usr[3], variable, adj = c( 0, 0 ),cex=0.75)
    
  }
  else if (out_family == 'binomial'){
    
    forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                            .(round(res$QEp,3)),')')),
           xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                             .(round(res$pval,3)))), atransf = exp)
    usr <- par("usr")
    text(usr[2], usr[4], "Relative Risk [95% CI]", adj = c(1, 4),cex=0.75)
    text(usr[1], usr[4], paste0(gsub(paste0(data_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=0.75)
    text(usr[1], usr[3], variable, adj = c( 0, 0),cex=0.75)
  }
  
  return(res)
  
}

ds.random_effects <- function(my_exposure, my_outcome, my_covariate, ref_table){
  
  REM_results = list()
  study_regs = data.frame()
  #ref_table = 'E4'
  
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
        
        if(study_names[i]=='REPRO'){
          #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
          # be inverted)
          fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
          reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family, opals[i], ref_table)
        }
        else if(study_names[i]=='DNBC'){
          #omit ethnicity, since it is 1 for all participants in REPRO (causes singular matrix that can't
          # be inverted)
          fmla <- as.formula(paste(ref_table,'$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table,'$',my_exposure[j]), paste0(ref_table, '$',my_covariate[! my_covariate %in% 'ETHNICITY'])), collapse= "+")))
          reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family, opals[i], ref_table)
        }
        else {
          fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+")))
          reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family, opals[i], ref_table)
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
        REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = outcome_family, variable = variables[n], data_table = ref_table)
      } 
    }
  }
  
  return(REM_results)
  
}
