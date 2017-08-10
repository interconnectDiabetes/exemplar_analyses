## Function for Survival Analyses in DataSHIELD
## Author: Paul Scherer
##		   Tom Bishop
## Date: 08/08/2017

# ###############################################################################
# ########################### FUNCTIONS  ########################################
# ###############################################################################
do_reg <- function(counter, my_fmla, study, outcome, out_family, studies = opals){
	temp <- ds.summary('D$TOTAL', datasources = studies)
	study_names <- names(temp)
	num_studies <- length(temp)
	rm(temp)
	
	# performs a regular regression and returns the coefficients of the fitted model as a dataframe
	print(studies[counter])
	print(my_fmla)
	model <- ds.glm(formula = my_fmla, data = ref_table, family = out_family, datasources=studies[counter], maxit = 25)
	model_coeffs <- as.data.frame(model$coefficients)
	model_coeffs$study = study
	model_coeffs$outcome = outcome
	model_coeffs$cov = rownames(model_coeffs)
	for (x in 1:3){ model_coeffs <- model_coeffs[,c(ncol(model_coeffs),1:(ncol(model_coeffs)-1))]}
	rownames(model_coeffs) = NULL
	return(model_coeffs)
}


do_reg_survival <- function(counter, my_fmla, study, outcome, out_family, offset_column, lexisTable, burtonWeights, studies = opals){
	# performs a survival analysis using the formula on the appropiately lexised table
	# note that the coefficients returned as a dataframe are not exponentiated. this is done
	# as part of the do_rem process
	temp <- ds.summary('D$TOTAL', datasources = studies)
	study_names <- names(temp)
	num_studies <- length(temp)
	rm(temp)
	
	print(studies[counter])
	print(my_fmla)
	model <- ds.glm(formula = my_fmla, data = lexisTable, family = "poisson", datasources=studies[counter], offset = offset_column, weights = burtonWeights, maxit = 100, checks = TRUE)
	model_coeffs <- as.data.frame(model$coefficients)
	model_coeffs$study = study
	model_coeffs$outcome = outcome
	model_coeffs$cov = rownames(model_coeffs)
	for (x in 1:3){ model_coeffs <- model_coeffs[,c(ncol(model_coeffs),1:(ncol(model_coeffs)-1))]}
	rownames(model_coeffs) = NULL
	return(model_coeffs)
}


do_REM <- function(coeffs, s_err, labels, fmla, out_family, variable){
	# takes a dataframe of coefficients and creates an appropiate random effects model whose results are stored as an
	# image of a forest plot and returns the random effects model 
	res <- rma(yi = coeffs, sei = s_err, method='DL', slab = labels)

	#add the weights to the labels
	res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")
	
	#forest plots
	if (out_family == 'gaussian') {
		forest(res, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
			.(sprintf("%.3f", round(res$QEp,3))),')')),
		xlab=bquote(paste('Test of H'[0]*': true mean association = 0, p = ',
			.(sprintf("%.3f", round(res$pval,3))))), cex=1, cex.lab=0.75, cex.axis=1)
		usr <- par("usr")
		text(usr[2], usr[4], "Beta [95% CI]", adj = c(1, 4),cex=1)
		text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=1)
		text(usr[1], usr[3], variable, adj = c( 0, 0 ),cex=1)
	}
	else if (out_family == 'poisson'){
		forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
			.(round(res$I2)),'%, p = ',
			.(round(res$QEp,3)),')')),
		xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
			.(round(res$pval,3)))), atransf = exp)
		usr <- par("usr")
		text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
		text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=0.75)
		text(usr[1], usr[3], variable, adj = c( 0, 0 ),cex=0.75)
	}
	else if (out_family == 'binomial'){
		forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
			.(sprintf("%.3f", round(res$QEp,3))),')')),
		xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
			.(sprintf("%.3f", round(res$pval,3))))), atransf = exp, cex=1, cex.lab=0.75, cex.axis=1)
		usr <- par("usr")
		
		text(usr[2], usr[4], "Relative Risk [95% CI]", adj = c(1, 4),cex=1)
		text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=1)
		text(usr[1], usr[3], variable, adj = c( 0, 0),cex=1)
	}
	return(res)
}


findOutcomeFamily <- function(ref_table, outcome, studies = opals){
	# Find outcome family for regression
	# used in the do_reg function
	out_class = ds.class(paste0(ref_table, '$', outcome), datasources = studies)[[1]]
	if (out_class == 'factor') {
		outcome_family = 'binomial'
	}
	else if (out_class == 'numeric' | out_class == 'integer') {
		outcome_family = 'gaussian'
	}
	return(outcome_family)
}


createModelFormula <- function(studyName, data_table, outcome, exposure, covariate_list, interaction_term = NULL, type = "survival") {
	if (studyName == "InterAct_france"){ 
		exceptions = c("SEX", "SUPPLEMENTS")
	} 
	else if (studyName == "InterAct_italy") {
		exceptions = c("FAM_DIAB", "SUPPLEMENTS")
	}	
	else if (studyName == "InterAct_spain") {
		exceptions = c("FAM_DIAB", "SUG_BEVS", "SUPPLEMENTS")
	} 
	else if (studyName == "InterAct_uk") {
		exceptions = c("SUPPLEMENTS")
	} 
	else if (studyName == "InterAct_netherlands") {
		exceptions = c("SUPPLEMENTS")
	} 
	else if (studyName == "InterAct_germany") {
		exceptions = c("SUPPLEMENTS")
	} 
	else if (studyName == "InterAct_sweden") {
		exceptions = c("SUPPLEMENTS")
	} 
	else if (studyName == "InterAct_denmark") {
		exceptions = c("SUPPLEMENTS")
	} 
	else if (studyName == "HOORN") {
		exceptions = c("SUPPLEMENTS")
	} 
	else if (studyName == "NHAPC") {
		exceptions = c("FAM_DIAB")
	} 
	else if (studyName == "NOWAC") {
		exceptions = c("SEX", "FAM_DIAB", "WAIST")
	} 
	else if (studyName == "Zutphen") {
		exceptions = c("MEAT", "SEX", "WAIST")
	}
	else if (studyName == "AusDiab") {
		exceptions = c("FATTY", "LEAN", "NONFISH", "FIBER", "SUG_BEVS", "FRESH", "SALT", "SSD", "SUPPLEMENTS", "E_INTAKE")
	} 
	else {
		exceptions = c()
	}

	if (type == "standard"){
		fmla <- as.formula(paste(data_table, '$', outcome," ~ ", paste0(c(paste0(data_table, '$',exposure), 
			paste0(data_table, '$',covariate_list[! covariate_list %in% exceptions])), collapse= "+")))
		return(fmla)
	} else if (type == "survival") {
		fmla <- as.formula(paste("censor"," ~ ", "0" , "+", 'tid.f', '+', paste0(c(paste0(data_table, '$',exposure), 
				paste0(data_table, '$',covariate_list[! covariate_list %in% exceptions])), collapse= "+")))
		return (fmla)
	} else if (type == "interaction") {
		fmla <- as.formula(paste("censor"," ~ ",  "0" , "+", 'tid.f', '+', paste0(c(paste0(data_table, '$', exposure), 
			paste0(data_table, '$',covariate_list[! covariate_list %in% exceptions])), collapse= "+"),"+", data_table,"$",interaction_term,"*", data_table,"$", exposure))
		return (fmla)
	} else {
		stop("Undefined type of model for formula")
		return(NULL)
	}
}


checkFactoredTime <- function(timeVariable = 'tid.f', studies = opals){
  # checks a factored time to censor representation typically tid.f for any zero valued levels
  # as this can be used as an indicator for singular matrices to come at the regression stage
  # start check with isValid 
  isValidList = ds.isValid(timeVariable, datasources = studies)
  
  if (all(isValidList)) { 
    # even if all the time variables are valid according to datashield, there may be levels with
    # 0 participants in the factored version of the time variable
    for (study in c(1:length(studies))) {
      print(study)
      print(studies[study])
      timeVariableSummary = ds.summary(timeVariable, datasources = studies[study])
      timeVariableSummary = (timeVariableSummary[[1]])[c(4:length(timeVariableSummary[[1]]))]
      
      for (i in timeVariableSummary) {
        if (i == 0){
          print(ds.summary('tid.f', datasources = studies[study]))
          stop("One of the levels in the timeVariable has zero members in it!")
        }
      }
    }
  } else {
    print(isValidList)
    stop("There are study(ies) with invalid values for the timeVariable!")
  }
  return(NULL)
}



runRegModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals){
	# main function that runs, fits, and stores the results of a regression model using the 
	# datashield process
	temp <- ds.summary('D$TOTAL', datasources = studies)
	study_names <- names(temp)
	num_studies <- length(temp)
	rm(temp)

	REM_results = list()
	study_regs = data.frame()

	svg(filename=mypath, 
		width=4.5 * length(my_exposure), 
		height=3.5 * length(my_outcome), 
		pointsize=10)
	par(mar=c(5,3,2,2)+0.1)
	par(mfrow=c(length(my_outcome),length(my_exposure)))
	par(ps=10)

	for (k in 1:length(my_outcome)){
		outcome_family = findOutcomeFamily(ref_table, my_outcome[k])

		# for each exposure and
		for (j in 1:length(my_exposure)){
			estimates = vector()
			s_errors = vector()
			labels = vector()

			for(i in 1:length(studies)) {
				reg_data <- data.frame()

				fmla <- createModelFormula(study_names[i], ref_table, my_outcome[k], my_exposure[j], my_covariate, type = "standard")
				reg_data <- do_reg(i,fmla, names(studies[i]), my_outcome[k], outcome_family, studies)
				
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
	model_all <- study_regs
	model_rem <- REM_results

	return (list(model_all, model_rem))
}


tunedLexisB <- function(ref_table, study) {
	# does a tuned Lexis B on one study can be used in a parallel loop to run
	# multiple lexis b at once
	# TODO check prerequisites with dsExists
  studyName = names(study)
	# assign Danger.NFILTER to some values as the current code in the dsBeta doesnt work without this.
	# and expand the dataset using the ds.lexis b command
	datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(0.1)), opals = study)
	datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(0.1)), opals = study)
	idColString = paste0(ref_table, '$', 'ID')
	entryColString = paste0(ref_table, '$', 'newStartDate')
	exitColString = paste0(ref_table, '$', 'newEndDate')
	statusColString = paste0(ref_table, '$', 'CASE_OBJ')

	if (studyName == "InterAct_france"){ 
		interval_width =  c(2,2,1,3.5,2,2,1,2,2,2)
	} 
	else if (studyName == "InterAct_italy") {
		interval_width =  c(2,2,1,3.5,2,2,1,2,2,2)
	}	
	else {
		interval_width =  c(2,2,1,3.5,2,2,1,2,2,2)
	}

	ds.lexis.b(data=ref_table, intervalWidth = interval_width, idCol = idColString, entryCol = entryColString, 
		exitCol = exitColString, statusCol = statusColString, expandDF = 'A', datasources = study)
	
	print("Did LexisB for study")
	print(studyName)
}


tunedSurvivalModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, interval_width, studies = opals) {
	# main function that runs, fits, and stores the results of a survival model using the 
	# lexisB function to expand the dataframe and also rebases the start and endtimes of the data variables.
	temp <- ds.summary('D$TOTAL', datasources = studies)
	study_names <- names(temp)
	num_studies <- length(temp)
	rm(temp)

	REM_results = list()
	study_regs = data.frame()

	svg(filename=mypath, 
		width=5*length(my_exposure), 
		height=4*length(my_outcome), 
		pointsize=10)
	par(mar=c(5,3,2,2)+0.1)
	par(mfrow=c(length(my_outcome),length(my_exposure)))
	par(ps=10)

	for (study in c(1:length(studies))) {
		tunedLexisB(ref_table, studies[study])
	}

	checkFactoredTime(studies = studies)
	ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA', datasources = studies)
	lexised_table = 'A'

	for (k in 1:length(my_outcome)){

		# for each exposure and
		for (j in 1:length(my_exposure)){
			estimates = vector()
			s_errors = vector()
			labels = vector()

			for(i in 1:length(studies)) {
				reg_data <- data.frame()
				
				fmla <- createModelFormula(study_names[i], lexised_table, my_outcome[k], my_exposure[j], my_covariate, type = "survival")
				reg_data <- do_reg_survival(i, my_fmla = fmla, study = names(studies[i]), outcome =  my_outcome[k],  out_family = "poisson", offset_column = "logSurvivalA", lexisTable = lexised_table, burtonWeights = paste0(lexised_table, "$burtonWeights"), studies = studies)

				study_regs = rbind(study_regs,reg_data)
				estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
				s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
				labels = rbind(labels, reg_data[2,1])      
				variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
			}
			fmla <- as.formula(paste("censor"," ~ ", '0', '+', 'tid.f', '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
			#meta analysis here
			for (n in 1:length(variables)){
				REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = "poisson", variable = variables[n])
			}
		}
	}

	#Store results
	dev.off()
	model_all <- study_regs
	model_rem <- REM_results

	return (list(model_all, model_rem))
}


runSurvivalModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, interval_width, studies = opals) {
	# main function that runs, fits, and stores the results of a survival model using the 
	# lexisB function to expand the dataframe and also rebases the start and endtimes of the data variables.
	temp <- ds.summary('D$TOTAL', datasources = studies)
	study_names <- names(temp)
	num_studies <- length(temp)
	rm(temp)

	REM_results = list()
	study_regs = data.frame()

	svg(filename=mypath, 
		width=5*length(my_exposure), 
		height=4*length(my_outcome), 
		pointsize=10)
	par(mar=c(5,3,2,2)+0.1)
	par(mfrow=c(length(my_outcome),length(my_exposure)))
	par(ps=10)

	# assign Danger.NFILTER to some values as the current code in the dsBeta doesnt work without this.
	# and expand the dataset using the ds.lexis b command
	datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(0.1)), opals = studies)
	datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(0.1)), opals = studies)
	idColString = paste0(ref_table, '$', 'ID')
	entryColString = paste0(ref_table, '$', 'newStartDate')
	exitColString = paste0(ref_table, '$', 'newEndDate')
	statusColString = paste0(ref_table, '$', 'CASE_OBJ')
	ds.lexis.b(data=ref_table, intervalWidth = interval_width, idCol = idColString, entryCol = entryColString, 
		exitCol = exitColString, statusCol = statusColString, expandDF = 'A', datasources = studies)
	
	ds.asNumeric('A$CENSOR','censor', datasources = studies)
	ds.asFactor('A$TIME.PERIOD','tid.f', datasources = studies)
	checkFactoredTime(studies = studies)
	ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA', datasources = studies)
	lexised_table = 'A'

	for (k in 1:length(my_outcome)){

		# for each exposure and
		for (j in 1:length(my_exposure)){
			estimates = vector()
			s_errors = vector()
			labels = vector()

			for(i in 1:length(studies)) {
				reg_data <- data.frame()
				
				fmla <- createModelFormula(study_names[i], lexised_table, my_outcome[k], my_exposure[j], my_covariate, type = "survival")
				reg_data <- do_reg_survival(i, my_fmla = fmla, study = names(studies[i]), outcome =  my_outcome[k],  out_family = "poisson", offset_column = "logSurvivalA", lexisTable = lexised_table, burtonWeights = paste0(lexised_table, "$burtonWeights"), studies = studies)

				study_regs = rbind(study_regs,reg_data)
				estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
				s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
				labels = rbind(labels, reg_data[2,1])      
				variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
			}
			fmla <- as.formula(paste("censor"," ~ ", '0', '+', 'tid.f', '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
			#meta analysis here
			for (n in 1:length(variables)){
				REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = "poisson", variable = variables[n])
			}
		}
	}

	#Store results
	dev.off()
	model_all <- study_regs
	model_rem <- REM_results

	return (list(model_all, model_rem))
}


runIncrementalSurvivalModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath_prefix, interval_width, studies = opals){
	# Runs survival models incrementally through a list of provided covariates, producing randomeffectmodels and therein
	# forest plots along the way. Mainly used for exploratory purposes.
  temp <- ds.summary('D$TOTAL', datasources = studies)
  study_names <- names(temp)
  num_studies <- length(temp)
  rm(temp)
  
	REM_results = list()
	study_regs = data.frame()
	overall_df = data.frame()
	for (i in 1:length(my_covariate)){
		mypath_func = file.path(paste0(mypath_prefix, "_", i, "_out_of_", length(my_covariate), ".svg"))
		sub_covariate_list = my_covariate[1:i]
		runResults = runSurvivalModel(ref_table, my_exposure, my_outcome, sub_covariate_list, mypath_func, c(2,2,2,2,2,2,2,2,2,2), studies)
		runCoeffs = runResults[[1]]
		overall_df = rbind(overall_df, runCoeffs)
		print(mypath_func)
	}
	return(overall_df)
}


runInteractionModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, interval_width, interaction_term, studies = opals) {
	# Runs an interaction model of the given interaction term. It is similar to the normal survival model runSurvival functions
	# however the formula has to take the additional interaction term into account.
  temp <- ds.summary('D$TOTAL', datasources = studies)
  study_names <- names(temp)
  num_studies <- length(temp)
  rm(temp)
  
	REM_results = list()
	study_regs = data.frame()

	number_of_interactions <- length(ds.levels(paste0(ref_table,'$',interaction_term), datasources = studies)[[1]])

	svg(filename=mypath, 
		width=5*number_of_interactions, 
		height=4*length(my_outcome), 
		pointsize=10)
	par(mar=c(5,3,2,2)+0.1)
	par(mfrow=c(length(my_outcome),number_of_interactions))
	par(ps=10)

	# assign Danger.NFILTER to some values as the current code in the dsBeta doesnt work without this.
	# and expand the dataset using the ds.lexis b command
	datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(1)), opals = studies)
	datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(1)), opals = studies)
	idColString = paste0(ref_table, '$', 'ID')
	entryColString = paste0(ref_table, '$', 'newStartDate')
	exitColString = paste0(ref_table, '$', 'newEndDate')
	statusColString = paste0(ref_table, '$', 'CASE_OBJ')
	ds.lexis.b(data=ref_table, intervalWidth = interval_width, idCol = idColString, entryCol = entryColString, 
	           exitCol = exitColString, statusCol = statusColString, expandDF = 'A', datasources = studies)
	
	ds.asNumeric('A$CENSOR','censor', datasources = studies)
	ds.asFactor('A$TIME.PERIOD','tid.f', datasources = studies)
	checkFactoredTime(studies = studies)
	ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA', datasources = studies)
	lexised_table = 'A'

	for (k in 1:length(my_outcome)){
	  
		# for each exposure and
		for (j in 1:length(my_exposure)){
			estimates = vector()
			s_errors = vector()
			labels = vector()

			for(i in 1:length(studies)) {
				reg_data <- data.frame()
        		fmla <- createModelFormula(study_names[i], lexised_table, my_outcome[k], my_exposure[j], my_covariate, interaction_term, type = "interaction")
				reg_data <- do_reg_survival(i, my_fmla = fmla, study = names(studies[i]), outcome =  my_outcome[k],  out_family = "poisson", offset_column = "logSurvivalA", lexisTable = lexised_table, burtonWeights = paste0(lexised_table, "$burtonWeights"), studies = studies)
				study_regs = rbind(study_regs,reg_data)
				# estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
				s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
				estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
				labels = rbind(labels, reg_data[2,1])      
				variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
			}

			fmla <- createModelFormula(study_names[i], lexised_table, my_outcome[k], my_exposure[j], my_covariate, interaction_term, type = "interaction")
			
			#meta analysis here
			for (n in 1:length(variables)){
				REM_results[[paste(c(my_outcome[k], my_exposure[j],my_covariate, variables[n],'REM'),collapse="_")]]  <- do_REM(estimates[,n], s_errors[,n], labels, fmla,out_family = "poisson", variable = variables[n])
			}
		}
	}

	#Store results
	dev.off()
	model_all <- study_regs
	model_rem <- REM_results

	return (list(model_all, model_rem))
}


runMediationModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath_prefix, interval_width, my_mediation, studies = opals) {
	# Runs a mediation survival model, with the provided extra mediations.
	# ie. it does the following:
	# 1. it runs the simple relationship outcome~exposure + covariates
	# 2. it runs the relationship mediator~exposure + covariates
	# 3. it runs the relationship outcome~mediator + covariates
	# 4. it runs the relationship outcome~exposure + covariates + mediator
	# make sure my_mediation is a vector with the string mediator

	# outcome~exposure + covariates
	mypath_1 = file.path(paste0(mypath_prefix, "_", "part_1", ".svg"))
	mediate1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath_1, interval_width, studies)

	# mediator~exposure + covariates
	mypath_2 = file.path(paste0(mypath_prefix, "_", "part_2", ".svg"))
	mediate2 = runSurvivalModel(ref_table, my_exposure, my_mediation, my_covariate, mypath_2, interval_width, studies)
	
	# outcome~mediator + covariates
	mypath_3 = file.path(paste0(mypath_prefix, "_", "part_3", ".svg"))
	mediate3 = runSurvivalModel(ref_table, my_mediation, my_mediation, my_covariate, mypathk_3, interval_width, studies)

	# outcome~exposure + covariates + mediator
	my_covariate = c(my_covariate, my_mediation)
	mypath_4 = file.path(paste0(mypath_prefix, "_", "part_4", ".svg"))
	mediate4 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath_4, interval_width, studies)

	return(list(mediate1, mediate2, mediate3, mediate4))
}


runStratificationModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath_prefix, interval_width, stratified_var, studies = opals) {
	# Runs a stratified set of survival models given the categorical variable to be stratified
	# could possibly be better implemented as a simple survival model where the input dataframe "ref_table" is
	# instead just subset before hand, makes it easier to see in the analysis code when reading
	# this function does the stratification automatically and runs a survival model on the newly stratified dataframe 

	# elicit categories
	cats = ds.summary(paste0('D$', stratified_var))[[1]]$categories # assumes that the categories of first study is shared
	list_of_models = vector("list", length(cats))

	for (category in 1:length(cats)){
		newTable = paste0(ref_table, '_S_', category)
		logicalOperatorString = paste0(stratified_var, '==')
		ds.subset(x = ref_table, subset = newTable, logicalOperator = logicalOperatorString, threshold = cats[category], datasources = studies)

		# run the model on the stratified dataframe and append to list of models
		mypath_func = file.path(paste0(mypath_prefix, "_",stratified_var,"==", cats[category], ".svg"))
		stratified_model = runSurvivalModel(newTable, my_exposure, my_outcome, my_covariate, mypath_func, interval_width, studies = opals)
		list_of_models[category] = stratified_model
	}

	return(list_of_models)
}


metaRegression <- function(outcome, exposure) {
	return (NULL)
}