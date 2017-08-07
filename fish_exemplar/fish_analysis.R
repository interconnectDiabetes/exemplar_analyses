\## Analysis script for Fish Exemplar Analysis
## Author: Paul Scherer
##		   Tom Bishop
## Date: 31/03/2017

###############################################################################
########################### Dependencies   ####################################
###############################################################################
library(opal)
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(dsBetaTestClient)
library(metafor)


###############################################################################
########################### SET UP SERVERS  ###################################
###############################################################################
# Set working directory to source our credentials
setwd("/home/l_pms69/exemplar_analyses/")
#setwd("/home/l_trpb2/git/exemplar_analyses/")

# get extra functions from betaTestClient that didnt make it
source("fish_exemplar/helperFunctions.R")
# Retrieve Credential Details
source("creds/fish_exemplar_creds.R")
setwd("~")

# Logout in case there was any older session in place and login with chosen variables
datashield.logout(opals)
myvars = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
           "FUP_OBJ", "FUP_OBJ_SELF", "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           "FAM_DIAB", "E_INTAKE", "FRUIT", "VEG",  "FIBER", "SUG_BEVS", "WAIST", "SUPPLEMENTS", 
           "AGE_END_OBJ_SELF", "AGE_END_OBJ", "AGE_BASE", "MEAT", "COMORBID")
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish') # for all available variables

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# all participants
all_participants <- ds.length('D$TOTAL')
all_participants_split <- ds.length('D$TOTAL',type = 'split')

# Set studynames and numstudies
temp <- ds.summary('D$TOTAL')
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

# remove participants with prevalent diabetes and type 1
ds.subset(x = 'D', subset = 'E1', logicalOperator = 'PREV_DIAB==', threshold = 0)
noPrevalence <- ds.length('E1$SEX', type = 'split')
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB==', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# In order to deal with the intake subsets stratified by sex we will have to create subsets of sex,
# do the intake subset and then rbind the groups back together. What follows is DataSHIELD magic
ds.asNumeric("E2$SEX", newobj = "sexNumbers")
ds.assign(toAssign="(sexNumbers*300)+E2$E_INTAKE", newobj = "adjustedLowerBound")
ds.assign(toAssign="(sexNumbers*700)+E2$E_INTAKE", newobj = "adjustedUpperBound")
ds.cbind(x=c("adjustedLowerBound", "E2"), newobj = "L1")
ds.cbind(x=c("adjustedUpperBound", "L1"), newobj = "L2")
# remove participants with too little and excessive consumption of calories
ds.subset(x = 'L2', subset = 'E3', logicalOperator = 'adjustedUpperBound<=', threshold = 4200)
under3500cal <- ds.length('E3$SEX', type = 'split')
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'adjustedLowerBound>=', threshold = 800)
afterIntake <- ds.length('E4$SEX', type = 'split')

# Setup an additional proxy ID column for each study 
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('ID','E4'), newobj='D2')

# Zeros, new start date and end date
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
rm(i)
ds.cbind(x=c('newStartDate','D2'), newobj='D3')
ds.assign(toAssign = 'D3$FUP_OBJ', newobj = 'newEndDate')
ds.cbind(x=c('newEndDate','D3'), newobj='D5')

# Adding in the weights as described by Dr. Burton
ds.asNumeric('D5$CASE_OBJ', newobj = "caseNums")
ds.assign(toAssign="((1 - caseNums)*35.92055) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_france'])
ds.assign(toAssign="((1 - caseNums)*23.55086) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_italy'])
ds.assign(toAssign="((1 - caseNums)*11.0115) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_spain'])
ds.assign(toAssign="((1 - caseNums)*27.87205) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_uk'])
ds.assign(toAssign="((1 - caseNums)*24.27497) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_netherlands'])
ds.assign(toAssign="((1 - caseNums)*24.62187) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_germany'])
ds.assign(toAssign="((1 - caseNums)*17.68276) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_sweden'])
ds.assign(toAssign="((1 - caseNums)*27.28305) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_denmark'])

# Non InterAct studies get a weighting of 1 in either case or noncase
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['HOORN'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['NHAPC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['NOWAC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['SMC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['ELSA'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['Whitehall'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['Zutphen'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['AusDiab'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['JPHC'])
ds.cbind(x=c('burtonWeights','D5'), newobj='D6')

# put in any dummy columns for the studies with completely missing columns
# italy missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['InterAct_italy'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['InterAct_italy'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['InterAct_italy'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['InterAct_italy'])
# spain missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['InterAct_spain'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['InterAct_spain'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['InterAct_spain'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['InterAct_spain'])
# nowac missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['NOWAC'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['NOWAC'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['NOWAC'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['NOWAC'])

# Loop to produce E4 and model_all_len for descriptive stats
# Note that this doesnt actually handle well if a study has lost all its participants before this section
my_vars_all = c("AGE_BASE", "CASE_OBJ", "TOTAL",
                "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL", "COMORBID", "E_INTAKE", "FRUIT",
                "VEG", "FIBER", "MEAT", "SUG_BEVS", "FAM_DIAB", "WAIST", "SUPPLEMENTS","newEndDate", "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all)
ds.subset(x = 'D7', subset = 'D4', completeCases = TRUE)
length_complete = ds.length('D4$SEX')
length_complete_split = ds.length("D4$SEX", type = "split")

## Dataframe to hold length figures
# model_all_len <- data.frame()
# model_all_len <- rbind(model_all_len, all_participants_split, noPrevalence, noType1, under3500cal, afterIntake)
# for (i in 2:length(my_vars_all)){
#   print(my_vars_all[1:i])
#   ds.subset(x = 'D6', subset = 'E6', cols =  my_vars_all[1:i])
#   ds.subset(x = 'E6', subset = 'E7', completeCases = TRUE)
#   thingToBind = vector("numeric")
#   print(i)
#   for (k in 1:num_studies){
#     lengthNum = ds.length('E7$ID', datasources = opals[k])
#     thingToBind = c(thingToBind, lengthNum)
#     print(thingToBind)
#   }
#   thingToBind = unlist(unname(thingToBind))
#   print("this is thingtobind unlistedunnamed")
#   print(k)
#   print(thingToBind)
#   model_all_len = rbind(model_all_len, thingToBind)
# }
# rownames = c("ALL", "PREV_DIAB", "TYPE_DIAB", "under3500cal", "afterIntake", my_vars_all[2:length(my_vars_all)])
# row.names(model_all_len) <- rownames


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
		for (study in studies) {
			timeVariableSummary = ds.summary(timeVariable, datasources = study)
			timeVariableSummary = (timeVariableSummary[[1]])[c(4:length(timeVariableSummary[[1]]))]
			for (i in timeVariableSummary) {
				if (i == 0){
					print(ds.summary('tid.f', datasources = studies[study]))
					stop("One of the levels in the timeVariable has zero members in it!")
				}
			}
			return(TRUE)
		}
	} else {
		print(isValidList)
		stop("There are study(ies) with invalid values for the timeVariable!")
	}
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




# ___  ___          _      _   __  
# |  \/  |         | |    | | /  | 
# | .  . | ___   __| | ___| | `| | 
# | |\/| |/ _ \ / _` |/ _ \ |  | | 
# | |  | | (_) | (_| |  __/ | _| |_
# \_|  |_/\___/ \__,_|\___|_| \___/

# The new idea from Silvia is to start using semi complete cases, ie we look at only the data that concerns the variables we want to look at
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "newEndDate", "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all)
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE)

                                 
# Exposure: total fish (g/d) at baseline
# Outcome: CASE_OBJ
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities
# To assess the impact of each confounder we will also run models including each confounder separately.
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")


# Simple Regression Model For Testing Quickly 
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_normal_regression.svg')
model_1reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath)
model_1reg_all = model_1reg_results[[1]]
model_1reg_REM = model_1reg_results[[2]]

# survival version with lexis b
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_survival.svg')
model_1 = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,1,3.5,2,2,1,2,2,2))
model_1_all = model_1[[1]]
model_1_rem = model_1[[2]]

# # incremental model 1
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_1_incremental')
model_1_inc = runIncrementalSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2))

# ___  ___          _      _   _____ 
# |  \/  |         | |    | | / __  \
# | .  . | ___   __| | ___| | `' / /'
# | |\/| |/ _ \ / _` |/ _ \ |   / /  
# | |  | | (_) | (_| |  __/ | ./ /___
# \_|  |_/\___/ \__,_|\___|_| \_____/

# Model 2a: As model 1 + adj for energy intake, alcohol intake, fibre intake, meat intake, 
#     fruit intake, vegetables intake, sugary drinks intake, fish oil supplements

studies_model2 = study_names[! study_names %in% c("NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA")]
opals_model2 = opals[studies_model2]


my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

# Survival Model
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2a_survival.svg')
model_2a = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), studies = opals_model2)
model_2a_all = model_2a[[1]]
model_2a_rem = model_2a[[2]]

# Normal Regression for Error Checking
ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_2_normal_regression.svg')
model_2reg_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals_model2)
model_2reg_all = model_2reg_results[[1]]
model_2reg_REM = model_2reg_results[[2]]

# # Incremental Model
# ref_table = 'D8'
# mypath = file.path('~', 'plots', 'model_2_incremental')
# model_2_inc = runIncrementalSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), studies = opals_model2)

# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |____ |
# | .  . | ___   __| | ___| |     / /
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | .___/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 
# sensitivity analysis
# Model 3a: As model 2 + adj for family history of diabetes

# The new idea from Silvia is to start using semi complete cases, ie we look at only the data that concerns the variables we want to look at
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "newEndDate", "newStartDate", "burtonWeights","FAM_DIAB")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D9', completeCases = TRUE, datasources = opals)

studies_model3 = study_names[! study_names %in% c("NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA")]
opals_model3 = opals[studies_model3]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", 
                  "FAM_DIAB")

ref_table = 'D9'
mypath = file.path('~', 'plots', 'model_3a_survival.svg')
model_3a = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_model3)
model_3a_all = model_3a[[1]]
model_3a_rem = model_3a[[2]]

# Model 3b: As model 2 + adj for waist circumference
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "newEndDate", "newStartDate", "burtonWeights","WAIST")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D10', completeCases = TRUE, datasources = opals)

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", 
                  "WAIST")

ref_table = 'D10'
mypath = file.path('~', 'plots', 'model_3b_survival.svg')
model_3b = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_model3)
model_3b_all = model_3b[[1]]
model_3b_rem = model_3b[[2]]


# Model 3c: As model 2 + adj for fish oil supplements
# ABSOLUTELY NO ONE HAS WORKING SUPPLEMENTS
my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS","newEndDate", "newStartDate", "burtonWeights",  "SUPPLEMENTS")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D11', completeCases = TRUE, datasources = opals)

studies_model3c = study_names[! study_names %in% c("NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA", "AusDiab")]
opals_model3c = opals[studies_model3c]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID", 
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", 
                  "SUPPLEMENTS")

ref_table = 'D11'
mypath = file.path('~', 'plots', 'model_3c_survival.svg')
model_3c = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_model3c)
model_3c_all = model_3c[[1]]
model_3c_rem = model_3c[[2]]


# ___  ___          _      _     ___ 
# |  \/  |         | |    | |   /   |
# | .  . | ___   __| | ___| |  / /| |
# | |\/| |/ _ \ / _` |/ _ \ | / /_| |
# | |  | | (_) | (_| |  __/ | \___  |
# \_|  |_/\___/ \__,_|\___|_|     |_/

# Exposure: total fish (g/d) at baseline*sex
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, 
#             fibre intake, meat intake, fruit intake, vegetables intake, sugary drinks intake.

# Stratified analyses by sex (men, women) if significant

my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "newEndDate", "newStartDate", "burtonWeights","SUG_BEVS")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals)
ds.subset(x = 'D7', subset = 'D12', completeCases = TRUE, datasources = opals)

studies_no_singleGender = study_names[! study_names %in% c("InterAct_france", "NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA")]
opals_no_SG = opals[studies_no_singleGender]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "SEX")
my_interaction = "SEX"

ref_table = 'D12'
mypath = file.path('~', 'plots', 'model_4_surv.svg')
model_4 = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, opals_no_SG)
model_4_all = model_4[[1]]
model_4_rem = model_4[[2]]

## If the result was significant
# Men
ds.subset(x = 'D12', subset = 'D12_men', logicalOperator = 'SEX==', threshold = 0, datasources = opals)
men <- ds.length('D12_men$SEX', type = 'split', datasources = opals)
ref_table = 'D12_men'
mypath = file.path('~', 'plots', 'model_4_men_surv.svg')
model_4men = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_no_SG)
model_4men_all = model_4men[[1]]
model_4men_rem = model_4men[[2]]

# Women
ds.subset(x = 'D12', subset = 'D12_women', logicalOperator = 'SEX==', threshold = 1, datasources = opals)
women <- ds.length('D12_women$SEX', type = 'split', datasources = opals)
ref_table = 'D12_women'
mypath = file.path('~', 'plots', 'model_4_women_surv.svg')
model_4women = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), opals_no_SG)
model_4women_all = model_4women[[1]]
model_4women_rem = model_4women[[2]]

# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |  ___|
# | .  . | ___   __| | ___| | |___ \ 
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 

# Exposure: total fish (g/d) at baseline*BMI
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, co-morbidities, BMI, energy intake, fibre intake, red and processed meat intake, fruit intake, vegetables intake, sugary drinks intake.
# 
# Stratified analyses by BMI (BMI<25, BMI ≥25) if significant

# Studies involved in model 5
studies_model5 = study_names[! study_names %in% c("NOWAC", "Zutphen", "HOORN", "NHAPC", "ELSA")]
opals_model5 = opals[studies_model5]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")
my_interaction = "BMI"

ref_table = 'D8'
mypath = file.path('~', 'plots', 'model_5_surv.svg')
model_5 = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_model5)
model_5_all = model_5[[1]]
model_5_rem = model_5[[2]]


my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

# BMI < 25
ds.subset(x = 'D8', subset = 'underweight', logicalOperator = 'BMI==', threshold = 0, datasources = opals)
men <- ds.length('underweight$SEX', type = 'split', datasources = opals)
ref_table = 'underweight'
mypath = file.path('~', 'plots', 'model_5_underweight_surv.svg')
model_underweight = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2),  studies = opals_model5)
model_underweight_all = model_underweight[[1]]
model_underweight_rem = model_underweight[[2]]

# BMI >= 25
ds.subset(x = 'D8', subset = 'overweight', logicalOperator = 'BMI==', threshold = 1, datasources = opals)
women <- ds.length('overweight$SEX', type = 'split', datasources = opals)
ref_table = 'overweight'
mypath = file.path('~', 'plots', 'model_5_overweight_surv.svg')
model_overweight = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2),  studies = opals_model5)
model_overweight_all = model_overweight[[1]]
model_overweight_rem = model_overweight[[2]]


# ___  ___          _      _    ____ 
# |  \/  |         | |    | |  / ___|
# | .  . | ___   __| | ___| | / /___ 
# | |\/| |/ _ \ / _` |/ _ \ | | ___ \
# | |  | | (_) | (_| |  __/ | | \_/ |
# \_|  |_/\___/ \__,_|\___|_| \_____/

# Present analyses by geographical area (Central area, Eastern area, Western area)
# subset opals list by geographic area then carry out regression for each one on their own.
opals_central = opals[c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                      "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                      "InterAct_denmark", "NOWAC", "SMC", "Whitehall")]
opals_western = opals["elsa"]
opals_eastern = opals["NHAPC", "JPHC"]

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

# Assign country code to each of the studies
# Adding in the weights as described by Dr. Burton
######################################################
# central area
######################################################
length_central = ds.length('D4$SEX', datasources = opals_central)
for(i in 1:length(opals_central)){
  ds.assign(toAssign = "newStartDate + 1", newobj = "geocode", datasources = opals_central[i])
  ds.asFactor(x = "geocode", newobj = "geocode", datasources = opals_central[i])
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('geocode','D4'), newobj='D4')

ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_6_central_surv.svg')
model_6central = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_central)
model_6central_all = model_6central[[1]]
model_6central_rem = model_6central[[2]]

######################################################
# eastern area
######################################################
length_eastern = ds.length('D4$SEX', datasources = opals_eastern)
for(i in 1:length(opals_eastern)){
  ds.assign(toAssign = "newStartDate + 1", newobj = "geocode", datasources = opals_eastern[i])
  ds.asFactor(x = "geocode", newobj = "geocode", datasources = opals_eastern[i])
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('geocode','D4'), newobj='D4')


ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_6_eastern_surv.svg')
model_6eastern = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_eastern)
model_6eastern_all = model_6eastern[[1]]
model_6eastern_rem = model_6eastern[[2]]

######################################################
# western area
######################################################
length_central = ds.length('D4$SEX', datasources = opals_western)
for(i in 1:length(opals_western)){
  ds.assign(toAssign = "newStartDate + 1", newobj = "geocode", datasources = opals_western[i])
  ds.asFactor(x = "geocode", newobj = "geocode", datasources = opals_western[i])
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('geocode','D4'), newobj='D4')

ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_6_eastern_surv.svg')
model_6western = runInteractionModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2), my_interaction, studies = opals_western)
model_6western_all = model_6western[[1]]
model_6western_rem = model_6western[[2]]

# Meta regression
# inside of a meta regression you take the regression coefficient values and regress them against another trait (like the geographical area)
# and then look at the coefficients here.
# we can just use the regression coefficients created out of the values and 
# then do local linear regression to see the relationship between the variables.
study_regression_coefficients = c(model_6central_all, model_6western_all, model_6eastern_all)
central_codes = rep(1, times = length(opals_central))
western_codes = rep(2, times = length(opals_western))
eastern_codes = rep(3, times = length(opals_eastern))
geocodes = as.factor(x = c(central_codes, western_codes, eastern_codes))

meta_fmla = "total_coeffs wrt censor ~  geocodes"
meta_regression_model = lm(formula = meta_fmla)













