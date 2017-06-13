## Analysis script for Fish Exemplar Analysis
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

# Retrieve Credential Details
source("creds/fish_exemplar_creds.R")
setwd("~")

datashield.logout(opals)

myvars = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
           "AGE_BASE", "AGE_END_OBJ","MI", "STROKE", "CANCER", "HYPERTENSION", "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           "FAM_DIAB", "E_INTAKE", "FRUIT", "VEG", "DAIRY", "FIBER", "RED_MEAT" , "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS")

opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')

# # To include all possible variables uncomment this line and and comment out previus line
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish')

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
ds.subset(x = 'D', subset = 'E1', logicalOperator = 'PREV_DIAB<', threshold = 1)
noPrevalence <- ds.length('E1$SEX', type = 'split')
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB==', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# remove participants with too little and excessive consumption of calories
ds.subset(x = 'E2', subset = 'E3', logicalOperator = 'E_INTAKE<=', threshold = 3500)
under3500cal <- ds.length('E3$SEX', type = 'split')
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'E_INTAKE>=', threshold = 500)
afterIntake <- ds.length('E4$SEX', type = 'split')

# Setup an additional proxy ID column for each study 
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('ID','E4'), newobj='D2')

# adding in zero columns to the studies
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('newStartDate','D2'), newobj='D3')

ds.assign(toAssign = 'D3$AGE_END_OBJ-D3$AGE_BASE', newobj = 'newEndDate')
ds.cbind(x=c('newEndDate','D3'), newobj='D4')

# ###############################################################################
# ########################### FUNCTIONS  ########################################
# ###############################################################################
do_reg <- function(counter, my_fmla, study, outcome, out_family){
	# performs a regular regression and returns the coefficients of the fitted model as a dataframe 
	model <- ds.glm(formula = my_fmla, data = ref_table, family = out_family, datasources=opals[counter], maxit=100, checks=TRUE)
	model_coeffs <- as.data.frame(model$coefficients)
	model_coeffs$study = study
	model_coeffs$outcome = outcome
	model_coeffs$cov = rownames(model_coeffs)
	for (x in 1:3){ model_coeffs <- model_coeffs[,c(ncol(model_coeffs),1:(ncol(model_coeffs)-1))]}
	rownames(model_coeffs) = NULL
	return(model_coeffs)
}


do_reg_survival <- function(counter, my_fmla, study, outcome, out_family, offset_column, lexisTable){
	# performs a survival analysis using the formula on the appropiately lexised table
	# note that the coefficients returned as a dataframe are not exponentiated. this is done
	# as part of the do_rem process
	model <- ds.glm(formula = my_fmla, data = lexisTable, family = out_family, datasources=opals[counter], offset = offset_column,  maxit=100, checks=TRUE)
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
			.(round(res$QEp,3)),')')),
			xlab=bquote(paste('Test of H'[0]*': true mean association = 0, p = ',
			.(round(res$pval,3)))))
		usr <- par("usr")
		text(usr[2], usr[4], "Beta [95% CI]", adj = c(1, 4),cex=0.75)
		text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=0.75)
		text(usr[1], usr[3], variable, adj = c( 0, 0 ),cex=0.75)
	}
	else if (out_family == 'poisson'){
	  forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
	         .(round(res$QEp,3)),')')),
	         xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
	         .(round(res$pval,3)))), atransf = exp)
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

findOutcomeFamily <- function(ref_table, outcome){
	# Find outcome family for regression
	# used in the do_reg function
	out_class = ds.class(paste0(ref_table, '$', outcome))[[1]]
	if (out_class == 'factor') {
		outcome_family = 'binomial'
	}
	else if (out_class == 'numeric' | out_class == 'integer') {
		outcome_family = 'gaussian'
	}
	return(outcome_family)
}


runRegModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath){
	# main function that runs, fits, and stores the results of a regression model using the 
	# datashield process 
	REM_results = list()
	study_regs = data.frame()

	svg(filename=mypath, 
		width=4 * length(my_exposure), 
		height=3 * length(my_outcome), 
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

			for(i in 1:length(opals)) {
				reg_data <- data.frame()

				fmla <- as.formula(paste(ref_table, '$', my_outcome[k]," ~ ", paste0(c(paste0(ref_table, '$',my_exposure[j]), paste0(ref_table, '$',my_covariate)), collapse= "+")))
				reg_data <- do_reg(i,fmla, names(opals[i]), my_outcome[k], outcome_family)

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

runSurvivalModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath) {
	# main function that runs, fits, and stores the results of a survival model using the 
	# normal lexis function to expand the dataframe. Note that this model is not recommended for the 
	# fish analysis and used to test the functionality of lexis against that of lexisB
	# There is no rebase of start and end times in this function
	study_regs = data.frame()

	svg(filename=mypath, 
		width=4 * length(my_exposure), 
		height=3 * length(my_outcome), 
		pointsize=10)
	par(mar=c(5,3,2,2)+0.1)
	par(mfrow=c(length(my_outcome),length(my_exposure)))
	par(ps=10)

	## attempt with ds.lexis, ie the poisson piecewise regression
	ds.lexis(data=ref_table, idCol='ID', entryCol='AGE_BASE', exitCol='AGE_END_OBJ', statusCol='CASE_OBJ', newobj = "A", datasources = opals)
	# set TIMEID as a factor as its not done automatically
	ds.asFactor(x = 'A$TIMEID', newobj = 'TIMEIDFACT')
	ds.cbind(x=c('TIMEIDFACT', 'A'), newobj = 'Afact')
	ds.assign(toAssign='log(Afact$SURVIVALTIME)', newobj='logSurvival')

	lexised_table = "Afact"

	for (k in 1:length(my_outcome)){
		outcome_family = 'poisson'

		# for each exposure and
		for (j in 1:length(my_exposure)){
			estimates = vector()
			s_errors = vector()
			labels = vector()

			for(i in 1:length(opals)) {
				reg_data <- data.frame()

				fmla <- as.formula(paste(lexised_table, '$', my_outcome[k]," ~ ", '0', '+', paste0(lexised_table, '$','TIMEID'), '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
				reg_data <- do_reg_survival(i, my_fmla = fmla, study =  names(opals[i]), outcome =  my_outcome[k],  out_family = "poisson", offset_column = "logSurvival", lexisTable = lexised_table)

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

runSurvival_B_Model <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, interval_width) {
	# main function that runs, fits, and stores the results of a survival model using the 
	# lexisB function to expand the dataframe and also rebases the start and endtimes of the data variables.
	REM_results = list()
	study_regs = data.frame()

	svg(filename=mypath, 
		width=4 * length(my_exposure), 
		height=3 * length(my_outcome), 
		pointsize=10)
	par(mar=c(5,3,2,2)+0.1)
	par(mfrow=c(length(my_outcome),length(my_exposure)))
	par(ps=10)

	# assign Danger.NFILTER to some values as the current code in the dsBeta doesnt work without this.
	# and expand the dataset using the ds.lexis b command
	datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(1)), opals = opals)
	datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(1)), opals = opals)
	idColString = paste0(ref_table, '$', 'ID')
	entryColString = paste0(ref_table, '$', 'newStartDate')
	exitColString = paste0(ref_table, '$', 'newEndDate')
	statusColString = paste0(ref_table, '$', 'CASE_OBJ')
	ds.lexis.b(data=ref_table, intervalWidth = interval_width, idCol = idColString, entryCol = entryColString, 
	           exitCol = exitColString, statusCol = statusColString, expandDF = 'A')
	
	ds.asNumeric('A$CENSOR','censor')
	ds.asFactor('A$TIME.PERIOD','tid.f')
	ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')
	lexised_table = 'A'

	for (k in 1:length(my_outcome)){
	  
		# for each exposure and
		for (j in 1:length(my_exposure)){
			estimates = vector()
			s_errors = vector()
			labels = vector()

			for(i in 1:length(opals)) {
				reg_data <- data.frame()

				# need to check this formula for correctness
				fmla <- as.formula(paste(lexised_table, '$', my_outcome[k]," ~ ", '0', '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
				fmla <- as.formula(paste("censor"," ~ ", '0', '+', 'tid.f', '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
				reg_data <- do_reg_survival(i, my_fmla = fmla, study = names(opals[i]), outcome =  my_outcome[k],  out_family = "poisson", offset_column = "logSurvivalA", lexisTable = lexised_table)
				study_regs = rbind(study_regs,reg_data)
				estimates = rbind(estimates,reg_data[grep(my_exposure[j], reg_data$cov),"Estimate"])
				s_errors = rbind(s_errors,reg_data[grep(my_exposure[j], reg_data$cov),"Std. Error"])
				labels = rbind(labels, reg_data[2,1])      
				variables = reg_data[grep(my_exposure[j], reg_data$cov), 'cov']
			}

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

runIncrementalSurvivalModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath_prefix, interval_width){
	# Runs survival models incrementally through a list of provided covariates, producing randomeffectmodels and therein
	# forest plots along the way. Mainly used for exploratory purposes.
	REM_results = list()
	study_regs = data.frame()
	overall_df = data.frame()
	for (i in 1:length(my_covariate)){
		mypath_func = file.path(paste0(mypath_prefix, "_", i, "outta", length(my_covariate), ".svg"))
		sub_covariate_list = my_covariate[1:i]
		runResults = runSurvival_B_Model(ref_table, my_exposure, my_outcome, sub_covariate_list, mypath_func, c(2,2,2,2,2,2,2,2,2,2))
		runCoeffs = runResults[[1]]
		overall_df = rbind(overall_df, runCoeffs)
		print(mypath_func)
	}
	return(overall_df)
}

runMediationModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath_prefix, interval_width, my_mediation) {
	# Runs a mediation survival model, with the provided extra mediations.
	# ie. it does the following:
	# 1. it runs the simple relationship outcome~exposure + covariates
	# 2. it runs the relationship mediator~exposure + covariates
	# 3. it runs the relationship outcome~mediator + covariates
	# 4. it runs the relationship outcome~exposure + covariates + mediator
	# make sure my_mediation is a vector with the string mediator

	# outcome~exposure + covariates
	mypath_1 = file.path(paste0(mypath_prefix, "_", "part_1", ".svg"))
	mediate1 = runSurvival_B_Model(ref_table, my_exposure, my_outcome, my_covariate, mypath_1, interval_width)

	# mediator~exposure + covariates
	mypath_2 = file.path(paste0(mypath_prefix, "_", "part_2", ".svg"))
	mediate2 = runSurvival_B_Model(ref_table, my_exposure, my_mediation, my_covariate, mypath_2, interval_width)
	
	# outcome~mediator + covariates
	mypath_3 = file.path(paste0(mypath_prefix, "_", "part_3", ".svg"))
	mediate3 = runSurvival_B_Model(ref_table, my_mediation, my_mediation, my_covariate, mypath_3, interval_width)

	# outcome~exposure + covariates + mediator
	my_covariate = c(my_covariate, my_mediation)
	mypath_4 = file.path(paste0(mypath_prefix, "_", "part_4", ".svg"))
	mediate4 = runSurvival_B_Model(ref_table, my_exposure, my_outcome, my_covariate, mypath_4, interval_width)

	return(list(mediate1, mediate2, mediate3, mediate4))
}

runStratificationModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, interval_width, stratified_var) {
	# Runs a stratified survival model given the factored variable to be stratified
	# could possibly be better implemented as a simple survival model where the input dataframe "ref_table" is
	# instead just subset before hand
	return(NULL)
}

# ___  ___          _      _   __  
# |  \/  |         | |    | | /  | 
# | .  . | ___   __| | ___| | `| | 
# | |\/| |/ _ \ / _` |/ _ \ |  | | 
# | |  | | (_) | (_| |  __/ | _| |_
# \_|  |_/\___/ \__,_|\___|_| \___/
                                 
# Exposure: total fish (g/d) at baseline
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, family history of diabetes, MI, stroke, cancer, hypertension
# 
# To assess the impact of each confounder we will also run models including each confounder separately.
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
# my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "HYPERTENSION")  
# my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA", "STROKE", "MI","HYPERTENSION")
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING")
ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_1.svg')

model_1_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath)
model_1_all = model_1_results[[1]]
model_1_REM = model_1_results[[2]]

# # survival version short running normal lexis
# ref_table = 'D4'
# mypath = file.path('~', 'plots', 'model_1_surv.svg')
# model_1_surv = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath)
# model_1_surv_all = model_1_surv[[1]]
# model_1_surv_all = model_1_surv[[2]]

# survival version with lexis b 
ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_1b_surv.svg')
model_1_b = runSurvival_B_Model(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,2,2,2,2,2,2,2,2))
model_1_b_all = model_1_b[[1]]
model_1_b_rem = model_1_b[[2]]


ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_1b_inc')
model_1_inc = runIncrementalSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,2,2,2,2,2,2,2,2))

ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_1b_mediate')
model_1_mediated = runMediationModel(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(2,2,2,2,2,2,2,2,2,2), c("PA"))


# ___  ___          _      _   _____ 
# |  \/  |         | |    | | / __  \
# | .  . | ___   __| | ___| | `' / /'
# | |\/| |/ _ \ / _` |/ _ \ |   / /  
# | |  | | (_) | (_| |  __/ | ./ /___
# \_|  |_/\___/ \__,_|\___|_| \_____/
# Model 2a: As model 1 + adj for alcohol intake, fibre intake, processed meat intake, fruit and vegetables intake, sugary drinks intake, fish oil supplements
# Model 2b: As model 2a + adj for energy intake

# model2a
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION",
				"ALCOHOL", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS")

# model2b
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION",
				"ALCOHOL", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "E_INTAKE")


# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |____ |
# | .  . | ___   __| | ___| |     / /
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | .___/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 
# Model 3: As model 2b + adj for BMI,  
# Sensitivity analyses: include waist circumference or waist to hip ratio
# 
# Models to test Interaction 
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION",
				"ALCOHOL", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "E_INTAKE", "BMI")

# interaction with waist
my_exposure = c('TOTAL')
my_outcome = c('WAIST')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION",
				"ALCOHOL", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "E_INTAKE", "BMI")


my_exposure = c('WAIST')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION",
				"ALCOHOL", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "E_INTAKE", "BMI")


# ___  ___          _      _     ___ 
# |  \/  |         | |    | |   /   |
# | .  . | ___   __| | ___| |  / /| |
# | |\/| |/ _ \ / _` |/ _ \ | / /_| |
# | |  | | (_) | (_| |  __/ | \___  |
# \_|  |_/\___/ \__,_|\___|_|     |_/
# Exposure: total fish (g/d) at baseline*sex
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, family history of diabetes, MI, stroke, cancer, hypertension,  energy intake, fibre intake, processed meat intake, fruit and vegetables intake, sugary drinks intake, fish oil supplements, BMI
# 
# Stratified analyses by sex (men, women) if positive interaction 
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION",
				"ALCOHOL", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "BMI")


# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |  ___|
# | .  . | ___   __| | ___| | |___ \ 
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | /\__/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 
# Exposure: total fish (g/d) at baseline*BMI
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, family history of diabetes, MI, stroke, cancer, hypertension, medications for hypertension, energy intake, fibre intake, processed meat intake, fruit and vegetables intake, sugary drinks intake, fish oil supplements
# 
# Stratified analyses by BMI (BMI<25, BMI ≥25) if positive interaction

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
# medication is new here
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION","MEDS",
				"E_INTAKE", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "BMI")


# ___  ___          _      _    ____ 
# |  \/  |         | |    | |  / ___|
# | .  . | ___   __| | ___| | / /___ 
# | |\/| |/ _ \ / _` |/ _ \ | | ___ \
# | |  | | (_) | (_| |  __/ | | \_/ |
# \_|  |_/\___/ \__,_|\___|_| \_____/
# Exposure: total fish (g/d) at baseline*geographical area
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, family history of diabetes, MI, stroke, cancer, hypertension, medications for hypertension, energy intake, fibre intake, processed meat intake, fruit and vegetables intake, sugary drinks intake, fish oil supplements, BMI
# 
# Stratified analyses by geographical area (Central area, Eastern area, Western area) if positive interaction 

my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION","MEDS",
				"E_INTAKE", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "BMI")

# GEOGRAPHIC AREA (BUT MIGHT NOT DO THIS ONE ANYWAY)

# repeat for each exposure
my_exposure = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED")
my_outcome = c('CASE_OBJ', "CASE_OBJ_SELF")
my_covariate = c("AGE_BASE", "AGE_END","MI", "STROKE", "HYPERTENSION", "SEX", "BMI", "GEOG_AREA", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
	"FAM_DIAB", "E_INTAKE", "FRUIT", "VEG", "DAIRY", "FIBER", "RED_MEAT" , "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST",
	"SUPPLEMENTS")

