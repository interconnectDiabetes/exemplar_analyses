## Analysis script for first trimester PAIP analysis
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

myvars = list('AGE_BASE', 'FATTY', 'FRESH', 'FRIED', 'LEAN', 'NONFISH', 'SALT', 'SSD', 'TOTAL', 'MI', 'CANCER', 'STROKE', 'HYPERTENSION',
              'TYPE_DIAB', 'PREV_DIAB','CASE_OBJ', "CASE_OBJ_SELF", "AGE_END")
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')

# # To include all possible variables uncomment this line and and comment out previus line
# opals <- datashield.login(logins=logindata_all,assign=TRUE, directory = '/home/shared/certificates/fish')

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# all participants
all_participants <- ds.length('D$TOTAL')
all_participants_split <- ds.length('D$TOTAL',type = 'split')

# Filter out missing values
temp <- ds.summary('D$TOTAL')
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

# Only Complete Cases
ds.subset(x = 'D', subset = 'D1', completeCases = TRUE)
complete_participants <- ds.length('D1$TOTAL')
complete_participants_split <- ds.length('D1$TOTAL',type = 'split')

# Setup an additional proxy ID column for each study
for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('ID','D'), newobj='D2')

###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
summaryContExp <- function(column, study_names, num_studies) {
    # given a table$column combination as a string, return the summary table 
    # for the continous variable
    summary_column_temp = ds.summary(column)
    summary_column = data.frame(matrix(unlist(summary_column_temp), nrow = num_studies, ncol=10, byrow=TRUE))
    rownames(summary_column) = study_names
    colnames(summary_column) = c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
    summary_column = summary_column[,c(2,6,5,7)]
    rm(summary_column_temp)
    return(summary_column)
}

summaryBinExp <- function(column, study_names, num_studies) {
    # given a table$column combination as a string, return the summary
    # table for the binary variable
    summary_column_temp = ds.summary(column)
    summary_column = data.frame(matrix(unlist(summary_column_temp), nrow = num_studies, ncol=6, byrow=TRUE))
    rownames(summary_column) <- study_names
    colnames(summary_column) <- c('type', 'n', '0', '1', 'No', 'Yes')
    rm(summary_column_temp)
    return(summary_column)
}

summaryCatExp <- function (column, study_names, num_studies, levels = 2){
	# given a table$column combination as a string, return the overall summary for categorical
	# variables set in the levels parameter.
	summary_column_temp = ds.summary(column)
	summary_column = data.frame(matrix(unlist(summary_column_temp), nrow=num_studies, ncol=(2+(2*levels)), byrow = TRUE))
	rownames(summary_column) <- study_names
	colnames(summary_column) = c('type', 'n')
	rm(summary_column_temp)
	return(summary_column)
}

# Exposures Missing Checker
fullNum = ds.length('D$AGE_BASE', type = 'split') 
fattyMissing =  ds.numNA('D$FATTY')
freshMissing = ds.numNA('D$FRESH')
friedMissing = ds.numNA('D$FRIED')
leanMissing = ds.numNA('D$LEAN')
nonfishMissing = ds.numNA('D$NONFISH')
saltMissing = ds.numNA('D$SALT')
ssdMissing = ds.numNA('D$SSD')
totalMissing = ds.numNA('D$TOTAL')

exposure_missings_table = data.frame(cbind(study_names,fullNum, fattyMissing, freshMissing, friedMissing, leanMissing, nonfishMissing, saltMissing, ssdMissing, totalMissing))
colnames(exposure_missings_table) <- c('Study Name', 'Total in Study', 'fattyMissing', 'freshMissing', 'friedMissing', 'leanMissing', 'nonfishMissing', 'saltMissing', 'ssdMissing', 'totalMissing')

# Confounders Missing Checker
miMissing = ds.numNA('D$MI')
strokeMissing = ds.numNA('D$STROKE')
cancerMissing = ds.numNA('D$CANCER')
hypertensionMissing = ds.numNA('D$HYPERTENSION')

conf_missings_table = data.frame(cbind(study_names, fullNum, miMissing, cancerMissing, strokeMissing, hypertensionMissing))
colnames(conf_missings_table) <- c('Study Name', 'Total in Study', 'miMissing', 'cancerMissing', 'strokeMissing', 'hypertensionMissing')

#---------------------------------------------------------
# Summaries for exposures
# fatty fish
# summary_fatty = summaryContExp('D$FATTY', study_names, num_studies)
# fresh fish
# summary_fresh = summaryContExp('D$FRESH', study_names, num_studies)
# fried fish
# summary_fried = summaryContExp('D$FRIED', study_names, num_studies)
# lean fish
# summary_lean = summaryContExp('D$LEAN', study_names, num_studies)
# nonfish
# summary_nonfish = summaryContExp('D$NONFISH', study_names, num_studies)
# salt fish
# summary_salt = summaryContExp('D$SALT', study_names, num_studies)
# ssd fish
# summary_ssd = summaryContExp('D$SSD', study_names, num_studies)
# total fish
# summary_total = summaryContExp('D$TOTAL', study_names, num_studies)

                               
#---------------------------------------------------------
# Summaries for outcomes


#---------------------------------------------------------
# Summaries for covariates and confounders
# education

# ses

# # smoking
# summaryBinExp('D$SMOKING', study_names, num_studies)

# # mi, stroke, cancer, hypertension
# summaryBinExp('D$MI', study_names, num_studies)
# summaryBinExp('D$STROKE', study_names, num_studies)
# summaryBinExp('D$CANCER', study_names, num_studies)
# summaryBinExp('D$HYPERTENSION', study_names, num_studies)

# # Continous covariates
# summary_pa = summaryContExp('D$PA', study_names, num_studies)
# summary_alc = summaryContExp('D$ALCOHOL', study_names, num_studies)
# summary_supplements = summaryContExp('D$SUPPLEMENTS', study_names, num_studies)
# summary_eintake = summaryContExp('D$E_INTAKE', study_names, num_studies)
# summary_red_meat = summaryContExp('D$RED_MEAT', study_names, num_studies)
# summary_proc_meat = summaryContExp('D$PROC_MEAT', study_names, num_studies)
# summary_fruit = summaryContExp('D$FRUIT', study_names, num_studies)
# summary_veg = summaryContExp('D$VEG', study_names, num_studies)
# summary_dairy = summaryContExp('D$DAIRY', study_names, num_studies)
# summary_fiber = summaryContExp('D$FIBER', study_names, num_studies)
# summary_sugardrinks = summaryContExp('D$SUG_BEVS', study_names, num_studies)

# ###############################################################################
# ########################### FUNCTIONS  ########################################
# ###############################################################################
do_reg <- function(my_fmla, study, outcome, out_family){
	model <- ds.glm(formula = my_fmla, data = ref_table, family = out_family, datasources=opals[i], maxit=100, checks=TRUE)
	model_coeffs <- as.data.frame(model$coefficients)
	model_coeffs$study = study
	model_coeffs$outcome = outcome
	model_coeffs$cov = rownames(model_coeffs)
	for (x in 1:3){ model_coeffs <- model_coeffs[,c(ncol(model_coeffs),1:(ncol(model_coeffs)-1))]}
	rownames(model_coeffs) = NULL
	return(model_coeffs)
}

do_reg_survival <- function(my_fmla, study, outcome, out_family, offset_column, lexisTable){
	model <- ds.glm(formula = my_fmla, data = lexisTable, family = out_family, datasources=opals[i], offset = offset_column,  maxit=100, checks=TRUE)
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
	else if (out_family == 'poisson'){
		forest(res, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
			.(round(res$QEp,3)),')')),
			xlab=bquote(paste('Test of H'[0]*': true hazard ratio = 1, p = ',
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

findOutcomeFamily <- function(ref_table, outcome){
	# Find outcome family for regression
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
				reg_data <- do_reg(fmla, names(opals[i]), my_outcome[k], outcome_family)

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
	REM_results = list()
	study_regs = data.frame()

	svg(filename=mypath, 
		width=4 * length(my_exposure), 
		height=3 * length(my_outcome), 
		pointsize=10)
	par(mar=c(5,3,2,2)+0.1)
	par(mfrow=c(length(my_outcome),length(my_exposure)))
	par(ps=10)

	## attempt with ds.lexis, ie the poisson piecewise regression
	ds.lexis(data=ref_table, idCol='ID', entryCol='AGE_BASE', exitCol='AGE_END', statusCol='CASE_OBJ', newobj = "A", datasources = opals)
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

				fmla <- as.formula(paste(lexised_table, '$', my_outcome[k]," ~ ", '1', '+', paste0(lexised_table, '$','TIMEID'), '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
				reg_data <- do_reg_survival(my_fmla = fmla, study =  names(opals[i]), outcome =  my_outcome[k],  out_family = outcome_family, offset_column = "logSurvival", lexisTable = lexised_table)

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
	entryColString = paste0(ref_table, '$', 'AGE_BASE')
	exitColString = paste0(ref_table, '$', 'AGE_END')
	statusColString = paste0(ref_table, '$', 'CASE_OBJ')
	ds.lexis.b(data=ref_table, intervalWidth = interval_width, idCol = idColString, entryCol = entryColString, 
	           exitCol = exitColString, statusCol = statusColString, expandDF = 'A')
	
	ds.asNumeric('A$CENSOR','censor')
	ds.asFactor('A$TIME.PERIOD','tid.f')
	ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')
	lexised_table = 'A'

	for (k in 1:length(my_outcome)){
		outcome_family = 'poisson'

		# for each exposure and
		for (j in 1:length(my_exposure)){
			estimates = vector()
			s_errors = vector()
			labels = vector()

			for(i in 1:length(opals)) {
				reg_data <- data.frame()

				# need to check this formula for correctness
				fmla <- as.formula(paste(lexised_table, '$', my_outcome[k]," ~ ", '1', '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
				reg_data <- do_reg_survival(my_fmla = fmla, study =  names(opals[i]), outcome =  my_outcome[k],  out_family = outcome_family, offset_column = "logSurvival", lexisTable = lexised_table)

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

# +-+-+-+-+-+ +-+
#   |m|o|d|e|l| |1|
#   +-+-+-+-+-+ +-+
# Exposure: total fish (g/d) at baseline
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, family history of diabetes, MI, stroke, cancer, hypertension
# 
# To assess the impact of each confounder we will also run models including each confounder separately.
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
# my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION")
my_covariate =  c("STROKE", "HYPERTENSION")

ref_table = 'D2'
mypath = file.path('~', 'plots', 'model_1.svg')

model_1_results = runRegModel(ref_table, my_exposure, my_outcome, my_covariate, mypath)
model_1_all = model_1_results[[1]]
model_1_REM = model_1_results[[2]]

# ## attempt with ds.lexis, ie the poisson piecewise regression
# ds.lexis(data='D2', idCol='ID', entryCol='AGE_BASE', exitCol='AGE_END', statusCol='CASE_OBJ', newobj = "d1_expanded", datasources = opals)
# ds.assign(toAssign='log(d1_expanded$SURVIVALTIME)', newobj='logSurvival')
# ds.glm(formula='d1_expanded$CASE_OBJ~1+ d1_expanded$TIMEID+d1_expanded$AGE_END+d1_expanded$TOTAL', data='d1_expanded',family='poisson',offset='logSurvival')

# datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(1)), opals = opals)
# datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(1)), opals = opals)
# 
# 
# ds.lexis.b(data='D2', intervalWidth=c(1,2,3), idCol='D2$ID', entryCol='D2$AGE_BASE', exitCol='D2$AGE_END', statusCol='D2$CASE_OBJ', expandDF = 'A')
# ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')
# 
# ds.asNumeric('A$AGE_BASE','age')
# ds.asNumeric('A$CENSOR','censor')
# ds.asNumeric('A$TOTAL', 'total')
# ds.asFactor('A$TIME.PERIOD','tid.f')
# 
# ds.glm(formula='censor~1+tid.f+age+total',family='poisson',offset='logSurvival')

# # test purposes
# ds.lexis.b(data='D', intervalWidth=c(1,2,3), idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor', expandDF = 'A')
# ds.lexis(data='D', idCol='ID', entryCol='entdate', exitCol='enddate', statusCol='censor', newobj = "d1_expanded", datasources = opals)
# 


# survival version short (case usage)
ref_table = 'D2'
mypath = file.path('~', 'plots', 'model_1_surv.svg')
model_1_surv = runSurvivalModel(ref_table, my_exposure, my_outcome, my_covariate, mypath)
model_1_surv_all = model_1_surv[[1]]
model_1_surv_all = model_1_surv[[2]]

# survival version with lexis b (case usage)
ref_table = 'D2'
mypath = file.path('~', 'plots', 'model_1b_surv.svg')
model_1_b = runSurvival_B_Model(ref_table, my_exposure, my_outcome, my_covariate, mypath, c(1,2,3))
model_1_b_all = model_1_b[[1]]
model_1_b_all = model_1_b[[2]]

# +-+-+-+-+-+ +-+
#   |m|o|d|e|l| |2|
#   +-+-+-+-+-+ +-+
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

# +-+-+-+-+-+ +-+
#   |m|o|d|e|l| |3|
#   +-+-+-+-+-+ +-+
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

# +-+-+-+-+-+ +-+
#   |m|o|d|e|l| |4|
#   +-+-+-+-+-+ +-+
# Exposure: total fish (g/d) at baseline*sex
# Outcome: Type 2 diabetes incidence
# Confounders: Age, sex, education, smoking, physical activity, family history of diabetes, MI, stroke, cancer, hypertension,  energy intake, fibre intake, processed meat intake, fruit and vegetables intake, sugary drinks intake, fish oil supplements, BMI
# 
# Stratified analyses by sex (men, women) if positive interaction 
my_exposure = c('TOTAL')
my_outcome = c('CASE_OBJ')
my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "FAM_DIAB", "MI", "STROKE", "CANCER", "HYPERTENSION",
				"ALCOHOL", "FIBER", "PROC_MEAT", "FRUIT", "VEG", "SUG_BEVS", "SUPPLEMENTS", "BMI")

# +-+-+-+-+-+ +-+
#   |m|o|d|e|l| |5|
#   +-+-+-+-+-+ +-+
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

# +-+-+-+-+-+ +-+
#   |m|o|d|e|l| |6|
#   +-+-+-+-+-+ +-+
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

