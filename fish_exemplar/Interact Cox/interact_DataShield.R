## ## Analysis script for Fish Exemplar Analysis
## Author: Paul Scherer
##		   Tom Bishop
## Date: 18/0062017

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

datashield.logout(opals)

myvars = c('TOTAL', 'NONFISH', 'FRESH', 'LEAN', 'FATTY', "SALT", "SSD", "FRIED", 'CASE_OBJ', "CASE_OBJ_SELF", "PREV_DIAB", "TYPE_DIAB", 
           "AGE_BASE", "AGE_END_OBJ","MI", "STROKE", "CANCER", "HYPERTENSION", "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL",
           "FAM_DIAB", "E_INTAKE", "FRUIT", "VEG", "DAIRY", "FIBER", "RED_MEAT" , "PROC_MEAT", "SUG_BEVS", "MEDS", "WAIST", "SUPPLEMENTS")

opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')

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

# Setup an additional proxy ID column for each study 
for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('ID','D'), newobj='D2')

# adding in zero columns to the studies
for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
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
				fmla <- as.formula(paste("censor"," ~ ", 'tid.f', '+', paste0(c(paste0(lexised_table, '$',my_exposure[j]), paste0(lexised_table, '$',my_covariate)), collapse= "+")))
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
my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING")

# survival version with lexis b 
ref_table = 'D4'
mypath = file.path('~', 'plots', 'model_1b_surv.svg')
model_1_b = runSurvival_B_Model(ref_table, my_exposure, my_outcome, my_covariate, mypath, 10)
model_1_b_all = model_1_b[[1]]
model_1_b_rem = model_1_b[[2]]