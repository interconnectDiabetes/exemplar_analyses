[1mdiff --git a/fish_exemplar/ds.glmDebug.R b/fish_exemplar/ds.glmDebug.R[m
[1mindex 2f70ef2..a91fec8 100644[m
[1m--- a/fish_exemplar/ds.glmDebug.R[m
[1m+++ b/fish_exemplar/ds.glmDebug.R[m
[36m@@ -1,5 +1,5 @@[m
 ## Alternative ds.glm function[m
[31m-## Author: Paul Scherer[m
[32m+[m[32m## Author: Gaye, A., Paul Scherer[m
 ## Date: 08/08/2017[m
 [m
 ds.glmDebug <- function(formula=NULL, data=NULL, family=NULL, offset=NULL, weights=NULL, checks=FALSE, maxit=15, CI=0.95, viewIter=FALSE, datasources=NULL) {[m
[1mdiff --git a/fish_exemplar/survival_analysis_dsFunctions.R b/fish_exemplar/survival_analysis_dsFunctions.R[m
[1mindex 8b297b8..0d8fe79 100644[m
[1m--- a/fish_exemplar/survival_analysis_dsFunctions.R[m
[1m+++ b/fish_exemplar/survival_analysis_dsFunctions.R[m
[36m@@ -169,23 +169,27 @@[m [mcreateModelFormula <- function(studyName, data_table, outcome, exposure, covaria[m
 	}[m
 }[m
 [m
[32m+[m
 checkFactoredTime <- function(timeVariable = 'tid.f', studies = opals){[m
 	# checks a factored time to censor representation typically tid.f for any zero valued levels[m
 	# as this can be used as an indicator for singular matrices to come at the regression stage[m
 	# start check with isValid [m
 	isValidList = ds.isValid(timeVariable, datasources = studies)[m
[32m+[m
 	if (all(isValidList)) { [m
 		# even if all the time variables are valid according to datashield, there may be levels with[m
 		# 0 participants in the factored version of the time variable[m
 		for (study in studies) {[m
 			timeVariableSummary = ds.summary(timeVariable, datasources = study)[m
 			timeVariableSummary = (timeVariableSummary[[1]])[c(4:length(timeVariableSummary[[1]]))][m
[32m+[m[41m			[m
 			for (i in timeVariableSummary) {[m
 				if (i == 0){[m
 					print(ds.summary('tid.f', datasources = studies[study]))[m
 					stop("One of the levels in the timeVariable has zero members in it!")[m
 				}[m
 			}[m
[32m+[m[41m			[m
 			return(TRUE)[m
 		}[m
 	} else {[m
[36m@@ -194,6 +198,7 @@[m [mcheckFactoredTime <- function(timeVariable = 'tid.f', studies = opals){[m
 	}[m
 }[m
 [m
[32m+[m
 runRegModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, studies = opals){[m
 	# main function that runs, fits, and stores the results of a regression model using the [m
 	# datashield process[m
[36m@@ -256,6 +261,7 @@[m [mrunRegModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath[m
 	return (list(model_all, model_rem))[m
 }[m
 [m
[32m+[m
 tunedLexisB <- function(ref_table, study) {[m
 	# does a tuned Lexis B on one study can be used in a parallel loop to run[m
 	# multiple lexis b at once[m
[36m@@ -349,6 +355,7 @@[m [mtunedSurvivalModel <- function(ref_table, my_exposure, my_outcome, my_covariate,[m
 	return (list(model_all, model_rem))[m
 }[m
 [m
[32m+[m
 runSurvivalModel <- function(ref_table, my_exposure, my_outcome, my_covariate, mypath, interval_width, studies = opals) {[m
 	# main function that runs, fits, and stores the results of a survival model using the [m
 	# lexisB function to expand the dataframe and also rebases the start and endtimes of the data variables.[m
[36m@@ -575,6 +582,7 @@[m [mrunStratificationModel <- function(ref_table, my_exposure, my_outcome, my_covari[m
 	return(list_of_models)[m
 }[m
 [m
[31m-metaRegression <- function(outcome, exposure,) {[m
[32m+[m
[32m+[m[32mmetaRegression <- function(outcome, exposure) {[m
 	return (NULL)[m
 }[m
\ No newline at end of file[m
