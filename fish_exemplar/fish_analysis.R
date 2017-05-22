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

myvars = list('AGE_BASE', 'FATTY', 'FRESH', 'FRIED', 'LEAN', 'NONFISH', 'SALT', 'SSD', 'TOTAL')
opals <- datashield.login(logins=logindata_all, assign=TRUE, variables =myvars, directory = '/home/shared/certificates/fish')


###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# all participants
all_participants <- ds.length('D$TOTAL')

# Filter out missing values
temp <- ds.summary('D$TOTAL')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

# # Only Complete Cases
# ds.subset(x = 'D', subset = 'D1', completeCases = TRUE)
# complete_participants <- ds.length('D1$TOTAL')

###############################################################################
########################### DATA SUMMARIES ####################################
###############################################################################
# Missing Checker
fullNum = ds.length('D$AGE_BASE', type = 'split') 
fattyMissing =  ds.numNA('D$FATTY')
freshMissing = ds.numNA('D$FRESH')
friedMissing = ds.numNA('D$FRIED')
leanMissing = ds.numNA('D$LEAN')
nonfishMissing = ds.numNA('D$NONFISH')
saltMissing = ds.numNA('D$SALT')
ssdMissing = ds.numNA('D$SSD')
totalMissing = ds.numNA('D$TOTAL')

missings_table = data.frame(cbind(study_names,fullNum, fattyMissing, freshMissing, friedMissing, leanMissing, nonfishMissing, saltMissing, ssdMissing, totalMissing))
colnames(missings_table) <- c('Study Name', 'Total in Study', 'fattyMissing', 'freshMissing', 'friedMissing', 'leanMissing', 'nonfishMissing', 'saltMissing', 'ssdMissing', 'totalMissing')
#---------------------------------------------------------
# Summaries for exposures 
# fatty fish
summary_fatty_temp <- ds.summary('D$FATTY')
summary_fatty <- data.frame(matrix(unlist(summary_fatty_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_fatty) <- study_names
colnames(summary_fatty) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_fatty <- summary_fatty[,c(2,6,5,7)]
rm(summary_fatty_temp)

# fresh fish
summary_fresh_temp <- ds.summary('D$NONFISH')
summary_fresh <- data.frame(matrix(unlist(summary_fresh_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_fresh) <- study_names
colnames(summary_fresh) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_fresh <- summary_fresh[,c(2,6,5,7)]
rm(summary_fresh_temp)

# lean fish
summary_lean_temp <- ds.summary('D$NONFISH')
sumamry_lean <- data.frame(matrix(unlist(summary_lean_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(sumamry_lean) <- study_names
colnames(sumamry_lean) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
sumamry_lean <- sumamry_lean[,c(2,6,5,7)]
rm(summary_lean_temp)

# nonfish
summary_non_fish_temp <- ds.summary('D$NONFISH')
summary_nonfish <- data.frame(matrix(unlist(summary_non_fish_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_nonfish) <- study_names
colnames(summary_nonfish) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_nonfish <- summary_nonfish[,c(2,6,5,7)]
rm(summary_non_fish_temp)

# total fish
summary_total_temp <- ds.summary('D$TOTAL')
summary_total <- data.frame(matrix(unlist(summary_total_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_total) <- study_names
colnames(summary_total) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_total <- summary_total[,c(2,6,5,7)]
rm(summary_total_temp)


#---------------------------------------------------------
# Summaries for outcomes


#---------------------------------------------------------
# Summaries for covariates and confounders
# education

# ses

# smoking, mi, stroke, cancer, hypertension
smoking_temp <- c('SMOKING', 'MI', 'STROKE', 'CANCER', 'HYPERTENSION')
smoking_df <- data.frame()
for (bin in smoking_temp) {
  summary_temp <- ds.summary(paste0('E4$',bin))
  summary_temp <- data.frame(matrix(unlist(summary_temp), nrow = num_studies, ncol=6, byrow=TRUE))
  rownames(summary_temp) <- paste0(study_names,'_',bin)
  smoking_df <- rbind(smoking_df, summary_temp)
}
colnames(smoking_df) <- c('type', 'n', '0', '1', 'No', 'Yes')
smoking_df <- smoking_df[,c(5,6)]
rm(summary_temp)

# pa

#alcohol

# supplements

# eintake
summary_eintake_temp <- ds.summary('D$E_INTAKE')
summary_eintake <- data.frame(matrix(unlist(summary_eintake_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_eintake) <- study_names
colnames(summary_eintake) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_eintake <- summary_eintake[,c(2,6,5,7)]
rm(summary_eintake_temp)

# meat
summary_meat_temp <- ds.summary('D$MEAT')
summary_meat <- data.frame(matrix(unlist(summary_meat_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_meat) <- study_names
colnames(summary_meat) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_meat <- summary_meat[,c(2,6,5,7)]
rm(summary_meat_temp)

# fruit
summary_fruit_temp <- ds.summary('D$FRUIT')
summary_fruit <- data.frame(matrix(unlist(summary_fruit_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_fruit) <- study_names
colnames(summary_fruit) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_fruit <- summary_fruit[,c(2,6,5,7)]
rm(summary_fruit_temp)

# veg
summary_veg_temp <- ds.summary('D$VEG')
summary_veg <- data.frame(matrix(unlist(summary_veg_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_veg) <- study_names
colnames(summary_veg) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_veg <- summary_veg[,c(2,6,5,7)]
rm(summary_veg_temp)

# dairy
summary_dairy_temp <- ds.summary('D$DAIRY')
summary_dairy <- data.frame(matrix(unlist(summary_dairy_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_dairy) <- study_names
colnames(summary_dairy) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_dairy <- summary_dairy[,c(2,6,5,7)]
rm(summary_dairy_temp)

# fiber
summary_fiber_temp <- ds.summary('D$FIBER')
summary_fiber <- data.frame(matrix(unlist(summary_fiber_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_fiber) <- study_names
colnames(summary_fiber) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_fiber <- summary_fiber[,c(2,6,5,7)]
rm(summary_fiber_temp)

# sugary drinks
summary_sugBev_temp <- ds.summary('D$SUG_BEVS')
summary_sug_bevs <- data.frame(matrix(unlist(summary_sugBev_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_sug_bevs) <- study_names
colnames(summary_sug_bevs) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_sug_bevs <- summary_sug_bevs[,c(2,6,5,7)]
rm(summary_sugBev_temp)

###############################################################################
########################### FUNCTIONS  ########################################
###############################################################################

#  /'\_/`\            /\ \        /\_ \       /' \    
# /\      \    ___    \_\ \     __\//\ \     /\_, \   
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_/\ \  
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_     \ \ \ 
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\     \ \_\
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/      \/_/


#  /'\_/`\            /\ \        /\_ \        /'___`\   
# /\      \    ___    \_\ \     __\//\ \      /\_\ /\ \  
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \     \/_/// /__ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_      // /_\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\    /\______/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/_____/ 


#  /'\_/`\            /\ \        /\_ \       /'__`\   
# /\      \    ___    \_\ \     __\//\ \     /\_\L\ \  
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_/_\_<_ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_    /\ \L\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \ \____/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/___/ 


#  /'\_/`\            /\ \        /\_ \     /\ \\ \     
# /\      \    ___    \_\ \     __\//\ \    \ \ \\ \    
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \ \ \\ \_  
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_   \ \__ ,__\
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \/_/\_\_/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/      \/_/  

#                      __          ___       ______    
#  /'\_/`\            /\ \        /\_ \     /\  ___\   
# /\      \    ___    \_\ \     __\//\ \    \ \ \__/   
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \ \___``\ 
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_   \/\ \L\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\   \ \____/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/___/ 