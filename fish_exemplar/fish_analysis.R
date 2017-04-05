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
opals <- datashield.login(logins=logindata_all, assign=TRUE, directory = '/home/shared/certificates/fish')


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

# smoking

# pa

# alcohol

# mi

# stroke

# cancer 

# hypertension

# supplements

# eintake

# meat

# fruit

# veg

# dairy

# fiber

# sugary drinks

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