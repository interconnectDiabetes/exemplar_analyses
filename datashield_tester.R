# This script quickly tests the functionality of datashield on a target study once they have
# finished setting up

## Author: Paul Scherer
## Date: 31/05/2017

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

server <- c('WHITEHALL')
url <- c('https://interconnect.slms.ucl.ac.uk')
table <- c('1_Fish_T2DM.fish_exemplar_harm')
password <- c( 'Mvcdrqcdl!')
user <- c('Harmonisation')
logindata_all <- data.frame(server,url,user,password, table)

datashield.logout(opals)
opals <- datashield.login(logins=logindata_all, assign=TRUE, directory = '/home/shared/certificates/fish')

a = "variable1"
b = "variable2"

acommand = paste0('D$', a)
bcommand = paste0('D$', b)

# quick summary command
ds.summary(acommand)
ds.summary(bcommand)

fmla = as.formula(paste0(acommand,"~",bcommand))
ds.glm(formula = fmla, family = "gaussian", data='D')