library(opal)
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(dsBetaTestClient)
library(metafor)

server <- c('study0')
url <- c('https://192.168.56.111:8443')
user <- c('administrator')
password <- c('password')
table <- c('test.cox')
logindata <- data.frame(server,url,user,password,table)
opals <- datashield.login(logins=logindata,assign=TRUE)


ds.lexis('D', idCol='ID', entryCol='entdate', exitCol='enddate',variables=c('age','drug'), statusCol='censor')

ds.assign(toAssign='log(D_expanded$SURVIVALTIME)', newobj='logSurvival')

ds.glm(formula='censor~1+TIMEID+age+drug', data='D_expanded',family='poisson',offset='logSurvival')
#-----
ds.lexis.5(data='D', intervalWidth=90, idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor')

ds.assign(toAssign='log(D_expanded$expanded.table$SURVTIME)', newobj='logSurvival')

ds.glm(formula='censor~1+idSeq+age+drug', data='D_expanded$expanded.table',family='poisson',offset='logSurvival')

#----------
ds.lexis.b(data='D', intervalWidth=90, idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor')

ds.assign(toAssign='log(D_expanded$SURVTIME)', newobj='logSurvival')

ds.asNumeric('D_expanded$age','age')
ds.asNumeric('D_expanded$CENSOR','censor')

ds.asFactor('D_expanded$drug','drug')
ds.asFactor('D_expanded$TIME.PERIOD','tid.f')

ds.glm(formula='censor~0+tid.f+age+drug',family='poisson',offset='logSurvival')

#------- logistic regression

ds.glm(formula='D$censor~D$age+D$drug',family='binomial')

#--------------
# Prentice weighting

ds.assign(toAssign='!D$drug*D$ent_time+D$drug*(D$ent_time-0.001)', newobj='D$ent_time_0')

datashield.assign(opals, 'D$ent_time_0', as.symbol('!D$drug*D$ent_time+D$drug*(D$ent_time-0.001)'))
