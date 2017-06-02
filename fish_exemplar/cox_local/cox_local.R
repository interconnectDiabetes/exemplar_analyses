## dependencies
library(survival)

setwd("~/workspace/exemplar_analyses/fish_exemplar/cox_local/")
hmo <- read.csv("hmohiv.csv", row.names = 1, header = TRUE)

# checking creation of survival object
survivalObject = Surv(time = hmo$entdate, time2 = hmo$enddate, event = hmo$censor)
str(survivalObject)

# testing the same coxph written in different ways
coxfit1 = coxph(Surv(entdate, enddate, censor) ~ age + drug, hmo)
coxfit2 = coxph(formula = as.formula(survivalObject~age+drug), hmo)

coeffs1 = (summary(coxfit1))$coefficients

## rebasing locally to 0 if that makes any difference here given that we create the survival object
hmo$enddatenew = hmo$enddate - hmo$entdate
hmo$entdatenew = 0

# checking if there is difference between the survivalobjects created
survivalObjectRebased = Surv(time = hmo$entdatenew, time2 = hmo$enddatenew, event = hmo$censor)
str(survivalObjectRebased)
identical(survivalObject, survivalObjectRebased)

coxfitrebase = coxph(Surv(entdatenew, enddatenew, censor) ~ age + drug, hmo)
coeffsrebase = (summary(coxfitrebase))$coefficients
