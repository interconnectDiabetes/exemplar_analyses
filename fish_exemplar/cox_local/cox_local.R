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