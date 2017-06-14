# Fish exemplar local testing

setwd("V:/Studies/InterConnect/Internal/Fish exemplar/InterAct/for local testing")
sweden <- read.csv("sweden.csv")

noPrevalence = subset(x = sweden, PREV_DIAB == 0)
noType1 = subset(x = noPrevalence, TYPE_DIAB == 2)
intake = subset(x = noType1, E_INTAKE<=3500)
intake2 = subset(x = intake, E_INTAKE>=500)

intake2$EDUCATION = as.factor(intake2$EDUCATION)
intake2$SMOKING = as.factor(intake2$SMOKING)
  
intake2$HYPERTENSION = unlist(lapply(X=intake2$HYPERTENSION, FUN = function(x){
  if (x == -1){
    a = NA
  } else if (x == 0){
    a = 0
  } else if (x == 1){
    a = 1
  } 
  return(a)
}))


intake2$STROKE = unlist(lapply(X=intake2$STROKE, FUN = function(x){
  if (x == -1){
    a = NA
  } else if (x == 0){
    a = 0
  } else if (x == 1){
    a = 1
  } 
  return(a)
}))


intake2$MI = unlist(lapply(X=intake2$MI, FUN = function(x){
  if (x == -1){
    a = NA
  } else if (x == 0){
    a = 0
  } else if (x == 1){
    a = 1
  } 
  return(a)
}))

intake2$STROKE = as.factor(intake2$STROKE)
intake2$MI = as.factor(intake2$MI)
intake2$HYPERTENSION = as.factor(intake2$HYPERTENSION)
intake2$CASE_OBJ = as.factor(intake2$CASE_OBJ)

# take columns that are factors and decide whether they only have one value in them or not
(l <- sapply(intake2, function(x) is.factor(x)))
m <- intake2[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

m = cbind(m, intake2$AGE_BASE)
m = cbind(m, intake2$TOTAL)



fmla = as.formula("CASE_OBJ~intake2$TOTAL + intake2$AGE_BASE + EDUCATION + STROKE + MI + HYPERTENSION")
a = glm(formula = fmla, family = "binomial", data = m)


strokePeople = subset(x = m, STROKE == 1)
hyperPeople = subset(x = m, HYPERTENSION == 1)

# shows that people with stroke all had hypertension. hence the glm fails as there is not more than 
# two levels in the factor to use for comparison, failing the contrasts() subfunction of  
summary(strokePeople$HYPERTENSION)





