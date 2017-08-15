# # All working at once
server <- c('HOORN')
url <- c(         'https://145.121.54.195:8443')
table <- c(
  'interconnect.hoorn_harm')
password <- c(
  'datashield-fish-privatekey.pem')
user <- c(
  'datashield-fish-publickey.pem')
logindata_all <- data.frame(server,url,user,password, table)



opals <- datashield.login(logins=logindata_all, assign=TRUE,directory = '/home/shared/certificates/fish')





# c(2,2,1,3.5,2,2,1,2,2,2))

my_study = 'InterAct_denmark'
my_study = 'InterAct_netherlands'
my_study = 'HOORN'
my_study = 'JPHC'
my_study = 'WHI'
my_study = 'InterAct_france'
my_study = 'AusDiab'

# and expand the dataset using the ds.lexis b command
datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(0.1)), opals = opals[my_study])
datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(0.1)), opals = opals[my_study])

my_vars_all = c("TOTAL", "CASE_OBJ", "AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID", 
                "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "newEndDate", "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all, datasources = opals[my_study])
ds.subset(x = 'D7', subset = 'D8', completeCases = TRUE, datasources = opals[my_study])


# Exposure: total fish (g/d) at baseline
# Outcome: CASE_OBJ
# Confounders: Age, sex, education, smoking, physical activity, BMI, co-morbidities
# To assess the impact of each confounder we will also run models including each confounder separately.


ds.lexis.b(data='D8', intervalWidth =  c(1,1,1,1,0.5,2.5), idCol = 'D8$ID', entryCol = 'D8$newStartDate', 
           exitCol = 'D8$newEndDate', statusCol = 'D8$CASE_OBJ', expandDF = 'A', datasources = opals[my_study])

ds.asNumeric('A$CENSOR','censor', datasources = opals[my_study])
ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA', datasources = opals[my_study])

ds.asFactor('A$TIME.PERIOD','tid.f', datasources = opals[my_study])
ds.summary('tid.f', datasources = opals[my_study])
ds.summary('A$TIME.PERIOD', datasources = opals[my_study])

results = ds.glm(formula = 'A$CASE_OBJ~0+tid.f+A$TOTAL+A$AGE_BASE+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$E_INTAKE+A$ALCOHOL+A$FIBER+A$MEAT+A$FRUIT+A$VEG+A$SUG_BEVS', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)
results = ds.glm(formula = 'A$CASE_OBJ~0+tid.f+A$TOTAL+A$AGE_BASE+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$ALCOHOL+A$FIBER+A$MEAT+A$FRUIT+A$VEG', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)
results = ds.glm(formula = 'A$CASE_OBJ~0+tid.f+A$TOTAL+A$AGE_BASE+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$E_INTAKE', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)
results = ds.glm(formula = 'A$CASE_OBJ~0+tid.f+A$TOTAL+A$AGE_BASE+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$SUG_BEVS+A$FRUIT+A$FIBER', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)
results = ds.glm(formula = 'A$CASE_OBJ~0+A$E_INTAKE+tid.f', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)
results = ds.glm(formula = 'A$CASE_OBJ~0+A$E_INTAKE', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)
results = ds.glm(formula = 'A$CASE_OBJ~A$TOTAL+A$AGE_BASE+A$PA+A$E_INTAKE+A$ALCOHOL+A$FIBER+A$MEAT+A$FRUIT+A$VEG+A$SUG_BEVS', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)
results = ds.glm(formula = 'A$CASE_OBJ~0+tid.f+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$E_INTAKE', data = 'A', family = 'binomial', datasources=opals[my_study], maxit=100)

results = ds.glm(formula = 'censor~0+tid.f+A$TOTAL+A$AGE_BASE+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$E_INTAKE+A$ALCOHOL+A$FIBER+A$MEAT+A$FRUIT+A$VEG+A$SUG_BEVS', data = 'A', family = 'poisson', datasources=opals[my_study], offset = 'logSurvivalA', weights = 'A$burtonWeights', maxit=100)
results = ds.glm(formula = 'censor~0+tid.f+A$TOTAL+A$AGE_BASE+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$ALCOHOL+A$FIBER+A$MEAT+A$FRUIT+A$VEG+A$SUG_BEVS', data = 'A', family = 'poisson', datasources=opals[my_study], offset = 'logSurvivalA', weights = 'A$burtonWeights', maxit=100)
results = ds.glm(formula = 'censor~0+tid.f+A$E_INTAKE', data = 'A', family = 'poisson', datasources=opals[my_study], offset = 'logSurvivalA', weights = 'A$burtonWeights', maxit=100)


for(i in 1:length(opals)) {
  tunedLexisB_test("D8", opals[i])
}

ds.asFactor('A$TIME.PERIOD','tid.f')
ds.summary('tid.f')


tunedLexisB_test <- function(ref_table, study) {
  # does a tuned Lexis B on one study can be used in a parallel loop to run
  # multiple lexis b at once
  # TODO check prerequisites with dsExists
  studyName = names(study)
  # assign Danger.NFILTER to some values as the current code in the dsBeta doesnt work without this.
  # and expand the dataset using the ds.lexis b command
  datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(0.1)), opals = study)
  datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(0.1)), opals = study)
  idColString = paste0(ref_table, '$', 'ID')
  entryColString = paste0(ref_table, '$', 'newStartDate')
  exitColString = paste0(ref_table, '$', 'newEndDate')
  statusColString = paste0(ref_table, '$', 'CASE_OBJ')
  
  if (studyName == "InterAct_germany"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "InterAct_denmark"){ 
    interval_width =  c(2,2,2,1.5,5)
  }
  else if (studyName == "InterAct_france"){ 
    interval_width =  c(1,1,1,1,1,1,1,1.5,1,4)
  }
  else if (studyName == "InterAct_italy"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "InterAct_netherlands"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "InterAct_spain"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "InterAct_sweden"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "InterAct_uk"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "HOORN"){ 
    interval_width =  c(2,2,2.5,2.5)
  }
  else if (studyName == "ELSA"){ 
    #interval_width =  c(2,2,1,3.5)
    interval_width =  c(2,2,0.5,3.5)
  }
  else if (studyName == "NOWAC"){ 
    interval_width =  c(1,1,1,1,1,1,1,2)
  }
  else if (studyName == "SMC"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "Zutphen"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "Whitehall"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "AusDiab"){ 
    interval_width =  c(1,1,1,1,1,1,1,1.5,1,1,1,2)
  }
  else if (studyName == "NHAPC"){ 
    interval_width =  c(1,1,1,1,1,1)
  }
  else if (studyName == "WHI"){ 
    interval_width =  c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2)
  }
  else if (studyName == "JPHC"){ 
    interval_width =  c(1,1,1,1,1,2.5)
  }
  else {
    interval_width =  c(1)
  }
  
  ds.lexis.b(data=ref_table, intervalWidth = interval_width, idCol = idColString, entryCol = entryColString, 
             exitCol = exitColString, statusCol = statusColString, expandDF = 'A', datasources = study)

  
  print("Did LexisB for study \n")
  print(studyName)
}

  


