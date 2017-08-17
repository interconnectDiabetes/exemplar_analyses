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


# # All working at once
server <- c('AusDiab')
url <- c('https://opal.mrc-epid.cam.ac.uk:8443')
table <- c('AusDiab.AusDiabHarm')
password <- c('interconnect2017')
user <- c('paul')
logindata_all <- data.frame(server,url,user,password, table)



opals['AusDiab'] <- datashield.login(logins=logindata_all, assign=TRUE,directory = '/home/shared/certificates/fish')

all_participants <- ds.length('D$TOTAL')
all_participants_split <- ds.length('D$TOTAL',type = 'split')

# Set studynames and numstudies
temp <- ds.summary('D$TOTAL')
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

# remove participants with prevalent diabetes and type 1
ds.subset(x = 'D', subset = 'E1', logicalOperator = 'PREV_DIAB==', threshold = 0)
noPrevalence <- ds.length('E1$SEX', type = 'split')
ds.subset(x = 'E1', subset = 'E2', logicalOperator = 'TYPE_DIAB==', threshold = 1)
noType1 <- ds.length('E2$SEX', type = 'split')

# In order to deal with the intake subsets stratified by sex we will have to create subsets of sex,
# do the intake subset and then rbind the groups back together. What follows is DataSHIELD magic
ds.asNumeric("E2$SEX", newobj = "sexNumbers")
ds.assign(toAssign="(sexNumbers*300)+E2$E_INTAKE", newobj = "adjustedLowerBound")
ds.assign(toAssign="(sexNumbers*700)+E2$E_INTAKE", newobj = "adjustedUpperBound")
ds.cbind(x=c("adjustedLowerBound", "E2"), newobj = "L1")
ds.cbind(x=c("adjustedUpperBound", "L1"), newobj = "L2")
# remove participants with too little and excessive consumption of calories
ds.subset(x = 'L2', subset = 'E3', logicalOperator = 'adjustedUpperBound<=', threshold = 4200)
under3500cal <- ds.length('E3$SEX', type = 'split')
ds.subset(x = 'E3', subset = 'E4', logicalOperator = 'adjustedLowerBound>=', threshold = 800)
afterIntake <- ds.length('E4$SEX', type = 'split')

# Setup an additional proxy ID column for each study 
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'ID', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
rm(i) # removal of i as it is not scoped within the loop
ds.cbind(x=c('ID','E4'), newobj='D2')

# Zeros, new start date and end date
for(i in 1:length(opals)){
  work1 <- afterIntake[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'newStartDate', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
rm(i)
ds.cbind(x=c('newStartDate','D2'), newobj='D3')
ds.assign(toAssign = 'D3$FUP_OBJ', newobj = 'newEndDate')
ds.cbind(x=c('newEndDate','D3'), newobj='D5')

# Adding in the weights as described by Dr. Burton
ds.asNumeric('D5$CASE_OBJ', newobj = "caseNums")
ds.assign(toAssign="((1 - caseNums)*35.92055) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_france'])
ds.assign(toAssign="((1 - caseNums)*23.55086) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_italy'])
ds.assign(toAssign="((1 - caseNums)*11.0115) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_spain'])
ds.assign(toAssign="((1 - caseNums)*27.87205) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_uk'])
ds.assign(toAssign="((1 - caseNums)*24.27497) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_netherlands'])
ds.assign(toAssign="((1 - caseNums)*24.62187) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_germany'])
ds.assign(toAssign="((1 - caseNums)*17.68276) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_sweden'])
ds.assign(toAssign="((1 - caseNums)*27.28305) + caseNums",  newobj = "burtonWeights", datasources = opals['InterAct_denmark'])

# Non InterAct studies get a weighting of 1 in either case or noncase
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['HOORN'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['NHAPC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['NOWAC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['SMC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['ELSA'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['Whitehall'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['Zutphen'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['AusDiab'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['JPHC'])
ds.assign(toAssign="newStartDate + 1",  newobj = "burtonWeights", datasources = opals['WHI'])
ds.cbind(x=c('burtonWeights','D5'), newobj='D6')

# put in any dummy columns for the studies with completely missing columns
# italy missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['InterAct_italy'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['InterAct_italy'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['InterAct_italy'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['InterAct_italy'])
# spain missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['InterAct_spain'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['InterAct_spain'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['InterAct_spain'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['InterAct_spain'])
# nowac missing fam_diab
d6len = ds.length('D6$SEX', datasources = opals['NOWAC'])
ds.assign(toAssign = "newStartDate + 1", newobj = "FAM_DIAB", datasources = opals['NOWAC'])
ds.asFactor(x = "FAM_DIAB", newobj = "FAM_DIAB", datasources = opals['NOWAC'])
ds.cbind(x = c("FAM_DIAB", "D6"), newobj = "D6", datasources = opals['NOWAC'])

# Loop to produce E4 and model_all_len for descriptive stats
# Note that this doesnt actually handle well if a study has lost all its participants before this section
my_vars_all = c("AGE_BASE", "CASE_OBJ", "TOTAL",
                "SEX", "BMI", "EDUCATION", "SMOKING", "PA", "ALCOHOL", "COMORBID", "E_INTAKE", "FRUIT",
                "VEG", "FIBER", "MEAT", "SUG_BEVS", "FAM_DIAB", "WAIST", "SUPPLEMENTS","newEndDate", "newStartDate", "burtonWeights")
my_vars_all <- c('ID', my_vars_all) #because datashield doesnt like single column subsets

# quicker complete cases
ds.subset(x = 'D6', subset = 'D7', cols =  my_vars_all)
ds.subset(x = 'D7', subset = 'D4', completeCases = TRUE)
length_complete = ds.length('D4$SEX')
length_complete_split = ds.length("D4$SEX", type = "split")







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


ds.lexis.b(data='D8', intervalWidth =  c(1,1,1,1,1,1,1,1.5,1,1,1,2), idCol = 'D8$ID', entryCol = 'D8$newStartDate', 
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

results = ds.glm(formula = 'censor~0+tid.f+A$TOTAL+A$AGE_BASE+A$SEX+A$EDUCATION+A$SMOKING+A$PA+A$BMI+A$COMORBID+A$E_INTAKE+A$ALCOHOL+A$MEAT+A$FRUIT+A$VEG', data = 'A', family = 'poisson', datasources=opals[my_study], offset = 'logSurvivalA', weights = 'A$burtonWeights', maxit=100)

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

  


