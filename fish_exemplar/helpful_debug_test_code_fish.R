## attempt with ds.lexis, ie the poisson piecewise regression
ds.lexis(data='D2', idCol='ID', entryCol='AGE_BASE', exitCol='AGE_END', statusCol='CASE_OBJ', newobj = "d1_expanded", datasources = opals)
ds.assign(toAssign='log(d1_expanded$SURVIVALTIME)', newobj='logSurvival')
ds.glm(formula='d1_expanded$CASE_OBJ~1+ d1_expanded$TIMEID+d1_expanded$AGE_END+d1_expanded$TOTAL', data='d1_expanded',family='poisson',offset='logSurvival')

ds.lexis.b(data='D2', intervalWidth=c(1,2,3), idCol='D2$ID', entryCol='D2$AGE_BASE', exitCol='D2$AGE_END', statusCol='D2$CASE_OBJ', expandDF = 'A')
ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')

# test purposes
ds.lexis.b(data='D', intervalWidth=c(1,2,3), idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor', expandDF = 'A')
ds.lexis(data='D', idCol='ID', entryCol='entdate', exitCol='enddate', statusCol='censor', newobj = "d1_expanded", datasources = opals)

## Lexis B
# Working with cox cox data to test local against datashield.
datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(1)), opals = opals)
datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(1)), opals = opals)

ds.lexis.b(data='D', intervalWidth=c(90,90,90,500,500), idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor', expandDF = 'A')
ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')

ds.asNumeric('A$age','age')
ds.asNumeric('A$CENSOR','censor')
ds.asNumeric('A$drug', 'drug')
ds.asFactor('A$TIME.PERIOD','tid.f')


ds.glm(formula='censor~1+tid.f+age+drug',family='poisson',offset='logSurvivalA')


# with rebase
all_participants_split <- ds.length('D$age',type = 'split')

for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'zeros', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('zeros','D'), newobj='D2')

ds.assign(toAssign = 'D$enddate-D$entdate', newobj = 'newenddate')
ds.cbind(x=c('newenddate','D2'), newobj='D3')


ds.lexis.b(data='D3', intervalWidth=c(90,90,90,500,500), idCol='D3$ID', entryCol='D3$zeros', exitCol='D3$newenddate', statusCol='D3$censor', expandDF = 'A3')
ds.assign(toAssign='log(A3$SURVTIME)', newobj='logSurvivalA3')
ds.asNumeric('A3$age','age3')
ds.asNumeric('A3$CENSOR','censor3')
ds.asNumeric('A3$drug', 'drug3')
ds.asFactor('A3$TIME.PERIOD','tid.f3')

ds.glm(formula='censor3~1+tid.f3+age3+drug3',family='poisson',offset='logSurvivalA3')


## Lexis o
# Working with cox cox data to test local against datashield.
source(ds.lexis.o.R)
datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(1)), opals = opals)
datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(1)), opals = opals)

ds.lexis.o(data='D', intervalWidth=c(90,90,90,500,500), idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor', expandDF = 'A')
ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')

ds.asNumeric('A$age','age')
ds.asNumeric('A$CENSOR','censor')
ds.asNumeric('A$drug', 'drug')
ds.asFactor('A$TIME.PERIOD','tid.f')


ds.glm(formula='censor~1+tid.f+age+drug',family='poisson',offset='logSurvivalA')


# with rebase
all_participants_split <- ds.length('D$age',type = 'split')

for(i in 1:length(opals)){
  work1 <- all_participants_split[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'zeros', quote(rep(0,",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('zeros','D'), newobj='D2')

ds.assign(toAssign = 'D$enddate-D$entdate', newobj = 'newenddate')
ds.cbind(x=c('newenddate','D2'), newobj='D3')


ds.lexis.o(data='D3', intervalWidth=c(90,90,90,500,500), idCol='D3$ID', entryCol='D3$zeros', exitCol='D3$newenddate', statusCol='D3$censor', expandDF = 'A3')
ds.assign(toAssign='log(A3$SURVTIME)', newobj='logSurvivalA3')
ds.asNumeric('A3$age','age3')
ds.asNumeric('A3$CENSOR','censor3')
ds.asNumeric('A3$drug', 'drug3')
ds.asFactor('A3$TIME.PERIOD','tid.f3')

ds.glm(formula='censor3~1+tid.f3+age3+drug3',family='poisson',offset='logSurvivalA3')