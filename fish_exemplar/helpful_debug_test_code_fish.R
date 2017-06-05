## attempt with ds.lexis, ie the poisson piecewise regression
ds.lexis(data='D2', idCol='ID', entryCol='AGE_BASE', exitCol='AGE_END', statusCol='CASE_OBJ', newobj = "d1_expanded", datasources = opals)
ds.assign(toAssign='log(d1_expanded$SURVIVALTIME)', newobj='logSurvival')
ds.glm(formula='d1_expanded$CASE_OBJ~1+ d1_expanded$TIMEID+d1_expanded$AGE_END+d1_expanded$TOTAL', data='d1_expanded',family='poisson',offset='logSurvival')

ds.lexis.b(data='D2', intervalWidth=c(1,2,3), idCol='D2$ID', entryCol='D2$AGE_BASE', exitCol='D2$AGE_END', statusCol='D2$CASE_OBJ', expandDF = 'A')
ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')

# test purposes
ds.lexis.b(data='D', intervalWidth=c(1,2,3), idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor', expandDF = 'A')
ds.lexis(data='D', idCol='ID', entryCol='entdate', exitCol='enddate', statusCol='censor', newobj = "d1_expanded", datasources = opals)


# Working with cox cox data to test local against datashield.
datashield.assign(symbol = 'DANGER.nfilter.tab', value = quote(c(1)), opals = opals)
datashield.assign(symbol = 'DANGER.nfilter.glm', value = quote(c(1)), opals = opals)

ds.lexis.b(data='D', intervalWidth=c(1,2,3), idCol='D$ID', entryCol='D$entdate', exitCol='D$enddate', statusCol='D$censor', expandDF = 'A')
ds.assign(toAssign='log(A$SURVTIME)', newobj='logSurvivalA')

ds.asNumeric('A$AGE_BASE','age')
ds.asNumeric('A$CENSOR','censor')
ds.asNumeric('A$TOTAL', 'total')
ds.asFactor('A$TIME.PERIOD','tid.f')


ds.glm(formula='censor~1+tid.f+age+total',family='poisson',offset='logSurvival')
