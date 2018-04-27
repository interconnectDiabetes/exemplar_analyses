library(metafor)

filter_csv = read.csv(file = 'U:/___Personal___/fish_opal_vars.csv',  header=TRUE, row.names = 1 )


#model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2017_12_04/model_1_TOTAL_A_OBJ_SELF.csv')
#model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2017_12_04/model 2 with regions/model_2_TOTAL_A_OBJ.csv')
#model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2017_12_04/model 4/model_4_men_TOTAL_A_OBJ_men.csv')
model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2017_12_04/model 4/model_4_women_TOTAL_A_OBJ_women.csv')

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')

my_outcome = c('CASE_OBJ')

ref_table =  'A_OBJ'

my_exposure = c('TOTAL')
# model 1
#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

#model 2

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

#model 4

my_covariate =  c("AGE_BASE", "EDUCATION", "SMOKING", "PA","BMI", "COMORBID","E_INTAKE", 
                  "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

#Regional split 

regions = list()

#model 1
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                               "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['nwestern']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
regions[['swestern']] = data.frame("study" = c("ELSA"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB", "Golestan", "SWHS", "SMHS"))
regions[['oceania']] = data.frame("study" = c("AusDiab"))

# model 2

# regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
#                                                "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
#                                                "InterAct_denmark","FMC", "NOWAC", "SMC"))
# regions[['nwestern']] = data.frame("study" = c("WHI", "ARIC", "MESA", "PRHHP"))
# regions[['swestern']] = data.frame("study" = c("ELSA"))
# regions[['eastern']] = data.frame("study" = c("JPHC", "CKB", "Golestan"))


mylist=list('SMC')
names(mylist) <- 'SMC'
myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
                               exposure = my_exposure, covariate_list = my_covariate, type = 'survival')

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2017_12_04/model 4 with regions', paste0('model_4_women_', names(regions[z]), '_', my_exposure,'_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=4.5 * length(my_exposure), 
      height=3.5 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  do_REM(coeffs = temp_data$Estimate, s_err = temp_data$`Std. Error`, labels = temp_data$study,fmla = myformula, out_family = 'poisson', variable = my_exposure, ref_table = ref_table)
  dev.off()
  
}

#########################

filter_csv = read.csv(file = 'U:/___Personal___/fish_opal_vars.csv',  header=TRUE, row.names = 1 )

model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/model_2_SERVINGS_A_OBJ.csv')

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')


#my_exposure = c('TOTAL')
my_exposure = c('SERVINGS2')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
#                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS", "FAM_DIAB")

my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'
#ref_table =  'A_OBJ_SELF'

# my_studies =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
#                                                "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
#                                                "InterAct_denmark","FMC", "NOWAC", "SMC","WHI", "ARIC", "MESA", "PRHHP",
#                                      "ELSA", "JPHC", "CKB", "Golestan"))

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
                               exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/', paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=5 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)
do_REM(coeffs = temp_data$Estimate, s_err = temp_data$`Std. Error`, labels = temp_data$study,fmla = myformula, out_family = 'poisson', variable = my_exposure, ref_table = ref_table)
dev.off()

#Regional split 

regions = list()

#model 1
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                               "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['nwestern']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
regions[['swestern']] = data.frame("study" = c("ELSA"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB", "Golestan", "SWHS", "SMHS"))
regions[['oceania']] = data.frame("study" = c("AusDiab"))

# model 2

# regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
#                                                "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
#                                                "InterAct_denmark","FMC", "NOWAC", "SMC"))
# regions[['nwestern']] = data.frame("study" = c("WHI", "ARIC", "MESA", "PRHHP"))
# regions[['swestern']] = data.frame("study" = c("ELSA"))
# regions[['eastern']] = data.frame("study" = c("JPHC", "CKB", "Golestan"))


mylist=list('SMC')
names(mylist) <- 'SMC'
myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
                               exposure = my_exposure, covariate_list = my_covariate, type = 'survival')

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/LEAN/', paste0('model_2_', names(regions[z]), '_', my_exposure,'_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=4.5 * length(my_exposure), 
      height=5 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  do_REM(coeffs = temp_data$Estimate, s_err = temp_data$`Std. Error`, labels = temp_data$study,fmla = myformula, out_family = 'poisson', variable = my_exposure, ref_table = ref_table)
  dev.off()
  
}


#########################

filter_csv = read.csv(file = 'U:/___Personal___/leg_opal_vars.csv',  header=TRUE, row.names = 1 )

model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/initial run/model_0_TOTAL_A_OBJ.csv')

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')


my_exposure = c('TOTAL')
#my_exposure = c('PBCL')
#my_exposure = c('NUTS_SEEDS')

my_covariate =  c("AGE_BASE", "SEX")


# my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL")

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL",
#                  "BMI", "WAIST")

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL",
#                  "BMI", "WAIST", "COMORBIDITY", "FAM_DIAB")
#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "E_INTAKE", "ALCOHOL",
#                  "BMI", "WAIST", "COMORBIDITY", "FAM_DIAB", "COV_FRUIT", "COV_VEG", "COV_FIBER", "COV_SUG_BEVS")


my_outcome = c('CASE_OBJ')
#my_outcome = c('CASE_OBJ_SELF')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'

# my_studies =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
#                                                "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
#                                                "InterAct_denmark","FMC", "NOWAC", "SMC","WHI", "ARIC", "MESA", "PRHHP",
#                                      "ELSA", "JPHC", "CKB", "Golestan"))

my_studies =  data.frame("study" = c("ARIC","AusDiab","CARDIA","CoLaus",
                                     "ELSA_B","FMC","Golestan","HOORN","InterAct_france",
                                     "InterAct_germany","InterAct_italy","InterAct_netherlands",
                                     "InterAct_spain","InterAct_sweden","InterAct_uk",
                                     "MESA","PRHHP","SMC","WHI","Whitehall","Zutphen"))

mylist=list('SMC')
names(mylist) <- 'SMC'
myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
                               exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path('V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/initial run/portion/', paste0('model_0_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=4.5 * length(my_exposure), 
    height=4.5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)
do_REM(coeffs = temp_data$Estimate*20, s_err = temp_data$`Std. Error`*20, labels = temp_data$study,fmla = myformula, out_family = 'poisson', variable = my_exposure, ref_table = ref_table)
dev.off()

####################################################
#### for 26/04 presentation
####################################################




filter_csv = read.csv(file = 'U:/___Personal___/fish_opal_vars.csv',  header=TRUE, row.names = 1 )


##### model 1 total primary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 1 with regions/'

model_1_alltuned = read.csv(paste0(file_loc,'model_1_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

my_outcome = c('CASE_OBJ')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'

myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_1_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                        .(round(res$I2)),'%, p = ',
                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)

text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)

dev.off()

##### model 1 total secondary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 1 with regions/'

model_1_alltuned = read.csv(paste0(file_loc,'model_1_TOTAL_A_OBJ_SELF.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

my_outcome = c('CASE_OBJ_SELF')

ref_table =  'A_OBJ_SELF'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'

myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_1_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)

text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)

dev.off()

##### model 2 total primary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()

##### model 2 total secondary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_TOTAL_A_OBJ_SELF.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ_SELF'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()


##### model 3a total 

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 3a and comparison to model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_3a_FAM_DIAB_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_3a_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()

##### model 3b total 

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 3b and comparison to model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_3b_WAIST_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_3b_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()

##### model 3c total 

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 3c and comparison to model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_3c_SUPPLEMENTS_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_3c_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()

##### model 4 total interaction men

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 4/'

model_1_alltuned = read.csv(paste0(file_loc,'model_4_men_TOTAL_A_OBJ_men.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_4_men_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()


##### model 4 total interaction women

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 4/'

model_1_alltuned = read.csv(paste0(file_loc,'model_4_women_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_4_women_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(format(round(res$pval,3),nsmall=3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()


##### model 2 servings 2 compared to 1

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_SERVINGS_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('SERVINGS2')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate, sei = temp_data$`Std. Error`, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.4,0.6,1.0,1.6,2.4)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(format(round(res$pval,3),nsmall=3)))), atransf = exp, xlim = log(c(0.1, 6)),alim=log(c(0.4, 2.4)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()


##### model 2 servings 3 compared to 1

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_SERVINGS_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('SERVINGS3')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate, sei = temp_data$`Std. Error`, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.4,0.6,1.0,1.6,2.4)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(format(round(res$pval,3),nsmall=3)))), atransf = exp, xlim = log(c(0.1, 6)),alim=log(c(0.4, 2.4)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()


##### model 2 servings 4 compared to 1

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_SERVINGS_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('SERVINGS4')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate, sei = temp_data$`Std. Error`, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.4,0.6,1.0,1.6,2.4)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(format(round(res$pval,3),nsmall=3)))), atransf = exp, xlim = log(c(0.1, 6)),alim=log(c(0.4, 2.4)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()

##### model 2 total primary by region

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 2/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

#Regional split 

regions = list()

regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                               "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['nwestern']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
regions[['swestern']] = data.frame("study" = c("ELSA"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB", "Golestan", "SWHS", "SMHS"))
regions[['oceania']] = data.frame("study" = c("AusDiab"))

for (z in 1:length(regions)){

  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_', names(regions[z]), '_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=6 * length(my_exposure), 
      height=3.2 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  
  res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)
  
  #add the weights to the labels
  res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")
  
  forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                          .(round(res$I2)),'%, p = ',
                                                                          .(format(round(res$QEp,3), nsmall=3)),')')),
         xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                           .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
  usr <- par("usr")
  usr <- par("usr")
  text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
  text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
  dev.off()
}

#### remove CKB


regions = list()

regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "Golestan", "SWHS", "SMHS"))


for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path(file_loc, paste0('model_2_noCKB_', my_exposure,'_', names(regions[z]), '_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=6 * length(my_exposure), 
      height=3.2 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  
  res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)
  
  #add the weights to the labels
  res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")
  
  forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                          .(round(res$I2)),'%, p = ',
                                                                          .(format(round(res$QEp,3), nsmall=3)),')')),
         xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                           .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
  usr <- par("usr")
  usr <- par("usr")
  text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
  text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
  dev.off()
}

##### model 2 total primary by region MEN

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 4/'

model_1_alltuned = read.csv(paste0(file_loc,'model_4_men_TOTAL_A_OBJ_men.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

#Regional split 

regions = list()

regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                               "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['nwestern']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
regions[['swestern']] = data.frame("study" = c("ELSA"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB", "Golestan", "SWHS", "SMHS"))
regions[['oceania']] = data.frame("study" = c("AusDiab"))

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path(file_loc, paste0('model_4_men_', my_exposure,'_', names(regions[z]), '_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=6 * length(my_exposure), 
      height=3.2 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  
  res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)
  
  #add the weights to the labels
  res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")
  
  forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                          .(round(res$I2)),'%, p = ',
                                                                          .(format(round(res$QEp,3), nsmall=3)),')')),
         xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                           .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
  usr <- par("usr")
  usr <- par("usr")
  text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
  text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
  dev.off()
}

##### model 2 total primary by region WOMEN

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/model 4/'

model_1_alltuned = read.csv(paste0(file_loc,'model_4_women_TOTAL_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('TOTAL')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

#Regional split 

regions = list()

regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                               "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['nwestern']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
regions[['swestern']] = data.frame("study" = c("ELSA"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB", "Golestan", "SWHS", "SMHS"))
regions[['oceania']] = data.frame("study" = c("AusDiab"))

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path(file_loc, paste0('model_4_women_', my_exposure,'_', names(regions[z]), '_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=6 * length(my_exposure), 
      height=3.2 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  
  res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)
  
  #add the weights to the labels
  res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")
  
  forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                          .(round(res$I2)),'%, p = ',
                                                                          .(format(round(res$QEp,3), nsmall=3)),')')),
         xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                           .(format(round(res$pval,3), nsmall=3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
  usr <- par("usr")
  usr <- par("usr")
  text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
  text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
  dev.off()
}

##### model 2 FATTY primary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/FATTY/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_FATTY_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('FATTY')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()


##### model 2 LEAN primary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/LEAN/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_LEAN_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

my_exposure = c('LEAN')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()



##### model 2 FRIED primary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/FRIED/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_FRIED_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

### REMOVE CARDIA

model_1_alltuned = model_1_alltuned[model_1_alltuned$study!='CARDIA',]

my_exposure = c('FRIED')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.6, 1.5)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()

##### model 2 NONFISH primary

file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_03_08/NONFISH/'

model_1_alltuned = read.csv(paste0(file_loc,'model_2_NONFISH_A_OBJ.csv'))

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')
model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]

### REMOVE FMC

model_1_alltuned = model_1_alltuned[model_1_alltuned$study!='FMC',]

my_exposure = c('NONFISH')

ref_table =  'A_OBJ'

my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
colnames(my_studies) = 'study'

mylist=list('SMC')
names(mylist) <- 'SMC'
#myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
#                              exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
myformula = ''

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path(file_loc, paste0('model_2_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=6 * length(my_exposure), 
    height=5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)

res <- rma(yi = temp_data$Estimate*100/7, sei = temp_data$`Std. Error`*100/7, method='DL', slab = temp_data$study)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

forest(res,at=log(c(0.8,0.9,1.0,1.1,1.2)),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                        .(round(res$I2)),'%, p = ',
                                                                        .(format(round(res$QEp,3), nsmall=3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true Hazard ratio = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp, xlim = log(c(0.9, 1.6)),alim=log(c(0.8, 1.2)))
usr <- par("usr")
usr <- par("usr")
text(usr[2], usr[4], "Hazard Ratio [95% CI]", adj = c(1, 4),cex=0.75)
text(usr[1], usr[3], paste0(ref_table,my_exposure), adj = c( 0, 0 ),cex=0.75)
dev.off()
