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
# regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
#                                                "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
#                                                "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
# regions[['nwestern']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
# regions[['swestern']] = data.frame("study" = c("ELSA"))
# regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB", "Golestan"))
# regions[['oceania']] = data.frame("study" = c("AusDiab"))

# model 2

regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                               "InterAct_denmark","FMC", "NOWAC", "SMC"))
regions[['nwestern']] = data.frame("study" = c("WHI", "ARIC", "MESA", "PRHHP"))
#regions[['swestern']] = data.frame("study" = c("ELSA"))
regions[['eastern']] = data.frame("study" = c("JPHC", "CKB", "Golestan"))


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

model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/tentative_plots/2017_12_04/model_2_FAM_DIAB_TOTAL_A_OBJ.csv')

model_1_alltuned = model_1_alltuned[,-1]
colnames(model_1_alltuned)[5] = c('Std. Error')


my_exposure = c('TOTAL')

#my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID")

my_covariate =  c("AGE_BASE", "SEX", "EDUCATION", "SMOKING", "PA", "BMI", "COMORBID",
                  "E_INTAKE", "ALCOHOL", "FIBER", "MEAT", "FRUIT", "VEG", "SUG_BEVS")

my_outcome = c('CASE_OBJ')
#my_exit_col = c('newEndDate')
ref_table =  'A_OBJ'

# my_studies =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
#                                                "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
#                                                "InterAct_denmark","FMC", "NOWAC", "SMC","WHI", "ARIC", "MESA", "PRHHP",
#                                      "ELSA", "JPHC", "CKB", "Golestan"))

my_studies =  data.frame("study" = c("InterAct_uk",
                                     "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                     "FMC", "SMC","WHI", "ARIC", "PRHHP",
                                      "JPHC", "CKB"))

mylist=list('SMC')
names(mylist) <- 'SMC'
myformula = createModelFormula(study = mylist['SMC'],data_table = ref_table, outcome = my_outcome, 
                               exposure = my_exposure, covariate_list = my_covariate, type = 'survival')
for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

temp_data = merge(x = my_studies, y = for_RMA, by = "study")
mypath = file.path('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/tentative_plots/2017_12_04', paste0('model_2_PRE_FAM_DIAB_', my_exposure,'_',ref_table,'.svg'))
svg(filename=mypath, 
    width=4.5 * length(my_exposure), 
    height=4.5 * length(my_outcome), 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(length(my_outcome),length(my_exposure)))
par(ps=10)
do_REM(coeffs = temp_data$Estimate, s_err = temp_data$`Std. Error`, labels = temp_data$study,fmla = myformula, out_family = 'poisson', variable = my_exposure, ref_table = ref_table)
dev.off()



