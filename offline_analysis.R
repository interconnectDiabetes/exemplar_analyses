#Regional split 

regions = list()
regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk", 
                                               "InterAct_netherlands", "InterAct_germany", "InterAct_sweden", 
                                               "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC", "Whitehall", "Zutphen"))
regions[['western']] = data.frame("study" = c("ELSA", "WHI", "CARDIA", "ARIC", "MESA", "PRHHP"))
regions[['eastern']] = data.frame("study" = c("NHAPC", "JPHC", "CKB"))
regions[['oceania']] = data.frame("study" = c("AusDiab"))
regions[['middle']] = data.frame("study" = c("Golestan"))

for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]

for (z in 1:length(regions)){
  
  temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
  mypath = file.path('~', 'plots/test', paste0('model_1_', names(regions[z]), '_', my_exposure,'_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=4.5 * length(my_exposure), 
      height=3.5 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  do_REM(coeffs = temp_data$Estimate, s_err = temp_data$`Std. Error`, labels = temp_data$study,fmla = "see main plot", out_family = 'poisson', variable = my_exposure, ref_table = ref_table)
  dev.off()
  
}