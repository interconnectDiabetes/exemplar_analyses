# meta regression



# This code can be used to generate tidied and scaled forest plots from the csv
# files saved during the DataSHIELD analyses

library('metafor')

do_offline <- function(filename_in, model_name, exposure, outcome, scaling_factor,
                       at_vect, xlim_vect, alim_vect, do_regions = FALSE, orderby = 'study'){

  model_1_alltuned = read.csv(paste0(file_loc,filename_in))
  
  model_1_alltuned = model_1_alltuned[,-1]
  colnames(model_1_alltuned)[5] = c('Std. Error')

  
  my_exposure = exposure
  
  ref_table =  outcome
  
  my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
  colnames(my_studies) = 'study'
  
  
  for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]
  
  temp_data = merge(x = my_studies, y = for_RMA, by = "study")
  
  temp_data = temp_data[order(eval(parse(text=paste0("temp_data$", orderby)))),]
  
  mypath = file.path(file_loc, paste0(model_name, my_exposure,'_',ref_table,'.svg'))
  svg(filename=mypath, 
      width=6 * length(my_exposure), 
      height=6 * length(my_outcome), 
      pointsize=10)
  par(mar=c(5,3,2,2)+0.1)
  par(mfrow=c(length(my_outcome),length(my_exposure)))
  par(ps=10)
  
  res <- rma(yi = temp_data$Estimate*scaling_factor, sei = temp_data$`Std. Error`*scaling_factor, method='DL', slab = temp_data$study)
  
  #add the weights to the labels
  res$slab <- paste(res$slab, " (", format(round(weights.rma.uni(res),digits=1), nsmall=1), "%)")
  
  forest(res,at=log(at_vect),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                          .(round(res$I2)),'%, p = ',
                                                                          .(format(round(res$QEp,3), nsmall=3)),')')),
         xlab=bquote(paste('Test of H'[0]*': true Rate Ratio = 1, p = ',
                           .(format(round(res$pval,3), nsmall=3)))), atransf = exp, xlim = log(xlim_vect),alim=log(alim_vect))
  usr <- par("usr")
  usr <- par("usr")
  text(usr[2], usr[4], "Rate Ratio [95% CI]", adj = c(1, 4),cex=0.75)
  dev.off()
  
  #Regional split 
  
  if(do_regions){
  
    regions = list()
    
    regions[['europe']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                                   "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                                   "InterAct_denmark","FMC","HOORN", "SMC","SUN", "Whitehall", "Zutphen"))
    regions[['n_america']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA"))
    regions[['australia']] = data.frame("study" = c("AusDiab"))
    regions[['asia']] = data.frame("study" = c("Golestan", "CKB"))
    regions[['latin']] = data.frame("study" = c("MTC", "PRHHP", "ELSA_B"))
  
    for (z in 1:length(regions)){
      
      temp_data = merge(x = regions[[z]], y = for_RMA, by = "study")
      mypath = file.path(file_loc, paste0(model_name, my_exposure,'_', names(regions[z]), '_',ref_table,'.svg'))
      svg(filename=mypath, 
          width=6 * length(my_exposure), 
          height=3.2 * length(my_outcome), 
          pointsize=10)
      par(mar=c(5,3,2,2)+0.1)
      par(mfrow=c(length(my_outcome),length(my_exposure)))
      par(ps=10)
      
      res <- rma(yi = temp_data$Estimate*scaling_factor, sei = temp_data$`Std. Error`*scaling_factor, method='DL', slab = temp_data$study)
      
      #add the weights to the labels
      res$slab <- paste(res$slab, " (", format(round(weights.rma.uni(res),digits=1), nsmall=1), "%)")
      
      forest(res,at=log(at_vect),  digits=3, mlab=bquote(paste('Overall (I'^2*' = ', 
                                                                              .(round(res$I2)),'%, p = ',
                                                                              .(format(round(res$QEp,3), nsmall=3)),')')),
             xlab=bquote(paste('Test of H'[0]*': true Rate Ratio = 1, p = ',
                               .(format(round(res$pval,3), nsmall=3)))), atransf = exp, xlim = log(xlim_vect),alim=log(alim_vect))
      usr <- par("usr")
      usr <- par("usr")
      text(usr[2], usr[4], "Rate Ratio [95% CI]", adj = c(1, 4),cex=0.75)
      dev.off()
    }
  }
}


file_loc = 'V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new run/'

# total legumes regional primary

do_offline(filename_in = 'model_4_TOTAL_A_OBJ.csv', model_name = 'model_4_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)

# meta analysis of consumption against estimate to see if it explains variation

mean_cons = read.csv(file='V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new run/median_TOTAL.csv')
model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new run/model_4_TOTAL_A_OBJ.csv')
model_1_alltuned=merge(x=model_1_alltuned, y=mean_cons, by = 'study')
sink(file='V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new run/meta_reg_cons.txt')
summary(lm(formula = model_1_alltuned$Estimate ~ model_1_alltuned$TOTAL))
sink()

# total legumes regional primary order by mean consumption (TOTAL)
# note that mean consumption was added manually

do_offline(filename_in = 'model_4_TOTAL_A_OBJ.csv', model_name = 'model_4_by_TOTAL_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE, orderby = 'TOTAL')




