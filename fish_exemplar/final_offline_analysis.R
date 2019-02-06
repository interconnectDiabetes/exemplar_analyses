# This code can be used to generate tidied and scaled forest plots from the csv
# files saved during the DataSHIELD analyses

library('metafor')

do_offline <- function(filename_in, model_name, exposure, outcome, scaling_factor,
                       at_vect, xlim_vect, alim_vect, do_regions = FALSE){

  model_1_alltuned = read.csv(paste0(file_loc,filename_in))
  
  model_1_alltuned = model_1_alltuned[,-1]
  colnames(model_1_alltuned)[5] = c('Std. Error')
  model_1_alltuned = model_1_alltuned[order(model_1_alltuned$study),]
  
  my_exposure = exposure
  
  ref_table =  outcome
  
  my_studies =  unique(as.data.frame(model_1_alltuned[,1]))
  colnames(my_studies) = 'study'
  
  
  for_RMA = model_1_alltuned[model_1_alltuned$cov==my_exposure,]
  
  temp_data = merge(x = my_studies, y = for_RMA, by = "study")
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
    
    regions[['central']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                                   "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                                   "InterAct_denmark","FMC","HOORN", "NOWAC", "SMC","SUN", "Whitehall", "Zutphen"))
    regions[['americas']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA", "PRHHP", "ELSA"))
    regions[['w_pacific']] = data.frame("study" = c("AusDiab", "NHAPC", "JPHC", "CKB", "SWHS", "SMHS"))
    regions[['e_med']] = data.frame("study" = c("Golestan"))
  
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


file_loc = 'V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_05_16/really final/'

#model 4 sex interaction term

do_offline(filename_in = 'model_4_TOTAL_A_OBJ.csv', model_name = 'model_4_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#men total fish regional primary

do_offline(filename_in = 'model_4_men_TOTAL_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)

#men total fish regional primary no IA Netherlands

do_offline(filename_in = 'model_4_men_TOTAL_A_OBJ_men_sensit.csv', model_name = 'model_4_men_sensit_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#women total fish regional primary

do_offline(filename_in = 'model_4_women_TOTAL_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)

#women total fish regional primary no IA Netherlands France

do_offline(filename_in = 'model_4_women_TOTAL_A_OBJ_women_sensit.csv', model_name = 'model_4_women_sensit_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)

#women total fish regional primary no IA  France

do_offline(filename_in = 'model_4_women_TOTAL_A_OBJ_women_no_france.csv', model_name = 'model_4_women_no_france_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)

#women total fish regional primary no IA Netherlands

do_offline(filename_in = 'model_4_women_TOTAL_A_OBJ_women_no_neth.csv', model_name = 'model_4_women_no_neth_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#men fatty primary

do_offline(filename_in = 'model_4_men_FATTY_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'FATTY', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#women fatty primary

do_offline(filename_in = 'model_4_women_FATTY_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'FATTY', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#men lean primary

do_offline(filename_in = 'model_4_men_LEAN_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'LEAN', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#women lean primary

do_offline(filename_in = 'model_4_women_LEAN_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'LEAN', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#men fried primary

do_offline(filename_in = 'model_4_men_FRIED_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'FRIED', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#women fried primary

do_offline(filename_in = 'model_4_women_FRIED_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'FRIED', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)

#men servings2 primary

do_offline(filename_in = 'model_4_men_SERVINGS_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'SERVINGS2', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.4,0.6,1.0,1.6,2.4),
           xlim_vect = c(0.1, 6), alim_vect = c(0.4, 2.4), do_regions = FALSE)


#women servings2 primary

do_offline(filename_in = 'model_4_women_SERVINGS_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'SERVINGS2', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.4,0.6,1.0,1.6,2.4),
           xlim_vect = c(0.1, 6), alim_vect = c(0.4, 2.4), do_regions = FALSE)


#men servings3 primary

do_offline(filename_in = 'model_4_men_SERVINGS_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'SERVINGS3', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.4,0.6,1.0,1.6,2.4),
           xlim_vect = c(0.1, 6), alim_vect = c(0.4, 2.4), do_regions = FALSE)


#women servings3 primary

do_offline(filename_in = 'model_4_women_SERVINGS_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'SERVINGS3', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.4,0.6,1.0,1.6,2.4),
           xlim_vect = c(0.1, 6), alim_vect = c(0.4, 2.4), do_regions = FALSE)

#men servings4 primary

do_offline(filename_in = 'model_4_men_SERVINGS_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'SERVINGS4', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.4,0.6,1.0,1.6,2.4),
           xlim_vect = c(0.1, 6), alim_vect = c(0.4, 2.4), do_regions = FALSE)

#women servings4 primary

do_offline(filename_in = 'model_4_women_SERVINGS_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'SERVINGS4', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.4,0.6,1.0,1.6,2.4),
           xlim_vect = c(0.1, 6), alim_vect = c(0.4, 2.4), do_regions = FALSE)

#men nonfish primary

do_offline(filename_in = 'model_4_men_NONFISH_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'NONFISH', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.6,0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(1, 2), alim_vect = c(0.6, 1.2), do_regions = FALSE)


#women nonfish primary

do_offline(filename_in = 'model_4_women_NONFISH_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'NONFISH', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#men salt primary

do_offline(filename_in = 'model_4_men_SALT_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'SALT', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#women salt primary

do_offline(filename_in = 'model_4_women_SALT_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'SALT', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#men fresh primary

do_offline(filename_in = 'model_4_men_FRESH_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'FRESH', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#women fresh primary

do_offline(filename_in = 'model_4_women_FRESH_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'FRESH', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#men ssd primary

do_offline(filename_in = 'model_4_men_SSD_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'SSD', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#women ssd primary

do_offline(filename_in = 'model_4_women_SSD_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'SSD', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#men total secondary

do_offline(filename_in = 'model_4_men_TOTAL_A_OBJ_SELF_men.csv', model_name = 'model_4_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ_SELF', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#women total secondary

do_offline(filename_in = 'model_4_women_TOTAL_A_OBJ_SELF_women.csv', model_name = 'model_4_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ_SELF', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#women fatty primary by area

do_offline(filename_in = 'model_4_women_FATTY_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'FATTY', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#women lean primary by area

do_offline(filename_in = 'model_4_women_LEAN_A_OBJ_women.csv', model_name = 'model_4_women_',
           exposure = 'LEAN', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)

#men total fish regional primary FAM DIAB

do_offline(filename_in = 'model_3a_men_FAM_DIAB_TOTAL_A_OBJ_men.csv', model_name = 'model_3a_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#women total fish regional primary FAM DIAB

do_offline(filename_in = 'model_3a_women_FAM_DIAB_TOTAL_A_OBJ_women.csv', model_name = 'model_3a_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#men total fish regional primary WAIST

do_offline(filename_in = 'model_3b_men_WAIST_TOTAL_A_OBJ_men.csv', model_name = 'model_3b_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#women total fish regional primary WAIST

do_offline(filename_in = 'model_3b_women_WAIST_TOTAL_A_OBJ_women.csv', model_name = 'model_3b_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#men total fish regional primary SUPPLEMENTS

do_offline(filename_in = 'model_3c_men_SUPPLEMENTS_TOTAL_A_OBJ_men.csv', model_name = 'model_3c_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#women total fish regional primary SUPPLEMENTS

do_offline(filename_in = 'model_3c_women_SUPPLEMENTS_TOTAL_A_OBJ_women.csv', model_name = 'model_3c_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#men total fish regional primary model 2 matching FAM DIAB

do_offline(filename_in = 'model_2a_men_FAM_DIAB_TOTAL_A_OBJ_men.csv', model_name = 'model_2a_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#women total fish regional primary model 2 matching  FAM DIAB

do_offline(filename_in = 'model_2a_women_FAM_DIAB_TOTAL_A_OBJ_women.csv', model_name = 'model_2a_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#men total fish regional primary model 2 matching  WAIST

do_offline(filename_in = 'model_2b_men_WAIST_TOTAL_A_OBJ_men.csv', model_name = 'model_2b_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#women total fish regional primary model 2 matching  WAIST

do_offline(filename_in = 'model_2b_women_WAIST_TOTAL_A_OBJ_women.csv', model_name = 'model_2b_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#men total fish regional primary model 2 matching  SUPPLEMENTS

do_offline(filename_in = 'model_2c_men_SUPPLEMENTS_TOTAL_A_OBJ_men.csv', model_name = 'model_2c_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

#women total fish regional primary model 2 matching  SUPPLEMENTS

do_offline(filename_in = 'model_2c_women_SUPPLEMENTS_TOTAL_A_OBJ_women.csv', model_name = 'model_2c_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


do_offline(filename_in = 'model_4_men_TOTAL_A_OBJ_men.csv', model_name = 'model_4_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = TRUE)


#women and men total fish secondary model 1

do_offline(filename_in = 'model_1_women_TOTAL_A_OBJ_SELF_women.csv', model_name = 'model_1_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ_SELF', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


do_offline(filename_in = 'model_1_men_TOTAL_A_OBJ_SELF_men.csv', model_name = 'model_1_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ_SELF', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


#women and men total fish primary model 1

do_offline(filename_in = 'model_1_women_TOTAL_A_OBJ_women.csv', model_name = 'model_1_women_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)


do_offline(filename_in = 'model_1_men_TOTAL_A_OBJ_men.csv', model_name = 'model_1_men_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 100/7, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE)

# meta analysis of age against estimate to see if it explains variation

model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_05_16/really final/model_4_women_TOTAL_A_OBJ_women.csv')
sink(file='V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_05_16/really final/meta_reg_age_women.txt')
res <- rma(yi = model_1_alltuned$Estimate, sei = model_1_alltuned$Std..Error, method='DL', mods = ~ model_1_alltuned$AGE_BASE, slab = model_1_alltuned$study, digits = 5)
print(res)
sink()

# meta analysis of region against estimate to see if it explains variation

model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_05_16/really final/model_4_women_TOTAL_A_OBJ_women.csv')
sink(file='V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_05_16/really final/meta_reg_region_women.txt')
res <- rma(yi = model_1_alltuned$Estimate, sei = model_1_alltuned$Std..Error, method='DL', mods = ~ model_1_alltuned$REGION , slab = model_1_alltuned$study, digits = 5)
print(res)
sink()

# meta analysis of mean BMI against estimate to see if it explains variation

model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_05_16/really final/model_4_women_TOTAL_A_OBJ_women.csv')
sink(file='V:/Studies/InterConnect/Internal/Fish exemplar/Analysis/2018_05_16/really final/meta_reg_bmi_women.txt')
res <- rma(yi = model_1_alltuned$Estimate*100/7, sei = model_1_alltuned$Std..Error*100/7, method='DL', mods = ~ model_1_alltuned$BMI, slab = model_1_alltuned$study, digits = 5)
print(res)
sink()

