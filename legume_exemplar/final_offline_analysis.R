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
  
  # reg_list = c(sort(c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
  #                     "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
  #                     "InterAct_denmark","FMC","HOORN", "SMC","SUN", "Whitehall", "Zutphen","ELSA_UK")),
  #                     sort(c("WHI", "CARDIA", "ARIC", "MESA", "MTC", "PRHHP", "ELSA_B")),
  #                     "Golestan", sort(c("AusDiab", "CKB", "KOGES_CAVAS", "KOGES_ASAS")))
  # 

  #temp_data = temp_data[match(temp_data$study,reg_list),] 

  
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
  text(usr[2], usr[4], "Rate Ratio [95% CI]", adj = c(1, 4),cex=0.75)
  text(usr[1], usr[3], filename_in, adj = c( 0, 0 ),cex=1)
  text(usr[1], usr[4], paste0(scaling_factor," g/day"), adj = c( 0, 1 ))
  dev.off()
  
  #Regional split 
  
  if(do_regions){
  
    regions = list()
    
    # regions[['europe']] =  data.frame("study" = c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
    #                                                "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
    #                                                "InterAct_denmark","FMC","HOORN", "SMC","SUN", "Whitehall", "Zutphen","ELSA_UK" ))
    # regions[['n_america']] = data.frame("study" = c("WHI", "CARDIA", "ARIC", "MESA"))
    # regions[['australia']] = data.frame("study" = c("AusDiab"))
    # regions[['asia']] = data.frame("study" = c("Golestan", "CKB", "KOGES_CAVAS", "KOGES_ASAS"))
    # regions[['latin']] = data.frame("study" = c("MTC", "PRHHP", "ELSA_B"))
  
     regions[['central']] =  data.frame("study" = sort(c("InterAct_france", "InterAct_italy", "InterAct_spain", "InterAct_uk",
                                                   "InterAct_netherlands", "InterAct_germany", "InterAct_sweden",
                                                   "InterAct_denmark","FMC","HOORN", "SMC","SUN", "Whitehall", "Zutphen","ELSA_UK" )))
     regions[['americas']] = data.frame("study" = sort(c("WHI", "CARDIA", "ARIC", "MESA", "MTC", "PRHHP", "ELSA_B")))
     regions[['e_med']] = data.frame("study" = c("Golestan"))
     regions[['w_pacific']] = data.frame("study" = sort(c("AusDiab", "CKB", "KOGES_CAVAS", "KOGES_ASAS")))

    
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
      text(usr[2], usr[4], "Rate Ratio [95% CI]", adj = c(1, 4),cex=0.75)
      text(usr[1], usr[3], filename_in, adj = c( 0, 0 ),cex=1)
      text(usr[1], usr[4], paste0(scaling_factor," g/day"), adj = c( 0, 1 ))
      dev.off()
    }
  }
}


file_loc = 'V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/after 2018_12_04 meeting/'

# total legumes regional primary

do_offline(filename_in = 'model_5_TOTAL_A_OBJ.csv', model_name = 'model_5_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 20, at_vect = c(0.75,0.9,1.0,1.25,1.5),
           xlim_vect = c(0.5, 2.1), alim_vect = c(0.75, 1.5), do_regions = TRUE)


# total legumes regional primary no germany

do_offline(filename_in = 'model_5_TOTAL_A_OBJ_no_DE.csv', model_name = 'model_5_no_DE_',
           exposure = 'TOTAL', outcome = 'A_OBJ',  scaling_factor = 20, at_vect = c(0.75,0.9,1.0,1.25,1.5),
           xlim_vect = c(0.5, 2.1), alim_vect = c(0.75, 1.5), do_regions = TRUE)

# total legumes regional primary no germany no sweden

do_offline(filename_in = 'model_5_TOTAL_A_OBJ_no_DE_no_SE.csv', model_name = 'model_5_no_DE_no_SE_',
           exposure = 'TOTAL', outcome = 'A_OBJ',  scaling_factor = 20, at_vect = c(0.75,0.9,1.0,1.25,1.5),
           xlim_vect = c(0.5, 2.1), alim_vect = c(0.75, 1.5), do_regions = TRUE)


# meta regression of consumption against estimate to see if it explains variation

mean_cons = read.csv(file='V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new new new run/median_TOTAL.csv')
model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new new new run/model_4_TOTAL_A_OBJ.csv')
model_1_alltuned=merge(x=model_1_alltuned, y=mean_cons, by = 'study')
sink(file='V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new new new run/meta_reg_cons.txt')
summary(lm(formula = model_1_alltuned$Estimate ~ model_1_alltuned$TOTAL))
sink()

# NEW meta regression of consumption against estimate to see if it explains variation

model_1_alltuned = read.csv('V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new new new run/model_5_TOTAL_A_OBJ.csv')
sink(file='V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new new new run/meta_reg_cons.txt')
res <- rma(yi = model_1_alltuned$Estimate, sei = model_1_alltuned$Std..Error, method='DL', mods = ~ model_1_alltuned$TOTAL, slab = model_1_alltuned$study, digits = 5)
print(res)
sink()


# total legumes primary


do_offline(filename_in = 'model_5_TOTAL_A_OBJ.csv', model_name = 'model_5_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.95,0.975,1.0,1.025,1.05),
           xlim_vect = c(0.8, 1.15), alim_vect = c(0.8, 1.2), do_regions = TRUE)

# total legumes secondary

do_offline(filename_in = 'model_5_TOTAL_A_OBJ_SELF.csv', model_name = 'model_5_',
           exposure = 'TOTAL', outcome = 'A_OBJ_SELF', scaling_factor = 1, at_vect = c(0.95,0.975,1.0,1.025,1.05),
           xlim_vect = c(0.8, 1.15), alim_vect = c(0.95, 1.05), do_regions = TRUE)



# total legumes regional primary order by mean consumption (TOTAL)
# note that mean consumption was added manually

do_offline(filename_in = 'model_4_TOTAL_A_OBJ.csv', model_name = 'model_4_by_TOTAL_',
           exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 1, at_vect = c(0.8,0.9,1.0,1.1,1.2),
           xlim_vect = c(0.6, 1.5), alim_vect = c(0.8, 1.2), do_regions = FALSE, orderby = 'TOTAL')


#rescale all plots in directory TOTAL_OBJ

file_loc = 'V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/after 2018_12_04 meeting/rescale/'

lf <- list.files(path = file_loc,full.names=T)
ld <- list.dirs(path = file_loc,recursive=F)
myfiles = lf[match(setdiff(normalizePath(lf),normalizePath(ld)), normalizePath(lf))]


for (name in 1:length(myfiles)){
  
  do_offline(filename_in = basename(myfiles[name]), model_name = basename(myfiles[name]),
             exposure = 'TOTAL', outcome = 'A_OBJ', scaling_factor = 20, at_vect = c(0.75,0.9,1.0,1.25,1.5),
             xlim_vect = c(0.5, 2.1), alim_vect = c(0.75, 1.5), do_regions = FALSE)
  
  
}

#rescale all plots in directory TOTAL OBJ_SELF

file_loc = 'V:/Studies/InterConnect/Internal/diet exemplars (n=3)/Legumes exemplar/Results/new new new run2/TOTAL/A_OBJ_SELF/'

lf <- list.files(path = file_loc,full.names=T)
ld <- list.dirs(path = file_loc,recursive=F)
myfiles = lf[match(setdiff(normalizePath(lf),normalizePath(ld)), normalizePath(lf))]


for (name in 1:length(myfiles)){
  
  do_offline(filename_in = basename(myfiles[name]), model_name = basename(myfiles[name]),
             exposure = 'TOTAL', outcome = 'A_OBJ_SELF', scaling_factor = 1, at_vect = c(0.95,0.975,1.0,1.025,1.05),
             xlim_vect = c(0.6, 1.15), alim_vect = c(0.95, 1.05), do_regions = FALSE)
  
  
}

