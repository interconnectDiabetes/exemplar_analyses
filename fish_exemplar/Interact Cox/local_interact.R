library('survival')

# List files in the directory by full path
files <- dir('/home/l_trpb2/interact', full.names=TRUE)

# read each file and assign names
df.list <- lapply(files, read.csv)
names(df.list) = c("denmark","france","germany","italy","netherlands","spain","sweden","uk")

remove_age <- function(DF) {
  # function that rebases the age groups
  new_df = data.frame(DF, start = 0, end = DF$AGE_END_OBJ - DF$AGE_BASE)
  return(new_df)
}
df.list2 <- lapply(df.list, remove_age)

cox_list <- function(DF) {
  model <- summary(coxph(Surv(start, end, CASE_OBJ)~TOTAL+AGE_BASE+EDUCATION+SMOKING, DF, robust = TRUE, ties = "breslow"))
  return(model)
}

df.list2 <- lapply(df.list, remove_age)
cox_out <- lapply(df.list2, cox_list)

coeffs = unlist(lapply(cox_out, function (x) x['coefficients'][[1]][1,1]))
s_err = unlist(lapply(cox_out, function (x) x['coefficients'][[1]][1,3]))
labels = names(cox_out)
res <- rma(yi = coeffs, sei = s_err, method='DL', slab = labels)

#add the weights to the labels
res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")
fmla <- as.formula(paste("censor"," ~ ", "TOTAL", "+", "AGE_BASE", "+", "EDUCATION", "+", "SMOKING"))

svg(filename='/home/l_pms69/exemplar_analyses/fish_exemplar/test.svg', 
    width=4 * 1, 
    height=3 * 1, 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(1,1))
par(ps=10)


  forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                          .(round(res$QEp,3)),')')),
         xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                           .(round(res$pval,3)))), atransf = exp)
  usr <- par("usr")
  text(usr[2], usr[4], "Relative Risk [95% CI]", adj = c(1, 4),cex=0.75)
  text(usr[1], usr[4], paste0(gsub(paste0('ref_table',"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=0.75)
  text(usr[1], usr[3], 'TOTAL', adj = c( 0, 0),cex=0.75)
  dev.off()
