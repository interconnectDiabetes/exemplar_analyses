

yi = log(c(0.968,0.990, 0.947, 1.234, 0.931))
upper = log(c(0.986, 1.133, 1.11, 2.03, 0.987))
lower = log(c(0.951, 0.865, 0.808, 0.751, 0.879))
sei = (upper-lower)/3.92
labels = c("DNBC", "GECKO", "HSS", "REPRO", "SWS")



res <- rma(yi = yi, sei = sei, method='DL', slab = labels)

res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

mypath = "U:\\plot1.svg"

svg(filename=mypath, 
    width=8, 
    height=6, 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(1,1))
par(ps=10)

forest(res, at=log(c(0.5,0.75,1.00,1.25,1.5,1.75,2,2.25)), digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                                                                       .(round(res$QEp,3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp)
usr <- par("usr")
text(usr[2], usr[4], "RR [95% CI]", adj = c(1, 12),cex=0.75)
dev.off()



yi = log(c(0.975, 1.042, 0.998, 1.089, 0.944))
upper = log(c(0.96, 0.929, 0.89, 0.719, 0.899))
lower = log(c(0.991, 1.169, 1.118, 1.649, 0.991))
sei = (upper-lower)/3.92
labels = c("DNBC", "GECKO", "HSS", "REPRO", "SWS")



res <- rma(yi = yi, sei = sei, method='DL', slab = labels)

res$slab <- paste(res$slab, " (", round(weights.rma.uni(res),digits=1), "%)")

mypath = "U:\\plot2.svg"

svg(filename=mypath, 
    width=8, 
    height=6, 
    pointsize=10)
par(mar=c(5,3,2,2)+0.1)
par(mfrow=c(1,1))
par(ps=10)

forest(res, at=log(c(0.5,0.75,1.00,1.25,1.5,1.75)), digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                                                                .(round(res$QEp,3)),')')),
       xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                         .(round(res$pval,3)))), atransf = exp)
usr <- par("usr")
text(usr[2], usr[4], "RR [95% CI]", adj = c(1, 12),cex=0.75)
dev.off()