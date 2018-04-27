#Lexis time buckets for fish exemplar

time_buckets = list(

"InterAct_germany" = c(1,1,1,1,1,1,1,1,1,1,1,1,2),
"InterAct_denmark" = c(2,2,2,1.5,5),
"InterAct_france" = c(1,1,1,1,1,1,1,1.5,1,4),
"InterAct_italy" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2),
"InterAct_netherlands" = c(1,1,1,1,1,1,1,1,1,1,1,2),
"InterAct_spain" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
"InterAct_sweden" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2),
"InterAct_uk" = c(1,1,1,1,1,1,1,1,1,1,1,2),
"HOORN" = c(2,2,2.5,2.5),
"ELSA" = c(2,2,0.5,3.5),
"NOWAC" =  c(1,1,1,1,1,1,1,2),
"SMC" = c(1,1,1,1,1,1,1,1,1,1,1,1,2),
"Zutphen" = c(1,1,1,1,1,1,1,1,2),
"Whitehall" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
"AusDiab" = c(1,1,1,1,1,1,1,1.5,1,1,1,2),
"NHAPC" =  c(1,1,1,1,1,1),
"WHI" =  c(1,1,1,1,1,1,1,1,1,1,1,1,1),
"JPHC" = c(1,1,1,1,0.5,2.5),
"ARIC" =  c(3,3,3,3,3,3,3),
"MESA" =  c(1,1,1,1,1,1),
"CARDIA" =  c(3,3,3,3,3,3,3,3),
"PRHHP" =  c(2,2,2,2),
"FMC" =  c(2,2,2,2),
"Golestan" =  c(2,2,2,2),
"CKB" =  c(2,2,2,2)

)

#temp = ds.quantileMean(x='F1$FUP_OBJ', type = 'split')
#temp2 = as.data.frame(temp)[-c(1,7,8),]
#temp3 = diff(as.matrix(temp2))
#temp4 = round(rbind(temp2[1,], temp3), digits = 2)
#temp5 = lapply(as.list(temp4),FUN = paste0, collapse = ',')


time_buckets = list(
  
  "InterAct_germany" = c(2.23,2.63,4.67,1.71,1.09),
  "InterAct_denmark" = c(3.2,3.15,4.02,1.25,0.78),
  "InterAct_france" = c(3.65,3.63,2.02,1.22,0.75),
  "InterAct_italy" = c(4.17,2.67,4.04,1.81,0.94),
  "InterAct_netherlands" = c(3.2,3.22,4.68,1.52,0.89),
  "InterAct_spain" = c(5.8,3.17,3.51,1.13,0.81),
  "InterAct_sweden" = c(5.22,4.15,2.68,1.61,1.49),
  "InterAct_uk" = c(4.29,2.04,4.24,1.63,0.98),
  "HOORN" = c(2,2,2.5,2.5),
  "ELSA" = c(2,2,0.5),
  "NOWAC" =  c(5,1,1,1),
  "SMC" = c(10,0.5,0.5,0.5),
  "Zutphen" = c(5.23,0.07,4.9,0.13,4.92),
  "Whitehall" = c(10.24,5.19,0.75,0.38,0.21),
  "AusDiab" = c(4.95,6.66,0.5),
  "NHAPC" =  c(3,3),
  "WHI" =  c(4.96,2.93,3.95,1.85,0.68),
  "JPHC" = c(4,0.5,2.5),
  "ARIC" =  c(8.76,0.13,2.94,11.79,0.94),
  "MESA" =  c(2,2,1),
  "CARDIA" =  c(7,12,6,10),
  "PRHHP" =  c(5,5),
  "FMC" =  c(10.68,11.84,1.73,1.54,1.12),
  "Golestan" =  c(2.83,0.81,0.59,1.39,1.02),
  "CKB" =  c(5.75,0.57,0.9,0.94,0.6)
  
)