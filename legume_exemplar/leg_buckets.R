#Lexis time buckets for legumes exemplar

# time_buckets = list(
# 
# "InterAct_germany" = c(1,1,1,1,1,1,1,1,1,1,1,1,2),
# "InterAct_denmark" = c(2,2,2,1.5,5),
# "InterAct_france" = c(1,1,1,1,1,1,1,1.5,1,4),
# "InterAct_italy" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2),
# "InterAct_netherlands" = c(1,1,1,1,1,1,1,1,1,1,1,2),
# "InterAct_spain" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
# "InterAct_sweden" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2),
# "InterAct_uk" = c(1,1,1,1,1,1,1,1,1,1,1,2),
# "HOORN" = c(2,2,2.5,2.5),
# "ELSA_B" = c(2,2,0.5,3.5),
# "NOWAC" =  c(1,1,1,1,1,1,1,2),
# "SMC" = c(1,1,1,1,1,1,1,1,1,1,1,1,2),
# "Zutphen" = c(1,1,1,1,1,1,1,1,2),
# "Whitehall" = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
# #"AusDiab" = c(1,1,1,1,1,1,1,1.5,1,1,1,2),
# #"AusDiab" = c(4,2,2.5,2,1,2),
# "AusDiab" = c(3,5,6),
# "NHAPC" =  c(1,1,1,1,1,1),
# "WHI" =  c(1,1,1,1,1,1,1,1,1,1,1,1,1),
# "JPHC" = c(1,1,1,1,0.5,2.5),
# "ARIC" =  c(3,3,3,3,3,3,3),
# "MESA" =  c(1,1,1,1,1,1),
# "CARDIA" =  c(3,3,3,3,3,3,3,3),
# "PRHHP" =  c(2,2,2,2),
# "FMC" =  c(2,2,2,2),
# "Golestan" =  c(2,2,2,2),
# "CoLaus" =  c(6,1),
# "CKB" =  c(2,2,2,2)
# 
# )

#temp = ds.quantileMean(x='F1$FUP_OBJ', type = 'split')
#temp2 = as.data.frame(temp)[-c(1,7,8),]
#temp3 = diff(as.matrix(temp2))
#temp4 = round(rbind(temp2[1,], temp3), digits = 2)
#temp5 = lapply(as.list(temp4),FUN = paste0, collapse = ',')

time_buckets = list(

"InterAct_germany" = c(1.97,2.7,5.06,1.82,1.06),
"InterAct_denmark" = c(3.95,2.81,3.91,0.98,0.75),
"InterAct_france" = c(4.11,3.73,1.64,1.04,0.75),
"InterAct_italy" = c(4.22,2.7,3.99,1.81,0.92),
"InterAct_netherlands" = c(3.2,3.15,4.76,1.53,0.9),
"InterAct_spain" = c(5.81,3.16,3.52,1.13,0.81),
"InterAct_sweden" = c(5.85,3.66,2.44,1.35,1.53),
"InterAct_uk" = c(4.2,1.95,4.23,1.89,1.17),
"HOORN" = c(2,2,2.5,2.5),
"ELSA_B" = c(2,2,0.5),
"SMC" = c(10,0.5,0.5,0.5),
"Zutphen" = c(5.23,0.07,4.9,0.13,4.92),
"Whitehall" = c(10.24,5.19,0.75,0.38,0.21),
"AusDiab" = c(4.95,6.66,0.5),
"WHI" =  c(5.94,2.05,3.9,1.82,0.82),
"JPHC" = c(1,1,1,1,0.5,2.5),
"ARIC" =  c(8.78,0.12,3.05,11.69,0.91),
"MESA" =  c(2,2,1),
"CARDIA" =  c(7,12,6,10),
"PRHHP" =  c(5,5),
"FMC" =  c(10.68,11.85,1.72,1.55,1.12),
"Golestan" =  c(2.83,0.8,0.60,1.38,1.02),
"CoLaus" =  c(5.25,0.06,0.11,0.21),
"CKB" =  c(2,2,2,2),
"SUN" = c(2.37, 2.48, 3.88, 4.25),
"KOGES_CAVAS" = c(1.03,2.23,1.04,1.21,0.77),
"KOGES_ASAS" = c(0.87,4.12,2.75,0.17,0.08)
)


