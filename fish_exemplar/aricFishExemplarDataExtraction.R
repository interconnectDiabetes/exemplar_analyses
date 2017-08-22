# This file is used to create a dataframe for the fish exemplar
# Author: Paul Scherer

library(tibble) # for easier dataframes, and rather the saving to dta
library(haven)
setwd("V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/aric/main_study/CSV_DATA")


#  _____       _                                 
# |  _  |     | |                                
# | | | |_   _| |_ ___ ___  _ __ ___   ___  ___  
# | | | | | | | __/ __/ _ \| '_ ` _ \ / _ \/ __| 
# \ \_/ / |_| | || (_| (_) | | | | | |  __/\__ \ 
#  \___/ \__,_|\__\___\___/|_| |_| |_|\___||___/ 

# AGE_BASE
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
V1AGE01 = subset.data.frame(dfb, select = c(V1AGE01, PID))
V1AGE01$V1AGE01 = as.numeric(as.character(V1AGE01$V1AGE01))

# TYPE_DIAB we assume all to be t2

# PREV_DIAB
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "v1/msra.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
HOM10E = subset.data.frame(dfa, select = c(HOM10E, PID))
MSRA08F = subset.data.frame(dfb, select = c(MSRA08F, PID))
GLUCOS01 = subset.data.frame(dfc, select = c(GLUCOS01, PID))

HOM10E$HOM10E = as.factor(HOM10E$HOM10E)
levels(x = HOM10E$HOM10E) = c("No","Yes", "Not Sure")
# MSRA08F$MSRA08F = as.factor(MSRA08F$MSRA08F)
# levels(x = MSRA08F$MSRA08F) = c("No","Yes", "Not Sure")
GLUCOS01$GLUCOS01 = as.numeric(as.character(GLUCOS01$GLUCOS01))

# CASE_OBJ and CASE_OBJ_SELF
dfa = read.csv2(file = "v1/chmb.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "v1/hhxb.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "v1/msrb.csv", header = TRUE, sep = ",")
dfd = read.csv2(file = "v1/licc04.csv", header = TRUE, sep = ",")
dfe = read.csv2(file = "v1/phxa04.csv", header = TRUE, sep = ",")
dff = read.csv2(file = "v1/msrc04.csv", header = TRUE, sep = ",")
dfg = read.csv2(file = "v1/lipd04.csv", header = TRUE, sep = ",")
dfh = read.csv2(file = "v1/phxb04.csv", header = TRUE, sep = ",")
dfi = read.csv2(file = "v1/msrd04.csv", header = TRUE, sep = ",")
dfj = read.csv2(file = "v1/derv.csv", header = TRUE, sep = ",")
dfk = read.csv2(file = "v1/msr.csv", header = TRUE, sep = ",")
CHMB07 = subset.data.frame(dfa, select = c(CHMB07, PID))
HXB05D = subset.data.frame(dfb, select = c(HXB05D, PID))
MSRB24F = subset.data.frame(dfc, select = c(MSRB24F, PID))
LIPC4A = subset.data.frame(dfd, select = c(LIPC4A, PID))
PHXA8K = subset.data.frame(dfe, select = c(PHXA8K, PID))
MSRC24G = subset.data.frame(dff, select = c(MSRC24G, PID))
LIPD4A = subset.data.frame(dfg, select = c(LIPD4A, PID))
PHXB6C = subset.data.frame(dfh, select = c(PHXB6C, PID))
MSRD24G = subset.data.frame(dfi, select = c(MSRD24G, PID))
LIP23 = subset.data.frame(dfj, select = c(LIP23, PID))
MSRF33C = subset.data.frame(dfk, select = c(MSRF33C, PID))

if (CHMB07= ≥126 or ≥200mg/dl or HXB05D=yes or MSRB24F=yes) 
OR (LIPC4A= ≥126 or ≥200mg/dl or PHXA8K=yes or MSRC24G=yes) 
OR (LIPD4A= ≥126 or ≥200mg/dl or PHXB6C=yes or MSRD24G=yes) 
OR (LIP23= ≥126 or ≥200mg/dl or MSRF33C=yes)

CHMB07$CHMB07 = as.numeric(as.character(CHMB07$CHMB07))
HXB05D$HXB05D = as.factor(HXB05D$HXB05D)
levels(x = HXB05D$HXB05D) = c("No","Yes", "Not Sure")
MSRB24F$MSRB24F = as.factor(MSRB24F$MSRB24F)
levels(x = MSRB24F$MSRB24F) = c("No","Yes", "Not Sure")

LIPC4A$LIPC4A = as.numeric(as.character(LIPC4A$LIPC4A))
PHXA8K$HXB05D = as.factor(PHXA8K$PHXA8K)
levels(x = PHXA8K$PHXA8K) = c("No","Yes", "Not Sure")
MSRC24G$MSRC24G = as.factor(MSRC24G$MSRC24G)
levels(x = MSRC24G$MSRC24G) = c("No","Yes", "Not Sure")

LIPD4A$LIPD4A = as.numeric(as.character(LIPD4A$LIPD4A))
PHXB6C$PHXB6C = as.factor(PHXB6C$PHXB6C)
levels(x = PHXB6C$PHXB6C) = c("No","Yes", "Not Sure")
MSRD24G$MSRD24G = as.factor(MSRD24G$MSRD24G)
levels(x = MSRD24G$MSRD24G) = c("No","Yes", "Not Sure")

LIPD4A$LIPD4A = as.numeric(as.character(LIPD4A$LIPD4A))
PHXB6C$PHXB6C = as.factor(PHXB6C$PHXB6C)
levels(x = PHXB6C$PHXB6C) = c("No","Yes", "Not Sure")


# AGE_END_OBJ 
# AGE_END_OBJ_SELF
dfa = read.csv2(file = "v1/derive2_10.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "v1/derive37.csv", header = TRUE, sep = ",")
dfe = read.csv2(file = "v1/derive47.csv", header = TRUE, sep = ",")
dfg = read.csv2(file = "v1/derive51.csv", header = TRUE, sep = ",")
dfi = read.csv2(file = "v1/derive2_10.csv", header = TRUE, sep = ",")
dfj = read.csv2(file = "v1/derive37.csv", header = TRUE, sep = ",")
dfk = read.csv2(file = "v1/derive47.csv", header = TRUE, sep = ",")
dfl = read.csv2(file = "v1/derive51.csv", header = TRUE, sep = ",")
V2AGE22 = subset.data.frame(dfa, select = c(V2AGE22, PID))
V1AGE01 = subset.data.frame(dfb, select = c(V1AGE01, PID))
V3AGE31 = subset.data.frame(dfc, select = c(V3AGE31, PID))
V4AGE41 = subset.data.frame(dfe, select = c(V4AGE41, PID))
V5AGE51 = subset.data.frame(dfg, select = c(V1AGE01, PID))
V2DAYS = subset.data.frame(dfi, select = c(V2DAYS, PID))
V3DAYS = subset.data.frame(dfj, select = c(V3DAYS, PID))
V4DAYS = subset.data.frame(dfk, select = c(V4DAYS, PID))
V5DATE51_DAYS = subset.data.frame(dfl, select = c(V5DATE51_DAYS, PID))

V2AGE22$V2AGE22 = as.numeric(as.character(V2AGE22$V2AGE22))
V1AGE01$V1AGE01 = as.numeric(as.character(V1AGE01$V1AGE01))
V3AGE31$V3AGE31 = as.numeric(as.character(V3AGE31$V3AGE31))
V4AGE41$V4AGE41 = as.numeric(as.character(V4AGE41$V4AGE41))
V5AGE51$V5AGE51 = as.numeric(as.character(V5AGE51$V5AGE51))
V2DAYS$V2DAYS = as.numeric(as.character(V2DAYS$V2DAYS))
V3DAYS$V3DAYS = as.numeric(as.character(V3DAYS$V3DAYS))
V4DAYS$V4DAYS = as.numeric(as.character(V4DAYS$V4DAYS))
V5DATE51_DAYS$V5DATE51_DAYS = as.numeric(as.character(V5DATE51_DAYS$V5DATE51_DAYS))


# _____                                         
# |  ___|                                        
# | |____  ___ __   ___  ___ _   _ _ __ ___  ___ 
# |  __\ \/ / '_ \ / _ \/ __| | | | '__/ _ \/ __|
# | |___>  <| |_) | (_) \__ \ |_| | | |  __/\__ \
# \____/_/\_\ .__/ \___/|___/\__,_|_|  \___||___/
#           | |                                  
#           |_|                                  
# FATTY
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
DTIA35 = subset.data.frame(dfa, select = c(DTIA35, PID))
DTIA35$DTIA35 = as.numeric(as.character(DTIA35$DTIA35))
DTIA35$DTIA35 = as.factor(DTIA35$DTIA35)
levels(x = DTIA35$DTIA35) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# FRESH 
# FRIED

# LEAN
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
DTIA36 = subset.data.frame(dfa, select = c(DTIA36, PID))
DTIA36$DTIA36 = as.numeric(as.character(DTIA36$DTIA36))
DTIA36$DTIA36 = as.factor(DTIA36$DTIA36)
levels(x = DTIA36$DTIA36) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# NONFISH
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
DTIA37 = subset.data.frame(dfa, select = c(DTIA37, PID))
DTIA37$DTIA37 = as.numeric(as.character(DTIA37$DTIA37))
DTIA37$DTIA37 = as.factor(DTIA37$DTIA37)
levels(x = DTIA37$DTIA37) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# SALT
# SSD 

# TOTAL
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
DTIA34 = subset.data.frame(dfa, select = c(DTIA34, PID))
DTIA34$DTIA34 = as.numeric(as.character(DTIA34$DTIA34))
DTIA34$DTIA34 = as.factor(DTIA34$DTIA34)
levels(x = DTIA34$DTIA34) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# ___  ___          _ _  __ _               
# |  \/  |         | (_)/ _(_)              
# | .  . | ___   __| |_| |_ _  ___ _ __ ___ 
# | |\/| |/ _ \ / _` | |  _| |/ _ \ '__/ __|
# | |  | | (_) | (_| | | | | |  __/ |  \__ \
# \_|  |_/\___/ \__,_|_|_| |_|\___|_|  |___/
# SEX
dfa = read.csv2(file = "v1/aaf01.csv", header = TRUE, sep = ",")
A01SEX = subset.data.frame(dfa, select = c(A01SEX, PID))
A01SEX$A01SEX = as.factor(A01SEX$A01SEX)
levels(x = A01SEX$A01SEX) = c("Male","Female")

# BMI
dfa = read.csv2(file = "v1/aaf20.csv", header = TRUE, sep = ",")
A20BMI = subset.data.frame(dfa, select = c(A20BMI, PID))
total$A06FISH = as.numeric(as.character(total$A06FISH))
             
#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/

# Education a bit weird have to or them
dfa = read.csv2(file = "v1/aaf01.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "v1/aaf03.csv", header = TRUE, sep = ",")
A01ED1 = subset.data.frame(dfa, select = c(A01ED1, PID))
A03ED = subset.data.frame(dfb, select = c(A03ED, PID))

A01ED1$A01ED1 = as.factor(A01ED1$A01ED1)
A03ED$A03ED = as.factor(A03ED$A03ED)


# Smoking also or them
dfa = read.csv2(file = "v1/aaf10.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "v1/aaf01.csv", header = TRUE, sep = ",")
A10SMOKE = subset.data.frame(dfa, select = c(A10SMOKE, PID))
A01SMNOW = subset.data.frame(dfb, select = c(A01SMNOW, PID))

A10SMOKE$A10SMOKE = as.factor(A10SMOKE$A10SMOKE)
levels(x = A10SMOKE$A10SMOKE) = c("Non Smokers","Ex Smokers", "Smokers")
A01SMNOW$A01SMNOW = as.factor(A01SMNOW$A01SMNOW)

# PA
dfa = read.csv2(file = "v1/aaf19.csv", header = TRUE, sep = ",")
A19MODWK = subset.data.frame(dfa, select = c(A19MODWK, PID))
A19MODWK$A19MODWK = as.numeric(as.character(A19MODWK$A19MODWK))

# ALCOHOL
dfa = read.csv2(file = "v1/aaf06a.csv", header = TRUE, sep = ",")
A06ALCHL = subset.data.frame(dfa, select = c(A06ALCHL, PID))
A06ALCHL$A06ALCHL = as.numeric(as.character(A06ALCHL$A06ALCHL))

# FAM_DIAB
dfa = read.csv2(file = "v1/aaf11.csv", header = TRUE, sep = ",")
famdiab = subset.data.frame(dfa, select = c(A11MDIAB,A11FDIAB,A11BDIAB, A11SDIAB, PID))

famdiab$A11MDIAB = as.factor(famdiab$A11MDIAB)
levels(x = famdiab$A11MDIAB) = c("No","Yes", "Not Sure")
famdiab$A11FDIAB = as.factor(famdiab$A11FDIAB)
levels(x = famdiab$A11FDIAB) = c("No","Yes", "Not Sure")
famdiab$A11BDIAB = as.factor(famdiab$A11BDIAB)
levels(x = famdiab$A11BDIAB) = c("No","Yes", "Not Sure")
famdiab$A11SDIAB = as.factor(famdiab$A11SDIAB)
levels(x = famdiab$A11SDIAB) = c("No","Yes", "Not Sure")

# MI
dfa = read.csv2(file = "v1/aaf09gen.csv", header = TRUE, sep = ",")
A09HRTAK = subset.data.frame(dfa, select = c(A09HRTAK, PID))
A09HRTAK$A09HRTAK = as.factor(A09HRTAK$A09HRTAK)
levels(x = A09HRTAK$A09HRTAK) = c("No","Yes", "Not Sure")

# STROKE
# nothing here mate

# CANCER
dfa = read.csv2(file = "v1/aaf08v2.csv", header = TRUE, sep = ",")
A08CANCR = subset.data.frame(dfa, select = c(A08CANCR, PID))
A08CANCR$A08CANCR = as.factor(A08CANCR$A08CANCR)
levels(x = A08CANCR$A08CANCR) = c("No","Yes", "Not Sure") 

# HYPERTENSION
dfa = read.csv2(file = "v1/aaf08v2.csv", header = TRUE, sep = ",")
A08HBP = subset.data.frame(dfa, select = c(A08HBP, PID))
A08HBP$A08HBP = as.factor(A08HBP$A08HBP)
levels(x = A08HBP$A08HBP) = c("No","Yes", "Not Sure") 

# E_INTAKE
dfa = read.csv2(file = "v1/aaf06a.csv", header = TRUE, sep = ",")
A06CALO = subset.data.frame(dfa, select = c(A06CALO, PID))
A06CALO$A06CALO = as.numeric(as.character(A06CALO$A06CALO))

# FRUIT
dfa = read.csv2(file = "v1/aaf06mj.csv", header = TRUE, sep = ",")
A06FRUIT = subset.data.frame(dfa, select = c(A06FRUIT, PID))
A06FRUIT$A06FRUIT = as.numeric(as.character(A06FRUIT$A06FRUIT))

# VEG
dfa = read.csv2(file = "v1/aaf06mj.csv", header = TRUE, sep = ",")
A06VEGETABLE = subset.data.frame(dfa, select = c(A06VEGETABLE, PID))
A06VEGETABLE$A06VEGETABLE = as.numeric(as.character(A06VEGETABLE$A06VEGETABLE))

# FIBER
dfa = read.csv2(file = "v1/aaf06a.csv", header = TRUE, sep = ",")
A06FIBER = subset.data.frame(dfa, select = c(A06FIBER, PID))
A06FIBER$A06FIBER = as.numeric(as.character(A06FIBER$A06FIBER))

# RED_MEAT
dfa = read.csv2(file = "v1/aaf06fg.csv", header = TRUE, sep = ",")
red = subset.data.frame(dfa, select = c(A06MRF0100,A06MRF0200,A06MRF0300, A06MRF0400, PID))
red$A06MRF0100 = as.numeric(as.character(red$A06MRF0100))
red$A06MRF0200 = as.numeric(as.character(red$A06MRF0200))
red$A06MRF0300 = as.numeric(as.character(red$A06MRF0300))
red$A06MRF0400 = as.numeric(as.character(red$A06MRF0400))

# PROC-MEAT
dfa = read.csv2(file = "v1/aaf06fg.csv", header = TRUE, sep = ",")
proc = subset.data.frame(dfa, select = c(A06MCF0200,A06MCF0100 , PID))
proc$A06MCF0200 = as.numeric(as.character(proc$A06MCF0200))
proc$A06MCF0100 = as.numeric(as.character(proc$A06MCF0100))

# SUG_BEVS
dfa = read.csv2(file = "v1/aaf06fg.csv", header = TRUE, sep = ",")
A06BVS0400 = subset.data.frame(dfa, select = c(A06BVS0400, PID))
A06BVS0400$A06BVS0400 = as.numeric(as.character(A06BVS0400$A06BVS0400))

# MEDS
dfa = read.csv2(file = "v1/aaf08v2.csv", header = TRUE, sep = ",")
A08BPMED = subset.data.frame(dfa, select = c(A08BPMED, PID))
A08BPMED$A08BPMED = as.factor(A08BPMED$A08BPMED)
levels(x = A08BPMED$A08BPMED) = c("No","Yes", "Not Sure")

# WAIST
dfa = read.csv2(file = "v1/aaf20.csv", header = TRUE, sep = ",")
A20WST = subset.data.frame(dfa, select = c(A20WST, PID))
A20WST$A20WST = as.numeric(as.character(A20WST$A20WST))

rm(dfa, dfb, dfc, dfd, dfe, dff, dfg, dfh, dfi, dfj, dfk, dfl, dfm, dfn, dfo, dfp, dfq, 
	dfr, dfs, dft, dfu, dfv, dfw, dfx, dfy, dfz, dfaa, dfab, dfac, dfad)


#################################################################################################################
#################################################################################################################
#################################################################################################################
dataframes_list = list(EXAMAGE,A01AGE1,A08DIAB,A09DIBST,AL3_GLU,B08DIAB,B09DIBAG,C08DIAB,C08DIBAG,
	D08DIAB,D08DIBAG,DL7GLU,E08DIAB,E08DIBAG,EL7GLU,EL7GLU2H,F08DIAB,F08DIBAG,FL7GLU,FDIABMED,
	G08DIAB,G08DIBAG,GL7GLU,GL7GLU2H,H08DIAB,H08DIBAG,HL7GLU,HL7GLU2H,EX2_AGE,
	EX3_AGE,EX4_AGE,EX5_AGE,EX6_AGE,EX7_AGE,EX8_AGE,fatty,fried,lean,nonfish,total,A01SEX,A20BMI,
	A01ED1,A03ED,A10SMOKE,A01SMNOW,A19MODWK,A06ALCHL,famdiab,A09HRTAK,A08CANCR,A08HBP,A06CALO,
	A06FRUIT,A06VEGETABLE,A06FIBER,red,proc,A06BVS0400,A08BPMED,A20WST)

cardia = Reduce(function(...) merge(..., by="PID", all=TRUE), dataframes_list)
colnames(cardia)[1] <- "ID"

cardia_tibble = as_tibble(cardia)
save(cardia,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/cardia/cardia_r_df.Rdata")
write_dta(data = cardia_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/cardia/cardia_new.dta")

