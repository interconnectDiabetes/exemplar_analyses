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
# V1AGE01$V1AGE01 = as.numeric(as.character(V1AGE01$V1AGE01))

# TYPE_DIAB we assume all to be t2

# PREV_DIAB
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "v1/msra.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
# HOM10E = subset.data.frame(dfa, select = c(HOM10E, PID))
# MSRA08F = subset.data.frame(dfb, select = c(MSRA08F, PID))
# GLUCOS01 = subset.data.frame(dfc, select = c(GLUCOS01, PID))

HOM10E$HOM10E = as.factor(HOM10E$HOM10E)
levels(x = HOM10E$HOM10E) = c("No","Yes", "Not Sure")
# MSRA08F$MSRA08F = as.factor(MSRA08F$MSRA08F)
# levels(x = MSRA08F$MSRA08F) = c("No","Yes", "Not Sure")
# GLUCOS01$GLUCOS01 = as.numeric(as.character(GLUCOS01$GLUCOS01))

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

# CHMB07$CHMB07 = as.numeric(as.character(CHMB07$CHMB07))
# HXB05D$HXB05D = as.factor(HXB05D$HXB05D)
# levels(x = HXB05D$HXB05D) = c("No","Yes", "Not Sure")
# MSRB24F$MSRB24F = as.factor(MSRB24F$MSRB24F)
# levels(x = MSRB24F$MSRB24F) = c("No","Yes", "Not Sure")

# LIPC4A$LIPC4A = as.numeric(as.character(LIPC4A$LIPC4A))
# PHXA8K$HXB05D = as.factor(PHXA8K$PHXA8K)
# levels(x = PHXA8K$PHXA8K) = c("No","Yes", "Not Sure")
# MSRC24G$MSRC24G = as.factor(MSRC24G$MSRC24G)
# levels(x = MSRC24G$MSRC24G) = c("No","Yes", "Not Sure")

# LIPD4A$LIPD4A = as.numeric(as.character(LIPD4A$LIPD4A))
# PHXB6C$PHXB6C = as.factor(PHXB6C$PHXB6C)
# levels(x = PHXB6C$PHXB6C) = c("No","Yes", "Not Sure")
# MSRD24G$MSRD24G = as.factor(MSRD24G$MSRD24G)
# levels(x = MSRD24G$MSRD24G) = c("No","Yes", "Not Sure")

# LIPD4A$LIPD4A = as.numeric(as.character(LIPD4A$LIPD4A))
# PHXB6C$PHXB6C = as.factor(PHXB6C$PHXB6C)
# levels(x = PHXB6C$PHXB6C) = c("No","Yes", "Not Sure")


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

# V2AGE22$V2AGE22 = as.numeric(as.character(V2AGE22$V2AGE22))
# V1AGE01$V1AGE01 = as.numeric(as.character(V1AGE01$V1AGE01))
# V3AGE31$V3AGE31 = as.numeric(as.character(V3AGE31$V3AGE31))
# V4AGE41$V4AGE41 = as.numeric(as.character(V4AGE41$V4AGE41))
# V5AGE51$V5AGE51 = as.numeric(as.character(V5AGE51$V5AGE51))
# V2DAYS$V2DAYS = as.numeric(as.character(V2DAYS$V2DAYS))
# V3DAYS$V3DAYS = as.numeric(as.character(V3DAYS$V3DAYS))
# V4DAYS$V4DAYS = as.numeric(as.character(V4DAYS$V4DAYS))
# V5DATE51_DAYS$V5DATE51_DAYS = as.numeric(as.character(V5DATE51_DAYS$V5DATE51_DAYS))


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
# DTIA35$DTIA35 = as.factor(DTIA35$DTIA35)
# levels(x = DTIA35$DTIA35) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# FRESH 
# FRIED

# LEAN
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
DTIA36 = subset.data.frame(dfa, select = c(DTIA36, PID))
# DTIA36$DTIA36 = as.factor(DTIA36$DTIA36)
# levels(x = DTIA36$DTIA36) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# NONFISH
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
DTIA37 = subset.data.frame(dfa, select = c(DTIA37, PID))
# DTIA37$DTIA37 = as.factor(DTIA37$DTIA37)
# levels(x = DTIA37$DTIA37) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# SALT
# SSD 

# TOTAL
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
DTIA34 = subset.data.frame(dfa, select = c(DTIA34, PID))
# DTIA34$DTIA34 = as.factor(DTIA34$DTIA34)
# levels(x = DTIA34$DTIA34) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# ___  ___          _ _  __ _               
# |  \/  |         | (_)/ _(_)              
# | .  . | ___   __| |_| |_ _  ___ _ __ ___ 
# | |\/| |/ _ \ / _` | |  _| |/ _ \ '__/ __|
# | |  | | (_) | (_| | | | | |  __/ |  \__ \
# \_|  |_/\___/ \__,_|_|_| |_|\___|_|  |___/
# SEX
dfa = read.csv2(file = "v1/derive47.csv", header = TRUE, sep = ",")
GENDER = subset.data.frame(dfa, select = c(GENDER, PID))
# GENDER$GENDER = as.factor(GENDER$GENDER)
# levels(x = GENDER$GENDER) = c("Male","Female")

# BMI
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
BMI01 = subset.data.frame(dfa, select = c(BMI01, PID))
# BMI01$BMI01 = as.numeric(as.character(BMI01$BMI01))
             
#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/

# Education a bit weird have to or them
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
ELEVEL02 = subset.data.frame(dfa, select = c(ELEVEL02, PID))
# ELEVEL02$ELEVEL02 = as.factor(ELEVEL02$ELEVEL02)

# Smoking
# PA

# ALCOHOL
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
ETHANL03 = subset.data.frame(dfa, select = c(ETHANL03, PID))
# ETHANL03$ETHANL03 = as.numeric(as.character(ETHANL03$ETHANL03))

# FAM_DIAB
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
famdiab = subset.data.frame(dfa, select = c(MOMHISTORYDIA, DADHISTORYDIA, PID))
# famdiab$MOMHISTORYDIA = as.factor(famdiab$MOMHISTORYDIA)
# levels(x = famdiab$MOMHISTORYDIA) = c("No","Yes", "Not Sure")
# famdiab$DADHISTORYDIA = as.factor(famdiab$DADHISTORYDIA)
# levels(x = famdiab$DADHISTORYDIA) = c("No","Yes", "Not Sure")

# MI
dfa = read.csv2(file = "v1/mhxa02.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",")
MHXA28 = subset.data.frame(dfa, select = c(MHXA28, PID))
# MHXA28$MHXA28 = as.factor(MHXA28$MHXA28)
# levels(x = MHXA28$MHXA28) = c("No","Unsure", "Yes")

HOM10C = subset.data.frame(dfb, select = c(HOM10C, PID))
# HOM10C$HOM10C = as.factor(HOM10C$HOM10C)
# levels(x = HOM10C$HOM10C) = c("No","Unsure", "Yes")

# STROKE
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",")
HOM10D = subset.data.frame(dfa, select = c(HOM10D, PID))
# HOM10D$HOM10D = as.factor(HOM10D$HOM10D)
# levels(x = HOM10D$HOM10D) = c("No","Unsure", "Yes")

# CANCER
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",")
HOM10F = subset.data.frame(dfa, select = c(HOM10F, PID))
# HOM10F$HOM10F = as.factor(HOM10F$HOM10F)
# levels(x = HOM10F$HOM10F) = c("No","Unsure", "Yes")

# HYPERTENSION
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",")
HOM10A = subset.data.frame(dfa, select = c(HOM10A, PID))
# HOM10A$HOM10A = as.factor(HOM10A$HOM10A)
# levels(x = HOM10A$HOM10A) = c("No","Unsure", "Yes")

# E_INTAKE
dfa = read.csv2(file = "v1/totnutx.csv", header = TRUE, sep = ",")
TCAL = subset.data.frame(dfa, select = c(TCAL, PID))
# TCAL$TCAL = as.numeric(as.character(TCAL$TCAL))

# FRUIT
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
fruit = subset.data.frame(dfa, select = c(DTIA11,DTIA12,DTIA13, DTIA14, PID))
# fruit$DTIA11 = as.factor(fruit$DTIA11)
# levels(x = fruit$DTIA11) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# fruit$DTIA12 = as.factor(fruit$DTIA12)
# levels(x = fruit$DTIA12) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# fruit$DTIA13 = as.factor(fruit$DTIA13)
# levels(x = fruit$DTIA13) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# fruit$DTIA14 = as.factor(fruit$DTIA14)
# levels(x = fruit$DTIA14) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")



# VEG
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
veg = subset.data.frame(dfa, select = c(DTIA15,DTIA16,DTIA17, DTIA18, DTIA19, DTIA20, DTIA21, 
	DTIA22, DTIA23, DTIA24, DTIA25, PID))
# veg$DTIA15 = as.factor(veg$DTIA15)
# levels(x = veg$DTIA15) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA16 = as.factor(veg$DTIA16)
# levels(x = veg$DTIA16) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA17 = as.factor(veg$DTIA17)
# levels(x = veg$DTIA17) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA18 = as.factor(veg$DTIA18)
# levels(x = veg$DTIA18) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA19 = as.factor(veg$DTIA19)
# levels(x = veg$DTIA19) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA20 = as.factor(veg$DTIA20)
# levels(x = veg$DTIA20) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA21 = as.factor(veg$DTIA21)
# levels(x = veg$DTIA21) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA22 = as.factor(veg$DTIA22)
# levels(x = veg$DTIA22) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA23 = as.factor(veg$DTIA23)
# levels(x = veg$DTIA23) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA24 = as.factor(veg$DTIA24)
# levels(x = veg$DTIA24) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# veg$DTIA25 = as.factor(veg$DTIA25)
# levels(x = veg$DTIA25) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")


# DAIRY

# FIBER
dfa = read.csv2(file = "v1/totnutx.csv", header = TRUE, sep = ",")
DFIB = subset.data.frame(dfa, select = c(DFIB, PID))
# DFIB$DFIB = as.numeric(as.character(DFIB$DFIB))

# RED_MEAT
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
red = subset.data.frame(dfa, select = c(DTIA32, DTIA33, PID))
# red$DTIA32 = as.factor(red$DTIA32)
# levels(x = red$DTIA32) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# red$DTIA33 = as.factor(red$DTIA33)
# levels(x = red$DTIA33) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")



# PROC-MEAT
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
proc = subset.data.frame(dfa, select = c(DTIA28,DTIA29,DTIA30, DTIA31, PID))
# proc$DTIA28 = as.factor(proc$DTIA28)
# levels(x = proc$DTIA28) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# proc$DTIA29 = as.factor(proc$DTIA29)
# levels(x = proc$DTIA29) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# proc$DTIA30 = as.factor(proc$DTIA30)
# levels(x = proc$DTIA30) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# proc$DTIA31 = as.factor(proc$DTIA31)
# levels(x = proc$DTIA31) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")


# SUG_BEVS
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",")
bevs = subset.data.frame(dfa, select = c(DTIA64, DTIA65, PID))
# bevs$DTIA64 = as.factor(bevs$DTIA64)
# levels(x = bevs$DTIA64) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
# bevs$DTIA65 = as.factor(bevs$DTIA65)
# levels(x = bevs$DTIA65) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# MEDS
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",")
HYPTMDCODE01 = subset.data.frame(dfa, select = c(HYPTMDCODE01, PID))
# HYPTMDCODE01$HYPTMDCODE01 = as.factor(HYPTMDCODE01$HYPTMDCODE01)
# levels(x = HYPTMDCODE01$HYPTMDCODE01) = c("No","Yes", "Not Sure")

dfa = read.csv2(file = "v1/anta.csv", header = TRUE, sep = ",")
# ANTA07A = subset.data.frame(dfa, select = c(ANTA07A, PID))
# ANTA07A$ANTA07A = as.numeric(as.character(ANTA07A$ANTA07A))

rm(dfa, dfb, dfc, dfd, dfe, dff, dfg, dfh, dfi, dfj, dfk, dfl)


#################################################################################################################
#################################################################################################################
#################################################################################################################
dataframes_list = list(HOM10E,MSRA08F,GLUCOS01,CHMB07,HXB05D,MSRB24F,LIPC4A,PHXA8K,MSRC24G,LIPD4A,PHXB6C,MSRD24G,
	LIP23,MSRF33C,V2AGE22,V1AGE01,V3AGE31,V4AGE41,V5AGE51,V2DAYS,V3DAYS,V4DAYS,V5DATE51_DAYS,DTIA35,DTIA36,DTIA37,
	DTIA34,GENDER,BMI01,ELEVEL02,ETHANL03,famdiab,MHXA28,HOM10C,HOM10D,HOM10F,HOM10A,TCAL,fruit,veg,DFIB,red,proc,
	bevs,HYPTMDCODE01,ANTA07A)

aric = Reduce(function(...) merge(..., by="PID", all=TRUE), dataframes_list)
colnames(aric)[1] <- "ID"

aric_tibble = as_tibble(aric)
# WAIST
save(aric,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/cardia/cardia_r_df.Rdata")
write_dta(data = aric_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/cardia/cardia_new.dta")

