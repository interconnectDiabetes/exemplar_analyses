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
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
V1AGE01 = subset.data.frame(dfa, select = c(V1AGE01, ID_C))
V1AGE01$V1AGE01 = as.numeric(as.character(V1AGE01$V1AGE01))

# TYPE_DIAB we assume all to be t2

# PREV_DIAB
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfb = read.csv2(file = "v1/msra.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfc = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
HOM10E = subset.data.frame(dfa, select = c(HOM10E, ID_C))
MSRA08F = subset.data.frame(dfb, select = c(MSRA08F, ID_C))
GLUCOS01 = subset.data.frame(dfc, select = c(GLUCOS01, ID_C))

HOM10E$HOM10E = as.factor(HOM10E$HOM10E)
levels(x = HOM10E$HOM10E) = c("No", "Unsure", "Yes")
MSRA08F$MSRA08F = as.factor(MSRA08F$MSRA08F)
levels(x = MSRA08F$MSRA08F) = c("No", "Unsure", "Yes")
GLUCOS01$GLUCOS01 = as.numeric(as.character(GLUCOS01$GLUCOS01))

# CASE_OBJ and CASE_OBJ_SELF
dfa = read.csv2(file = "v2/chmb.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfb = read.csv2(file = "v2/hhxb.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfc = read.csv2(file = "v2/msrb.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfd = read.csv2(file = "v3/lipc04.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfe = read.csv2(file = "v3/phxa04.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dff = read.csv2(file = "v3/msrc04.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfg = read.csv2(file = "v4/lipd04.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfh = read.csv2(file = "v4/phxb04.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfi = read.csv2(file = "v4/msrd04.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfj = read.csv2(file = "v5/derv.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfk = read.csv2(file = "v5/msr.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
CHMB07 = subset.data.frame(dfa, select = c(CHMB07, ID_C))
HHXB05D = subset.data.frame(dfb, select = c(HHXB05D, ID_C))
MSRB24F = subset.data.frame(dfc, select = c(MSRB24F, ID_C))
LIPC4A = subset.data.frame(dfd, select = c(LIPC4A, ID_C))
PHXA8K = subset.data.frame(dfe, select = c(PHXA8K, ID_C))
MSRC24G = subset.data.frame(dff, select = c(MSRC24G, ID_C))
LIPD4A = subset.data.frame(dfg, select = c(LIPD4A, ID_C))
PHXB6C = subset.data.frame(dfh, select = c(PHXB6C, ID_C))
MSRD24G = subset.data.frame(dfi, select = c(MSRD24G, ID_C))
LIP23 = subset.data.frame(dfj, select = c(FASTING_GLUCOSE, ID_C))
MSRF33C = subset.data.frame(dfk, select = c(MSRF33C, ID_C))

CHMB07$CHMB07 = as.numeric(as.character(CHMB07$CHMB07))
HHXB05D$HHXB05D = as.factor(HHXB05D$HHXB05D)
levels(x = HHXB05D$HHXB05D) = c("No", "Unsure", "Yes")
MSRB24F$MSRB24F = as.factor(MSRB24F$MSRB24F)
levels(x = MSRB24F$MSRB24F) = c("No", "Unsure", "Yes")

LIPC4A$LIPC4A = as.numeric(as.character(LIPC4A$LIPC4A))
PHXA8K$HXB05D = as.factor(PHXA8K$PHXA8K)
levels(x = PHXA8K$PHXA8K) = c("No", "Unsure", "Yes")
MSRC24G$MSRC24G = as.factor(MSRC24G$MSRC24G)
levels(x = MSRC24G$MSRC24G) = c("No", "Unsure", "Yes")

LIPD4A$LIPD4A = as.numeric(as.character(LIPD4A$LIPD4A))
PHXB6C$PHXB6C = as.factor(PHXB6C$PHXB6C)
levels(x = PHXB6C$PHXB6C) = c("No", "Unsure", "Yes")
MSRD24G$MSRD24G = as.factor(MSRD24G$MSRD24G)
levels(x = MSRD24G$MSRD24G) = c("No", "Unsure", "Yes")

LIPD4A$LIPD4A = as.numeric(as.character(LIPD4A$LIPD4A))
PHXB6C$PHXB6C = as.factor(PHXB6C$PHXB6C)
levels(x = PHXB6C$PHXB6C) = c("No", "Unsure", "Yes")


# AGE_END_OBJ 
# AGE_END_OBJ_SELF
dfa = read.csv2(file = "v2/derive2_10.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfb = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfc = read.csv2(file = "v3/derive37.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfe = read.csv2(file = "v4/derive47.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfg = read.csv2(file = "v5/derive51.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfi = read.csv2(file = "v2/derive2_10.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfj = read.csv2(file = "v3/derive37.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfk = read.csv2(file = "v4/derive47.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfl = read.csv2(file = "v5/derive51.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
V2AGE22 = subset.data.frame(dfa, select = c(V2AGE22, ID_C))
V1AGE01 = subset.data.frame(dfb, select = c(V1AGE01, ID_C))
V3AGE31 = subset.data.frame(dfc, select = c(V3AGE31, ID_C))
V4AGE41 = subset.data.frame(dfe, select = c(V4AGE41, ID_C))
V5AGE51 = subset.data.frame(dfg, select = c(V5AGE51, ID_C))
V2DAYS = subset.data.frame(dfi, select = c(V2DAYS, ID_C))
V3DAYS = subset.data.frame(dfj, select = c(V3DAYS, ID_C))
V4DAYS = subset.data.frame(dfk, select = c(V4DAYS, ID_C))
V5DATE51_DAYS = subset.data.frame(dfl, select = c(V5DATE51_DAYS, ID_C))

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

#TOTAL (all variables needed)
#DTIA24 + DTIA15 + DTIA21 + DTIA55 + DTIA52

dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA = subset.data.frame(dfa, select = c(DTIA24, DTIA15, DTIA21, DTIA55, DTIA52, ID_C))
DTIA$DTIA24 = as.factor(DTIA$DTIA24)
DTIA$DTIA15 = as.factor(DTIA$DTIA15)
DTIA$DTIA21 = as.factor(DTIA$DTIA21)
DTIA$DTIA55 = as.factor(DTIA$DTIA55)
DTIA$DTIA52 = as.factor(DTIA$DTIA52)
levels(x = DTIA$DTIA24) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA$DTIA15) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA$DTIA21) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA$DTIA55) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA$DTIA52) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# ___  ___          _ _  __ _               
# |  \/  |         | (_)/ _(_)              
# | .  . | ___   __| |_| |_ _  ___ _ __ ___ 
# | |\/| |/ _ \ / _` | |  _| |/ _ \ '__/ __|
# | |  | | (_) | (_| | | | | |  __/ |  \__ \
# \_|  |_/\___/ \__,_|_|_| |_|\___|_|  |___/
# SEX
dfa = read.csv2(file = "v4/derive47.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
GENDER = subset.data.frame(dfa, select = c(GENDER, ID_C))
GENDER$GENDER = as.factor(GENDER$GENDER)
levels(x = GENDER$GENDER) = c("FEMALE","MALE")

# BMI
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
BMI01 = subset.data.frame(dfa, select = c(BMI01, ID_C))
BMI01$BMI01 = as.numeric(as.character(BMI01$BMI01))
             
#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/

# Education a bit weird have to or them
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
ELEVEL02 = subset.data.frame(dfa, select = c(ELEVEL02, ID_C))
ELEVEL02$ELEVEL02 = as.factor(ELEVEL02$ELEVEL02)

# Smoking
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
CIGT01 = subset.data.frame(dfa, select = c(CIGT01, ID_C))
CIGT01$CIGT01 = as.factor(CIGT01$CIGT01)
levels(x = CIGT01$CIGT01) = c("Current Smoker", "Former Smoker", "Never Smoker", "Unknown")

# PA
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
SPRT_I02 = subset.data.frame(dfa, select = c(SPRT_I02, ID_C))
SPRT_I02$SPRT_I02 = as.numeric(as.character(SPRT_I02$SPRT_I02))

# ALCOHOL
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
ETHANL03 = subset.data.frame(dfa, select = c(ETHANL03, ID_C))
ETHANL03$ETHANL03 = as.numeric(as.character(ETHANL03$ETHANL03))

# FAM_DIAB
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
famdiab = subset.data.frame(dfa, select = c(MOMHISTORYDIA, DADHISTORYDIA, ID_C))
famdiab$MOMHISTORYDIA = as.factor(famdiab$MOMHISTORYDIA)
levels(x = famdiab$MOMHISTORYDIA) = c("No", "Yes")
famdiab$DADHISTORYDIA = as.factor(famdiab$DADHISTORYDIA)
levels(x = famdiab$DADHISTORYDIA) = c("No", "Yes")

# MI
dfa = read.csv2(file = "v1/mhxa02.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfb = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
MHXA28 = subset.data.frame(dfa, select = c(MHXA28, ID_C))
MHXA28$MHXA28 = as.factor(MHXA28$MHXA28)
levels(x = MHXA28$MHXA28) = c("No", "Unsure", "Yes")

HOM10C = subset.data.frame(dfb, select = c(HOM10C, ID_C))
HOM10C$HOM10C = as.factor(HOM10C$HOM10C)
levels(x = HOM10C$HOM10C) = c("No", "Unsure", "Yes")

# STROKE
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
HOM10D = subset.data.frame(dfa, select = c(HOM10D, ID_C))
HOM10D$HOM10D = as.factor(HOM10D$HOM10D)
levels(x = HOM10D$HOM10D) = c("No", "Unsure", "Yes")

# CANCER
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
HOM10F = subset.data.frame(dfa, select = c(HOM10F, ID_C))
HOM10F$HOM10F = as.factor(HOM10F$HOM10F)
levels(x = HOM10F$HOM10F) = c("No", "Unsure", "Yes")

# HYPERTENSION
dfa = read.csv2(file = "v1/hom.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
HOM10A = subset.data.frame(dfa, select = c(HOM10A, ID_C))
HOM10A$HOM10A = as.factor(HOM10A$HOM10A)
levels(x = HOM10A$HOM10A) = c("No", "Unsure", "Yes")

# E_INTAKE
dfa = read.csv2(file = "v1/totnutx.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
TCAL = subset.data.frame(dfa, select = c(TCAL, ID_C))
TCAL$TCAL = as.numeric(as.character(TCAL$TCAL))

dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
TOTCAL03 = subset.data.frame(dfa, select = c(TOTCAL03, ID_C))
TOTCAL03$TOTCAL03 = as.numeric(as.character(TOTCAL03$TOTCAL03))

# FRUIT
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
fruit = subset.data.frame(dfa, select = c(DTIA11,DTIA12,DTIA13, DTIA14, ID_C))
fruit$DTIA11 = as.factor(fruit$DTIA11)
levels(x = fruit$DTIA11) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "M")
fruit$DTIA12 = as.factor(fruit$DTIA12)
levels(x = fruit$DTIA12) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "S")
fruit$DTIA13 = as.factor(fruit$DTIA13)
levels(x = fruit$DTIA13) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
fruit$DTIA14 = as.factor(fruit$DTIA14)
levels(x = fruit$DTIA14) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")



# VEG - note removed legumes
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
veg = subset.data.frame(dfa, select = c(DTIA16,DTIA17, DTIA18, DTIA19, DTIA20, 
	DTIA22, DTIA23, DTIA25, ID_C))

veg$DTIA16 = as.factor(veg$DTIA16)
levels(x = veg$DTIA16) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "R")

veg$DTIA17 = as.factor(veg$DTIA17)
levels(x = veg$DTIA17) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

veg$DTIA18 = as.factor(veg$DTIA18)
levels(x = veg$DTIA18) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

veg$DTIA19 = as.factor(veg$DTIA19)
levels(x = veg$DTIA19) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

veg$DTIA20 = as.factor(veg$DTIA20)
levels(x = veg$DTIA20) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

veg$DTIA22 = as.factor(veg$DTIA22)
levels(x = veg$DTIA22) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

veg$DTIA23 = as.factor(veg$DTIA23)
levels(x = veg$DTIA23) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

veg$DTIA25 = as.factor(veg$DTIA25)
levels(x = veg$DTIA25) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")


# DAIRY

# FIBER
dfa = read.csv2(file = "v1/anut2.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DFIB = subset.data.frame(dfa, select = c(DFIB, ID_C))
DFIB$DFIB = as.numeric(as.character(DFIB$DFIB))


# RED_MEAT
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
red = subset.data.frame(dfa, select = c(DTIA32, DTIA33, ID_C))
red$DTIA32 = as.factor(red$DTIA32)
levels(x = red$DTIA32) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
red$DTIA33 = as.factor(red$DTIA33)
levels(x = red$DTIA33) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")



# PROC-MEAT
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
proc = subset.data.frame(dfa, select = c(DTIA28,DTIA29,DTIA30, DTIA31, ID_C))
proc$DTIA28 = as.factor(proc$DTIA28)
levels(x = proc$DTIA28) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
proc$DTIA29 = as.factor(proc$DTIA29)
levels(x = proc$DTIA29) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
proc$DTIA30 = as.factor(proc$DTIA30)
levels(x = proc$DTIA30) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
proc$DTIA31 = as.factor(proc$DTIA31)
levels(x = proc$DTIA31) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")


# SUG_BEVS
dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
bevs = subset.data.frame(dfa, select = c(DTIA64, DTIA65, ID_C))
bevs$DTIA64 = as.factor(bevs$DTIA64)
levels(x = bevs$DTIA64) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
bevs$DTIA65 = as.factor(bevs$DTIA65)
levels(x = bevs$DTIA65) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# MEDS
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
HYPTMDCODE01 = subset.data.frame(dfa, select = c(HYPTMDCODE01, ID_C))
HYPTMDCODE01$HYPTMDCODE01 = as.factor(HYPTMDCODE01$HYPTMDCODE01)
levels(x = HYPTMDCODE01$HYPTMDCODE01) = c("No", "Yes")

dfa = read.csv2(file = "v1/anta.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
ANTA07A = subset.data.frame(dfa, select = c(ANTA07A, ID_C))
ANTA07A$ANTA07A = as.numeric(as.character(ANTA07A$ANTA07A))

rm(dfa, dfb, dfc, dfd, dfe, dff, dfg, dfh, dfi, dfj, dfk, dfl)


#################################################################################################################
#################################################################################################################
#################################################################################################################
dataframes_list = list(HOM10E,MSRA08F,GLUCOS01,CHMB07,HHXB05D,MSRB24F,LIPC4A,PHXA8K,MSRC24G,LIPD4A,PHXB6C,MSRD24G,
	LIP23,MSRF33C,V2AGE22,V1AGE01,V3AGE31,V4AGE41,V5AGE51,V2DAYS,V3DAYS,V4DAYS,V5DATE51_DAYS,DTIA, 
	GENDER,BMI01,ELEVEL02,CIGT01,SPRT_I02,ETHANL03,famdiab,MHXA28,HOM10C,HOM10D,HOM10F,HOM10A,TCAL,fruit,veg,DFIB,red,proc,
	bevs,HYPTMDCODE01,ANTA07A, TOTCAL03)

aric = Reduce(function(...) merge(..., by="ID_C", all=TRUE), dataframes_list)
colnames(aric)[1] <- "ID"

aric_tibble = as_tibble(aric)
# WAIST
save(aric,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/aric/main_study/aric_r_legumes.Rdata")
write_dta(data = aric_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/aric/main_study/aric_legumes.dta")

