# This file is used to create a dataframe for the fish exemplar in prhhp
# Author: Paul Scherer

library(tibble) # for easier dataframes, and rather the saving to dta
library(haven)
setwd("V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/prhhp/Data")

#  _____       _                                 
# |  _  |     | |                                
# | | | |_   _| |_ ___ ___  _ __ ___   ___  ___  
# | | | | | | | __/ __/ _ \| '_ ` _ \ / _ \/ __| 
# \ \_/ / |_| | || (_| (_) | | | | | |  __/\__ \ 
#  \___/ \__,_|\__\___\___/|_| |_| |_|\___||___/ 

# AGE_BASE

dfa = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA17 = subset.data.frame(dfa, select = c(PA17, NEWID))
PA17$PA17 = as.numeric(as.character(PA17$PA17))

dfa = read.csv2(file = "prevent.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PE3 = subset.data.frame(dfa, select = c(PE3, NEWID))
PE3$PE3 = as.numeric(as.character(PE3$PE3))

# TYPE_DIAB
# PREV_DIAB
dfa = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
prevalence = subset.data.frame(dfa, select = c(PA118, PA119, PA199, PA214, PA211, NEWID))

prevalence$PA118 = as.factor(prevalence$PA118)
prevalence$PA119 = as.factor(prevalence$PA119)
prevalence$PA214 = as.factor(prevalence$PA214)
prevalence$PA199 = as.factor(prevalence$PA199)
levels(x = prevalence$PA118) = c("No", "Yes", "Unsure")
levels(x = prevalence$PA119) = c("No", "Diet Treatment", "Oral Medication", "Insulin Treatment")
levels(x = prevalence$PA214) = c("No", "Yes", "Unsure")
levels(x = prevalence$PA199) = c("No", "Yes", "Unsure")
prevalence$PA211 = as.numeric(as.character(prevalence$PA211))

# CASE_OBJ # CASE_OBJ_SELF
dfa = read.csv2(file = "prexam2.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfb = read.csv2(file = "prexam3.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
pb = subset.data.frame(dfa, select = c(PB146, PB160, NEWID))
pc = subset.data.frame(dfb, select = c(PC106, PC21, PC22, PC120, PC119, NEWID))

pb$PB146 = as.factor(pb$PB146)
pc$PC106 = as.factor(pc$PC106)
pc$PC21 = as.factor(pc$PC21)
pc$PC22 = as.factor(pc$PC22)
pc$PC120 = as.factor(pc$PC120)
pb$PB160 = as.numeric(as.character(pb$PB160))
pc$PC119 = as.numeric(as.character(pc$PC119))
levels(x = pb$PB146) = c("No", "Yes", "Unsure")
levels(x = pc$PC106) = c("No", "Yes", "Unsure")
levels(x = pc$PC21) = c("No", "Yes", "Unsure")
levels(x = pc$PC22) = c("No", "Yes", "Unsure")
levels(x = pc$PC120) = c("No", "Yes")

# AGE_END_OBJ
# AGE_END_OBJ_SELF
dfa = read.csv2(file = "prexam2.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfb = read.csv2(file = "prexam3.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfc = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
pb = subset.data.frame(dfc, select = c(PA17, NEWID))
pb = subset.data.frame(dfa, select = c(PB216, PB13, NEWID))
pc = subset.data.frame(dfb, select = c(PC121, PC5, NEWID))

# pa$PA17 = as.numeric(as.character(pa$PA17))
pb$PB216 = as.numeric(as.character(pb$PB216))
pb$PB13 = as.numeric(as.character(pb$PB13))
pc$PC121 = as.numeric(as.character(pc$PC121))
pc$PC5 = as.numeric(as.character(pc$PC5))

# _____                                         
# |  ___|                                        
# | |____  ___ __   ___  ___ _   _ _ __ ___  ___ 
# |  __\ \/ / '_ \ / _ \/ __| | | | '__/ _ \/ __|
# | |___>  <| |_) | (_) \__ \ |_| | | |  __/\__ \
# \____/_/\_\ .__/ \___/|___/\__,_|_|  \___||___/
#           | |                                  
#           |_|                                  

# FATTY
# FRESH
# FRIED
# LEAN
# NONFISH
dfa = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA229 = subset.data.frame(dfa, select = c(PA229, NEWID))
PA229$PA229 = as.numeric(as.character(PA229$PA229))
# SALT
# SSD
dfa = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA228 = subset.data.frame(dfa, select = c(PA228, NEWID))
PA228$PA228 = as.numeric(as.character(PA228$PA228))
# TOTAL
dfa = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA227 = subset.data.frame(dfa, select = c(PA227, NEWID))
PA227$PA227 = as.numeric(as.character(PA227$PA227))

# ___  ___          _ _  __ _               
# |  \/  |         | (_)/ _(_)              
# | .  . | ___   __| |_| |_ _  ___ _ __ ___ 
# | |\/| |/ _ \ / _` | |  _| |/ _ \ '__/ __|
# | |  | | (_) | (_| | | | | |  __/ |  \__ \
# \_|  |_/\___/ \__,_|_|_| |_|\___|_|  |___/

# SEX all dudes
# BMI
dfa = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
BMI1 = subset.data.frame(dfa, select = c(BMI1, NEWID))
BMI1$BMI1 = as.numeric(as.character(BMI1$BMI1))
# GEOG_AREA

         
#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/

# EDUCATION
# SMOKING
# PA
# ALCOHOL
# FAM_DIAB
# MI
# STROKE
# CANCER
# HYPERTENSION
# E_INTAKE
# COV_FRUIT
# COV_VEG
# COV_DAIRY
# COV_FIBER
# COV_RED_MEAT
# COV_PROC_MEAT
# COV_SUG_BEVS
# MEDS
# WAIST
# SUPPLEMENTS


#################################################################################################################
#################################################################################################################
#################################################################################################################
dataframes_list = list(HOM10E,MSRA08F,GLUCOS01,CHMB07,HHXB05D,MSRB24F,LIPC4A,PHXA8K,MSRC24G,LIPD4A,PHXB6C,MSRD24G,
	LIP23,MSRF33C,V2AGE22,V1AGE01,V3AGE31,V4AGE41,V5AGE51,V2DAYS,V3DAYS,V4DAYS,V5DATE51_DAYS,DTIA35,DTIA36,DTIA37,
	DTIA34,GENDER,BMI01,ELEVEL02,CIGT01,SPRT_I02,ETHANL03,famdiab,MHXA28,HOM10C,HOM10D,HOM10F,HOM10A,TCAL,fruit,veg,DFIB,red,proc,
	bevs,HYPTMDCODE01,ANTA07A, TOTCAL03)

prhhp = Reduce(function(...) merge(..., by="ID_C", all=TRUE), dataframes_list)
colnames(prhhp)[1] <- "ID"

prhhp_tibble = as_tibble(prhhp)
# WAIST
save(prhhp,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/prhhp/prhhp.Rdata")
write_dta(data = prhhp_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/prhhp/prhhp.dta")

