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
case_b = subset.data.frame(dfa, select = c(PB146, PB160, NEWID))
case_c = subset.data.frame(dfb, select = c(PC106, PC21, PC22, PC120, PC119, NEWID))

case_b$PB146 = as.factor(case_b$PB146)
case_c$PC106 = as.factor(case_c$PC106)
case_c$PC21 = as.factor(case_c$PC21)
case_c$PC22 = as.factor(case_c$PC22)
case_c$PC120 = as.factor(case_c$PC120)
case_b$PB160 = as.numeric(as.character(case_b$PB160))
case_c$PC119 = as.numeric(as.character(case_c$PC119))
levels(x = case_b$PB146) = c("No", "Yes", "Unsure")
levels(x = case_c$PC106) = c("No", "Yes", "Unsure")
levels(x = case_c$PC21) = c("No", "Yes", "Unsure")
levels(x = case_c$PC22) = c("No", "Yes", "Unsure")
levels(x = case_c$PC120) = c("No", "Yes")

# AGE_END_OBJ
# AGE_END_OBJ_SELF
dfa = read.csv2(file = "prexam2.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
dfb = read.csv2(file = "prexam3.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
age_end_b = subset.data.frame(dfa, select = c(PB216, PB13, NEWID))
age_end_c = subset.data.frame(dfb, select = c(PC121, PC5, NEWID))

age_end_b$PB216 = as.numeric(as.character(age_end_b$PB216))
age_end_b$PB13 = as.numeric(as.character(age_end_b$PB13))
age_end_c$PC121 = as.numeric(as.character(age_end_c$PC121))
age_end_c$PC5 = as.numeric(as.character(age_end_c$PC5))

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
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA28 = subset.data.frame(dfa, select = c(PA28, NEWID))
PA28$PA28 = as.factor(PA28$PA28)
levels(x = PA28$PA28) = c("0", "1", "2", "3", "4", "5", "6", "9999")

# SMOKING
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA42 = subset.data.frame(dfa, select = c(PA42, NEWID))
PA42$PA42 = as.factor(PA42$PA42)
levels(x = PA42$PA42) = c("0", "1", "2", "9999")

# PA
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA52 = subset.data.frame(dfa, select = c(PA52, NEWID))
PA52$PA52 = as.numeric(as.character(PA52$PA52))

# ALCOHOL
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA78 = subset.data.frame(dfa, select = c(PA78, NEWID))
PA78$PA78 = as.numeric(as.character(PA78$PA78))

# FAM_DIAB
dfa = read.csv2(file = 'prevent.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PE34 = subset.data.frame(dfa, select = c(PE34, NEWID))
PE34$PE34 = as.factor(PE34$PE34)
levels(x = PE34$PE34) = c("0", "1", "2", "3", "9999")

# MI
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA60 = subset.data.frame(dfa, select = c(PA60, NEWID))
PA60$PA60 = as.factor(PA60$PA60)
levels(x = PA60$PA60) = c("0", "1", "2", "3", "9999")

# STROKE
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA194 = subset.data.frame(dfa, select = c(PA194, NEWID))
PA194$PA194 = as.factor(PA194$PA194)
levels(x = PA194$PA194) = c("0", "1", "2", "9999")

# CANCER

# HYPERTENSION
dfa = read.csv2(file = 'prevent.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PE66 = subset.data.frame(dfa, select = c(PE66, NEWID))
PE66$PE66 = as.factor(PE66$PE66)
levels(x = PE66$PE66) = c("0", "1", "2", "9999")


# E_INTAKE
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA63 = subset.data.frame(dfa, select = c(PA63, NEWID))
PA63$PA63 = as.numeric(as.character(PA63$PA63))

# COV_FRUIT
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA237 = subset.data.frame(dfa, select = c(PA237, NEWID))
PA237$PA237 = as.numeric(as.character(PA237$PA237))

dfb = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA238 = subset.data.frame(dfb, select = c(PA238, NEWID))
PA238$PA238 = as.numeric(as.character(PA238$PA238))

dfc = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA239 = subset.data.frame(dfc, select = c(PA239, NEWID))
PA239$PA239 = as.numeric(as.character(PA239$PA239))


# COV_VEG
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA233 = subset.data.frame(dfa, select = c(PA233, NEWID))
PA233$PA233 = as.numeric(as.character(PA233$PA233))

dfb = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA235 = subset.data.frame(dfb, select = c(PA235, NEWID))
PA235$PA235 = as.numeric(as.character(PA235$PA235))

dfc = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA236 = subset.data.frame(dfc, select = c(PA236, NEWID))
PA236$PA236 = as.numeric(as.character(PA236$PA236))

# DAIRY

# FIBER
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA77 = subset.data.frame(dfa, select = c(PA77, NEWID))
PA77$PA77 = as.numeric(as.character(PA77$PA77))

# RED_MEAT
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA221 = subset.data.frame(dfa, select = c(PA221, NEWID))
PA221$PA221 = as.numeric(as.character(PA221$PA221))

dfb = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA223 = subset.data.frame(dfb, select = c(PA223, NEWID))
PA223$PA223 = as.numeric(as.character(PA223$PA223))

# PROC_MEAT
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA224 = subset.data.frame(dfa, select = c(PA224, NEWID))
PA224$PA224 = as.numeric(as.character(PA224$PA224))

dfb = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA225 = subset.data.frame(dfb, select = c(PA225, NEWID))
PA225$PA225 = as.numeric(as.character(PA225$PA225))

# SUG_BEVS
dfb = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA250 = subset.data.frame(dfb, select = c(PA250, NEWID))
PA250$PA250 = as.numeric(as.character(PA250$PA250))

# MEDS
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA125 = subset.data.frame(dfa, select = c(PA125, NEWID))
PA125$PA125 = as.factor(PA125$PA125)
levels(x = PA125$PA125) = c("0", "1", "2", "9999")

# WAIST
# SUPPLEMENTS

rm(dfa, dfb, dfc)

#################################################################################################################
#################################################################################################################
#################################################################################################################
dataframes_list = list(PA17,PE3,prevalence,case_b,case_c,age_end_b,age_end_c,
	PA229,PA228,PA227,BMI1,PA28,PA42,PA52,PA78,PE34,PA60,PA194,PE66,PA63,PA237,
	PA238,PA239,PA233,PA235,PA236,PA77,PA221,PA223,PA224,PA225,PA250,PA125)

prhhp = Reduce(function(...) merge(..., by="NEWID", all=TRUE), dataframes_list)
colnames(prhhp)[1] <- "ID"
prhhp$ID = as.character(prhhp$ID)

prhhp_tibble = as_tibble(prhhp)
# WAIST
save(prhhp,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/prhhp/prhhp.Rdata")
write_dta(data = prhhp_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/prhhp/prhhp_new.dta")

