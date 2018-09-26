# This file is used to create a dataframe for the pattern exemplar in prhhp
# Author: Paul Scherer and Stefan Dietrich

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
levels(x = prevalence$PA118) = c("NO", "Yes", "Unsure")
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
levels(x = case_b$PB146) = c("No", "Yes", "Maybe")
levels(x = case_c$PC106) = c("No", "Yes", "Maybe")
levels(x = case_c$PC21) = c("No", "Yes", "Maybe")
levels(x = case_c$PC22) = c("No", "Yes", "Maybe")
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


# TOTAL
dfa = read.csv2(file = "prexam1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA217 = subset.data.frame(dfa, select = c(PA217, NEWID))
PA218 = subset.data.frame(dfa, select = c(PA218, NEWID))
PA219 = subset.data.frame(dfa, select = c(PA219, NEWID))
PA220 = subset.data.frame(dfa, select = c(PA220, NEWID))
PA221 = subset.data.frame(dfa, select = c(PA221, NEWID))
PA222 = subset.data.frame(dfa, select = c(PA222, NEWID))
PA223 = subset.data.frame(dfa, select = c(PA223, NEWID))
PA224 = subset.data.frame(dfa, select = c(PA224, NEWID))
PA225 = subset.data.frame(dfa, select = c(PA225, NEWID))
PA226 = subset.data.frame(dfa, select = c(PA226, NEWID))
PA227 = subset.data.frame(dfa, select = c(PA227, NEWID))
PA228 = subset.data.frame(dfa, select = c(PA228, NEWID))
PA229 = subset.data.frame(dfa, select = c(PA229, NEWID))
PA230 = subset.data.frame(dfa, select = c(PA230, NEWID))
PA231 = subset.data.frame(dfa, select = c(PA231, NEWID))
PA232 = subset.data.frame(dfa, select = c(PA232, NEWID))
PA233 = subset.data.frame(dfa, select = c(PA233, NEWID))
PA234 = subset.data.frame(dfa, select = c(PA234, NEWID))
PA235 = subset.data.frame(dfa, select = c(PA235, NEWID))
PA236 = subset.data.frame(dfa, select = c(PA236, NEWID))
PA237 = subset.data.frame(dfa, select = c(PA237, NEWID))
PA238 = subset.data.frame(dfa, select = c(PA238, NEWID))
PA239 = subset.data.frame(dfa, select = c(PA239, NEWID))
PA240 = subset.data.frame(dfa, select = c(PA240, NEWID))
PA241 = subset.data.frame(dfa, select = c(PA241, NEWID))
PA242 = subset.data.frame(dfa, select = c(PA242, NEWID))
PA243 = subset.data.frame(dfa, select = c(PA243, NEWID))
PA244 = subset.data.frame(dfa, select = c(PA244, NEWID))
PA245 = subset.data.frame(dfa, select = c(PA245, NEWID))
PA246 = subset.data.frame(dfa, select = c(PA246, NEWID))
PA247 = subset.data.frame(dfa, select = c(PA247, NEWID))
PA248 = subset.data.frame(dfa, select = c(PA248, NEWID))
PA249 = subset.data.frame(dfa, select = c(PA249, NEWID))
PA250 = subset.data.frame(dfa, select = c(PA250, NEWID))
PA252 = subset.data.frame(dfa, select = c(PA252, NEWID))
PA253 = subset.data.frame(dfa, select = c(PA253, NEWID))
PA254 = subset.data.frame(dfa, select = c(PA254, NEWID))
PA255 = subset.data.frame(dfa, select = c(PA255, NEWID))

PA217$PA217 = as.numeric(as.character(PA217$PA217))
PA218$PA218 = as.numeric(as.character(PA218$PA218))
PA219$PA219 = as.numeric(as.character(PA219$PA219))
PA220$PA220 = as.numeric(as.character(PA220$PA220))
PA221$PA221 = as.numeric(as.character(PA221$PA222))
PA222$PA222 = as.numeric(as.character(PA222$PA222))
PA223$PA223 = as.numeric(as.character(PA223$PA223))
PA224$PA224 = as.numeric(as.character(PA224$PA224))
PA225$PA225 = as.numeric(as.character(PA225$PA225))
PA226$PA226 = as.numeric(as.character(PA226$PA226))
PA227$PA227 = as.numeric(as.character(PA227$PA227))
PA228$PA228 = as.numeric(as.character(PA228$PA228))
PA229$PA229 = as.numeric(as.character(PA229$PA229))
PA230$PA230 = as.numeric(as.character(PA230$PA230))
PA231$PA231 = as.numeric(as.character(PA231$PA231))
PA232$PA232 = as.numeric(as.character(PA232$PA232))
PA233$PA233 = as.numeric(as.character(PA233$PA233))
PA234$PA234 = as.numeric(as.character(PA234$PA234))
PA235$PA235 = as.numeric(as.character(PA235$PA235))
PA236$PA236 = as.numeric(as.character(PA236$PA236))
PA237$PA237 = as.numeric(as.character(PA237$PA237))
PA238$PA238 = as.numeric(as.character(PA238$PA238))
PA239$PA239 = as.numeric(as.character(PA239$PA239))
PA240$PA240 = as.numeric(as.character(PA240$PA240))
PA241$PA241 = as.numeric(as.character(PA241$PA241))
PA242$PA242 = as.numeric(as.character(PA242$PA242))
PA243$PA243 = as.numeric(as.character(PA243$PA243))
PA244$PA244 = as.numeric(as.character(PA244$PA244))
PA245$PA245 = as.numeric(as.character(PA245$PA245))
PA246$PA246 = as.numeric(as.character(PA246$PA246))
PA247$PA247 = as.numeric(as.character(PA247$PA247))
PA248$PA248 = as.numeric(as.character(PA248$PA248))
PA249$PA249 = as.numeric(as.character(PA249$PA249))
PA250$PA250 = as.numeric(as.character(PA250$PA250))
PA252$PA252 = as.numeric(as.character(PA252$PA252))
PA253$PA253 = as.numeric(as.character(PA253$PA253))
PA254$PA254 = as.numeric(as.character(PA254$PA254))
PA255$PA255 = as.numeric(as.character(PA255$PA255))


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
PA28$PA28 = as.factor(as.character(PA28$PA28))
levels(x = PA28$PA28) = c("None", "Grades 1-4", "Grades 5-8", "HighSchool Attended", "HighSchool Graduated", "University Attended", "University Graduated")

# SMOKING
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA42 = subset.data.frame(dfa, select = c(PA42, NEWID))
PA42$PA42 = as.factor(PA42$PA42)
levels(x = PA42$PA42) = c("Never", "Now Smoker", "Previous Smoker")

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
levels(x = PE34$PE34) = c("no", "parents only", "siblings", "parent and sibling")

# MI
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA60 = subset.data.frame(dfa, select = c(PA60, NEWID))
PA60$PA60 = as.factor(PA60$PA60)
levels(x = PA60$PA60) = c("none", "new", "old", "doubtful")

# STROKE
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA194 = subset.data.frame(dfa, select = c(PA194, NEWID))
PA194$PA194 = as.factor(PA194$PA194)
levels(x = PA194$PA194) = c("no", "Yes", "Questionable")

# CANCER

# HYPERTENSION
dfa = read.csv2(file = 'prevent.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PE66 = subset.data.frame(dfa, select = c(PE66, NEWID))
PE66$PE66 = as.factor(PE66$PE66)
levels(x = PE66$PE66) = c("None", "Borderline", "Definite")

# E_INTAKE
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA63 = subset.data.frame(dfa, select = c(PA63, NEWID))
PA63$PA63 = as.numeric(as.character(PA63$PA63))

# MEDS
dfa = read.csv2(file = 'prexam1.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
PA125 = subset.data.frame(dfa, select = c(PA125, NEWID))
PA125$PA125 = as.factor(PA125$PA125)
levels(x = PA125$PA125) = c("No", "Yes", "doubtful")

# WAIST
# SUPPLEMENTS

rm(dfa, dfb, dfc)

#################################################################################################################
#################################################################################################################
#################################################################################################################
dataframes_list = list(PA17,PE3,prevalence,case_b,case_c,age_end_b,age_end_c,
	BMI1,PA28,PA42,PA52,PA78,PE34,PA60,PA194,PE66,PA63, PA125,
	PA217, PA218, PA219, PA220, PA221, PA222, PA223, PA224, PA225, PA226, PA227, PA228, PA229, PA230, PA231, PA232,
	PA233, PA234, PA235, PA236, PA237, PA238, PA239, PA240, PA241, PA242, PA243, PA244, PA245, PA246, PA247, PA248,
	PA249, PA250, PA252, PA253, PA254, PA255)

prhhp = Reduce(function(...) merge(..., by="NEWID", all=TRUE), dataframes_list)
colnames(prhhp)[1] <- "ID"
prhhp$ID = as.character(prhhp$ID)

prhhp_tibble = as_tibble(prhhp)
# WAIST
save(prhhp,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/prhhp/prhhp_pattern.Rdata")
write_dta(data = prhhp_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/prhhp/prhhp_pattern.dta")

