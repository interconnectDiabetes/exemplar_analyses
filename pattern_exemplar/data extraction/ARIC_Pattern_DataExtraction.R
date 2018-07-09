# This file is used to create a dataframe for the pattern exemplar
# Author: Paul Scherer and Stefan Dietrich

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

dfa = read.csv2(file = "v1/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA01 = subset.data.frame(dfa, select = c(DTIA01, ID_C))
DTIA02 = subset.data.frame(dfa, select = c(DTIA02, ID_C))
DTIA03 = subset.data.frame(dfa, select = c(DTIA03, ID_C))
DTIA04 = subset.data.frame(dfa, select = c(DTIA04, ID_C))
DTIA05 = subset.data.frame(dfa, select = c(DTIA05, ID_C))
DTIA06 = subset.data.frame(dfa, select = c(DTIA06, ID_C))
DTIA07 = subset.data.frame(dfa, select = c(DTIA07, ID_C))
DTIA08 = subset.data.frame(dfa, select = c(DTIA08, ID_C))
DTIA09 = subset.data.frame(dfa, select = c(DTIA09, ID_C))
DTIA10 = subset.data.frame(dfa, select = c(DTIA10, ID_C))
DTIA11 = subset.data.frame(dfa, select = c(DTIA11, ID_C))
DTIA12 = subset.data.frame(dfa, select = c(DTIA12, ID_C))
DTIA13 = subset.data.frame(dfa, select = c(DTIA13, ID_C))
DTIA14 = subset.data.frame(dfa, select = c(DTIA14, ID_C))
DTIA15 = subset.data.frame(dfa, select = c(DTIA15, ID_C))
DTIA16 = subset.data.frame(dfa, select = c(DTIA16, ID_C))
DTIA17 = subset.data.frame(dfa, select = c(DTIA17, ID_C))
DTIA18 = subset.data.frame(dfa, select = c(DTIA18, ID_C))
DTIA19 = subset.data.frame(dfa, select = c(DTIA19, ID_C))
DTIA20 = subset.data.frame(dfa, select = c(DTIA20, ID_C))
DTIA21 = subset.data.frame(dfa, select = c(DTIA21, ID_C))
DTIA22 = subset.data.frame(dfa, select = c(DTIA22, ID_C))
DTIA23 = subset.data.frame(dfa, select = c(DTIA23, ID_C))
DTIA24 = subset.data.frame(dfa, select = c(DTIA24, ID_C))
DTIA25 = subset.data.frame(dfa, select = c(DTIA25, ID_C))
DTIA26 = subset.data.frame(dfa, select = c(DTIA26, ID_C))
DTIA27 = subset.data.frame(dfa, select = c(DTIA27, ID_C))
DTIA28 = subset.data.frame(dfa, select = c(DTIA28, ID_C))
DTIA29 = subset.data.frame(dfa, select = c(DTIA29, ID_C))
DTIA30 = subset.data.frame(dfa, select = c(DTIA30, ID_C))
DTIA31 = subset.data.frame(dfa, select = c(DTIA31, ID_C))
DTIA32 = subset.data.frame(dfa, select = c(DTIA32, ID_C))
DTIA33 = subset.data.frame(dfa, select = c(DTIA33, ID_C))
DTIA34 = subset.data.frame(dfa, select = c(DTIA34, ID_C))
DTIA35 = subset.data.frame(dfa, select = c(DTIA35, ID_C))
DTIA36 = subset.data.frame(dfa, select = c(DTIA36, ID_C))
DTIA37 = subset.data.frame(dfa, select = c(DTIA37, ID_C))
DTIA38 = subset.data.frame(dfa, select = c(DTIA38, ID_C))
DTIA39 = subset.data.frame(dfa, select = c(DTIA39, ID_C))
DTIA40 = subset.data.frame(dfa, select = c(DTIA40, ID_C))
DTIA41 = subset.data.frame(dfa, select = c(DTIA41, ID_C))
DTIA42 = subset.data.frame(dfa, select = c(DTIA42, ID_C))
DTIA43 = subset.data.frame(dfa, select = c(DTIA43, ID_C))
DTIA44 = subset.data.frame(dfa, select = c(DTIA44, ID_C))
DTIA45 = subset.data.frame(dfa, select = c(DTIA45, ID_C))
DTIA46 = subset.data.frame(dfa, select = c(DTIA46, ID_C))
DTIA47 = subset.data.frame(dfa, select = c(DTIA47, ID_C))
DTIA48 = subset.data.frame(dfa, select = c(DTIA48, ID_C))
DTIA49 = subset.data.frame(dfa, select = c(DTIA49, ID_C))
DTIA50 = subset.data.frame(dfa, select = c(DTIA50, ID_C))
DTIA51 = subset.data.frame(dfa, select = c(DTIA51, ID_C))
DTIA52 = subset.data.frame(dfa, select = c(DTIA52, ID_C))
DTIA53 = subset.data.frame(dfa, select = c(DTIA53, ID_C))
DTIA54 = subset.data.frame(dfa, select = c(DTIA54, ID_C))
DTIA55 = subset.data.frame(dfa, select = c(DTIA55, ID_C))
DTIA56 = subset.data.frame(dfa, select = c(DTIA56, ID_C))
DTIA57 = subset.data.frame(dfa, select = c(DTIA57, ID_C))
DTIA58 = subset.data.frame(dfa, select = c(DTIA58, ID_C))
DTIA59 = subset.data.frame(dfa, select = c(DTIA59, ID_C))
DTIA60 = subset.data.frame(dfa, select = c(DTIA60, ID_C))
DTIA61 = subset.data.frame(dfa, select = c(DTIA61, ID_C))
DTIA62 = subset.data.frame(dfa, select = c(DTIA62, ID_C))
DTIA63 = subset.data.frame(dfa, select = c(DTIA63, ID_C))
DTIA64 = subset.data.frame(dfa, select = c(DTIA64, ID_C))
DTIA65 = subset.data.frame(dfa, select = c(DTIA65, ID_C))
DTIA66 = subset.data.frame(dfa, select = c(DTIA66, ID_C))
DTIA84 = subset.data.frame(dfa, select = c(DTIA84, ID_C))
DTIA88 = subset.data.frame(dfa, select = c(DTIA88, ID_C))
DTIA96 = subset.data.frame(dfa, select = c(DTIA96, ID_C))
DTIA97 = subset.data.frame(dfa, select = c(DTIA97, ID_C))
DTIA98 = subset.data.frame(dfa, select = c(DTIA98, ID_C))
DTIA01$DTIA01 = as.factor(DTIA01$DTIA01)
DTIA02$DTIA02 = as.factor(DTIA02$DTIA02)
DTIA03$DTIA03 = as.factor(DTIA03$DTIA03)
DTIA04$DTIA04 = as.factor(DTIA04$DTIA04)
DTIA05$DTIA05 = as.factor(DTIA05$DTIA05)
DTIA06$DTIA06 = as.factor(DTIA06$DTIA06)
DTIA07$DTIA07 = as.factor(DTIA07$DTIA07)
DTIA08$DTIA08 = as.factor(DTIA08$DTIA08)
DTIA09$DTIA09 = as.factor(DTIA09$DTIA09)
DTIA10$DTIA10 = as.factor(DTIA10$DTIA10)
DTIA11$DTIA11 = as.factor(DTIA11$DTIA11)
DTIA12$DTIA12 = as.factor(DTIA12$DTIA12)
DTIA13$DTIA13 = as.factor(DTIA13$DTIA13)
DTIA14$DTIA14 = as.factor(DTIA14$DTIA14)
DTIA15$DTIA15 = as.factor(DTIA15$DTIA15)
DTIA16$DTIA16 = as.factor(DTIA16$DTIA16)
DTIA17$DTIA17 = as.factor(DTIA17$DTIA17)
DTIA18$DTIA18 = as.factor(DTIA18$DTIA18)
DTIA19$DTIA19 = as.factor(DTIA19$DTIA19)
DTIA20$DTIA20 = as.factor(DTIA20$DTIA20)
DTIA21$DTIA21 = as.factor(DTIA21$DTIA21)
DTIA22$DTIA22 = as.factor(DTIA22$DTIA22)
DTIA23$DTIA23 = as.factor(DTIA23$DTIA23)
DTIA24$DTIA24 = as.factor(DTIA24$DTIA24)
DTIA25$DTIA25 = as.factor(DTIA25$DTIA25)
DTIA26$DTIA26 = as.factor(DTIA26$DTIA26)
DTIA27$DTIA27 = as.factor(DTIA27$DTIA27)
DTIA28$DTIA28 = as.factor(DTIA28$DTIA28)
DTIA29$DTIA29 = as.factor(DTIA29$DTIA29)
DTIA30$DTIA30 = as.factor(DTIA30$DTIA30)
DTIA31$DTIA31 = as.factor(DTIA31$DTIA31)
DTIA32$DTIA32 = as.factor(DTIA32$DTIA32)
DTIA33$DTIA33 = as.factor(DTIA33$DTIA33)
DTIA34$DTIA34 = as.factor(DTIA34$DTIA34)
DTIA35$DTIA35 = as.factor(DTIA35$DTIA35)
DTIA36$DTIA36 = as.factor(DTIA36$DTIA36)
DTIA37$DTIA37 = as.factor(DTIA37$DTIA37)
DTIA38$DTIA38 = as.factor(DTIA38$DTIA38)
DTIA39$DTIA39 = as.factor(DTIA39$DTIA39)
DTIA40$DTIA40 = as.factor(DTIA40$DTIA40)
DTIA41$DTIA41 = as.factor(DTIA41$DTIA41)
DTIA42$DTIA42 = as.factor(DTIA42$DTIA42)
DTIA43$DTIA43 = as.factor(DTIA43$DTIA43)
DTIA44$DTIA44 = as.factor(DTIA44$DTIA44)
DTIA45$DTIA45 = as.factor(DTIA45$DTIA45)
DTIA46$DTIA46 = as.factor(DTIA46$DTIA46)
DTIA47$DTIA47 = as.factor(DTIA47$DTIA47)
DTIA48$DTIA48 = as.factor(DTIA48$DTIA48)
DTIA49$DTIA49 = as.factor(DTIA49$DTIA49)
DTIA50$DTIA50 = as.factor(DTIA50$DTIA50)
DTIA51$DTIA51 = as.factor(DTIA51$DTIA51)
DTIA52$DTIA52 = as.factor(DTIA52$DTIA52)
DTIA53$DTIA53 = as.factor(DTIA53$DTIA53)
DTIA54$DTIA54 = as.factor(DTIA54$DTIA54)
DTIA55$DTIA55 = as.factor(DTIA55$DTIA55)
DTIA56$DTIA56 = as.factor(DTIA56$DTIA56)
DTIA57$DTIA57 = as.factor(DTIA57$DTIA57)
DTIA58$DTIA58 = as.factor(DTIA58$DTIA58)
DTIA59$DTIA59 = as.factor(DTIA59$DTIA59)
DTIA60$DTIA60 = as.factor(DTIA60$DTIA60)
DTIA61$DTIA61 = as.factor(DTIA61$DTIA61)
DTIA62$DTIA62 = as.factor(DTIA62$DTIA62)
DTIA63$DTIA63 = as.factor(DTIA63$DTIA63)
DTIA64$DTIA64 = as.factor(DTIA64$DTIA64)
DTIA65$DTIA65 = as.factor(DTIA65$DTIA65)
DTIA66$DTIA66 = as.factor(DTIA66$DTIA66)
#DTIA84$DTIA84 = as.factor(DTIA84$DTIA84)
DTIA88$DTIA88 = as.factor(DTIA88$DTIA88)
#DTIA96$DTIA96 = as.factor(DTIA96$DTIA96)
#DTIA97$DTIA97 = as.factor(DTIA97$DTIA97)
#DTIA98$DTIA98 = as.factor(DTIA98$DTIA98)
levels(x = DTIA01$DTIA01) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA02$DTIA02) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA03$DTIA03) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA04$DTIA04) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA05$DTIA05) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "P")
levels(x = DTIA06$DTIA06) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "R")
levels(x = DTIA07$DTIA07) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA08$DTIA08) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA09$DTIA09) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "T")
levels(x = DTIA10$DTIA10) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "Y")
levels(x = DTIA11$DTIA11) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "M")
levels(x = DTIA12$DTIA12) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "S")
levels(x = DTIA13$DTIA13) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA14$DTIA14) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA15$DTIA15) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA16$DTIA16) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "R")
levels(x = DTIA17$DTIA17) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA18$DTIA18) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA19$DTIA19) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA20$DTIA20) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA21$DTIA21) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA22$DTIA22) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA23$DTIA23) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA24$DTIA24) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA25$DTIA25) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA26$DTIA26) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA27$DTIA27) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA28$DTIA28) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA29$DTIA29) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA30$DTIA30) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA31$DTIA31) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA32$DTIA32) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA33$DTIA33) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA34$DTIA34) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA35$DTIA35) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA36$DTIA36) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA37$DTIA37) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA38$DTIA38) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA39$DTIA39) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA40$DTIA40) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA41$DTIA41) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA42$DTIA42) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA43$DTIA43) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA44$DTIA44) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA45$DTIA45) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA46$DTIA46) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA47$DTIA47) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA48$DTIA48) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA49$DTIA49) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA50$DTIA50) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA51$DTIA51) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA52$DTIA52) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA53$DTIA53) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA54$DTIA54) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA55$DTIA55) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA56$DTIA56) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA57$DTIA57) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA58$DTIA58) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA59$DTIA59) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA60$DTIA60) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA61$DTIA61) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA62$DTIA62) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA63$DTIA63) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA64$DTIA64) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA65$DTIA65) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA66$DTIA66) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
#levels(x = DTIA84$DTIA84) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
levels(x = DTIA88$DTIA88) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
#levels(x = DTIA96$DTIA96) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
#levels(x = DTIA97$DTIA97) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
#levels(x = DTIA98$DTIA98) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")

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

#ETHNICITY
#dfa = read.csv2(file = "cohort_Stroke/cstrps11.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
#STRX10 = subset.data.frame(dfa, select = c(STRX10, ID_C))
#levels(x = STRX10$STRX10) = c("?","?")
            
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

# MEDS
dfa = read.csv2(file = "v1/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
HYPTMDCODE01 = subset.data.frame(dfa, select = c(HYPTMDCODE01, ID_C))
HYPTMDCODE01$HYPTMDCODE01 = as.factor(HYPTMDCODE01$HYPTMDCODE01)
levels(x = HYPTMDCODE01$HYPTMDCODE01) = c("No", "Yes")

# WAIST
dfa = read.csv2(file = "v1/anta.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
ANTA07A = subset.data.frame(dfa, select = c(ANTA07A, ID_C))
ANTA07A$ANTA07A = as.numeric(as.character(ANTA07A$ANTA07A))

rm(dfa, dfb, dfc, dfd, dfe, dff, dfg, dfh, dfi, dfj, dfk, dfl)

#################################################################################################################
#################################################################################################################
#################################################################################################################
dataframes_list = list(HOM10E,MSRA08F,GLUCOS01,CHMB07,HHXB05D,MSRB24F,LIPC4A,PHXA8K,MSRC24G,LIPD4A,PHXB6C,MSRD24G,
	LIP23,MSRF33C,V2AGE22,V1AGE01,V3AGE31,V4AGE41,V5AGE51,V2DAYS,V3DAYS,V4DAYS,V5DATE51_DAYS, DTIA01, DTIA02, DTIA03, 
                     DTIA04, DTIA05, DTIA06, DTIA07, DTIA08, DTIA09, DTIA10, DTIA11, DTIA12, DTIA13, DTIA14, DTIA15, DTIA16, DTIA17, DTIA18, DTIA19, 
                     DTIA20, DTIA21, DTIA22, DTIA23, DTIA24, DTIA25, DTIA26, DTIA27, DTIA28, DTIA29, DTIA30, DTIA31, DTIA32, DTIA33, DTIA34, DTIA35,
                     DTIA36, DTIA37, DTIA38, DTIA39, DTIA40, DTIA41, DTIA42, DTIA43, DTIA44, DTIA45, DTIA46, DTIA47, DTIA48, DTIA49, DTIA50, DTIA51, 
                     DTIA52, DTIA53, DTIA54, DTIA55, DTIA56, DTIA57, DTIA58, DTIA59, DTIA60, DTIA61, DTIA62, DTIA63, DTIA64, DTIA65, DTIA66, DTIA84,
                     DTIA88, DTIA96, DTIA97, DTIA98, GENDER, BMI01, ELEVEL02, CIGT01, SPRT_I02, ETHANL03, famdiab, MHXA28, HOM10C, HOM10D,
                     HOM10F, HOM10A, TCAL, TOTCAL03, HYPTMDCODE01, ANTA07A )

aric = Reduce(function(...) merge(..., by="ID_C", all=TRUE), dataframes_list)
colnames(aric)[1] <- "ID"

aric_tibble = as_tibble(aric)
save(aric,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/aric/main_study/aric_r_pattern.Rdata")
write_dta(data = aric_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/aric/main_study/aric_pattern.dta")

