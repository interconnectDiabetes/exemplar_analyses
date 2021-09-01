# Data extraction_additional variables 
# Chunxiao Li
# 2021.08

## ARIC





setwd("V:/Studies/InterConnect/Internal/Meat exemplar/BioLINCC_Data_Renewal/ARIC_2021a/Main_Study/") # TO CHANGE 

# OUTCOME - TO ADD 3 variables in visit 6
dfa = read.csv2(file = "V6/CSV/derive61.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DIABTS64 = subset.data.frame(dfa, select = c(DIABTS64, ID_C))
DIABTS64$DIABTS64 = as.factor(DIABTS64$DIABTS64)
levels(x = DIABTS64$DIABTS64) = c("0", "1", "T")

dfa = read.csv2(file = "V6/CSV/derive61.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DIABTS66 = subset.data.frame(dfa, select = c(DIABTS66, ID_C))
DIABTS66$DIABTS66 = as.factor(DIABTS66$DIABTS66)
levels(x = DIABTS66$DIABTS66) = c("0", "1", "T")

dfa = read.csv2(file = "V6/CSV/derive61.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DIABTS67 = subset.data.frame(dfa, select = c(DIABTS67, ID_C))
DIABTS67$DIABTS67 = as.factor(DIABTS67$DIABTS67)
levels(x = DIABTS67$DIABTS67) = c("0", "1", "T")


# DATE OF VISIT6
dfa = read.csv2(file = "V6/CSV/derive61.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
V6DATE61_DAYS = subset.data.frame(dfa, select = c(V6DATE61_DAYS, ID_C))
V6DATE61_DAYS$V6DATE61_DAYS = as.numeric(as.character(V6DATE61_DAYS$V6DATE61_DAYS))

# ETHICITY - modifier and confounder
dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
RACEGRP = subset.data.frame(dfa, select = c(RACEGRP, ID_C))
RACEGRP$RACEGRP = as.numeric(as.character(RACEGRP$RACEGRP))

# FRUIT - to REPLACE 

dfa = read.csv2(file = "v1/CSV/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
# fruit = subset.data.frame(dfa, select = c(DTIA11,DTIA12,DTIA13, DTIA14, ID_C))
fruit = subset.data.frame(dfa, select = c(DTIA09, DTIA10,DTIA12,DTIA13, DTIA14, ID_C))

fruit$DTIA09 = as.factor(fruit$DTIA09)
levels(x = fruit$DTIA09) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "T")
fruit$DTIA10 = as.factor(fruit$DTIA10)
levels(x = fruit$DTIA10) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "Y")
fruit$DTIA12 = as.factor(fruit$DTIA12)
levels(x = fruit$DTIA12) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "S")
fruit$DTIA13 = as.factor(fruit$DTIA13)
levels(x = fruit$DTIA13) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
fruit$DTIA14 = as.factor(fruit$DTIA14)
levels(x = fruit$DTIA14) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")


# POTATOES


dfa = read.csv2(file = "v1/CSV/dtia.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
potato = subset.data.frame(dfa, select = c(DTIA53, DTIA54, DTIA56, DTIA23, ID_C))

potato$DTIA53 = as.factor(potato$DTIA53)
levels(x = potato$DTIA53) = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
potato$DTIA54 = as.factor(potato$DTIA54)
levels(x = potato$DTIA54) = c("B", "C", "D", "E", "F", "G", "H", "I")
potato$DTIA56 = as.factor(potato$DTIA56)
levels(x = potato$DTIA56) = c("B", "C", "D", "E", "F", "G", "H", "I")
potato$DTIA23 = as.factor(potato$DTIA23)
levels(x = potato$DTIA23) = c("B", "C", "D", "E", "F", "G", "H", "I")

## vegetables

# replace the DIA55 DTIA52 to NUTS in harmonisation step

# DAIRY

# remove DTIA 07 DTIA 08 to FAT in harmonisation step

# other covars

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA38 = subset.data.frame(dfa, select = c(DTIA38, ID_C))
DTIA38$DTIA38 = as.factor(DTIA38$DTIA38)
levels(x = DTIA38$DTIA38) = c("A","B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA48 = subset.data.frame(dfa, select = c(DTIA48, ID_C))
DTIA48$DTIA48 = as.factor(DTIA48$DTIA48)
levels(x = DTIA48$DTIA48) = c("A","B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA49 = subset.data.frame(dfa, select = c(DTIA49, ID_C))
DTIA49$DTIA49 = as.factor(DTIA49$DTIA49)
levels(x = DTIA49$DTIA49) = c("A","B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA50 = subset.data.frame(dfa, select = c(DTIA50, ID_C))
DTIA50$DTIA50 = as.factor(DTIA50$DTIA50)
levels(x = DTIA50$DTIA50) = c("A","B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA51 = subset.data.frame(dfa, select = c(DTIA51, ID_C))
DTIA51$DTIA51 = as.factor(DTIA51$DTIA51)
levels(x = DTIA51$DTIA51) = c("A","B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA57 = subset.data.frame(dfa, select = c(DTIA57, ID_C))
DTIA57$DTIA57 = as.factor(DTIA57$DTIA57)
levels(x = DTIA57$DTIA57) = c("A","B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA58 = subset.data.frame(dfa, select = c(DTIA58, ID_C))
DTIA58$DTIA58 = as.factor(DTIA58$DTIA58)
levels(x = DTIA58$DTIA58) = c("B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA61 = subset.data.frame(dfa, select = c(DTIA61, ID_C))
DTIA61$DTIA61 = as.factor(DTIA61$DTIA61)
levels(x = DTIA61$DTIA61) = c("A","B", "C", "D", "E", "F", "G", "H", "I")

dfa = read.csv2(file = "v1/CSV/derive13.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
DTIA62 = subset.data.frame(dfa, select = c(DTIA62, ID_C))
DTIA62$DTIA62 = as.factor(DTIA62$DTIA62)
levels(x = DTIA62$DTIA62) = c("A","B", "C", "D", "E", "F", "G", "H", "I")



