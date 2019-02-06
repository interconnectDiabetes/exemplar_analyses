# This file is used to create a dataframe for the fish exemplar
# Author: Tom Bishop

library(tibble) # for easier dataframes, and rather the saving to dta
library(haven)
setwd("V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/chs")


#  _____       _                                 
# |  _  |     | |                                
# | | | |_   _| |_ ___ ___  _ __ ___   ___  ___  
# | | | | | | | __/ __/ _ \| '_ ` _ \ / _ \/ __| 
# \ \_/ / |_| | || (_| (_) | | | | | |  __/\__ \ 
#  \___/ \__,_|\__\___\___/|_| |_| |_|\___||___/ 


base1 = read.csv2(file = "BASELINE/base1final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))

base1_trim = subset.data.frame(base1, select = c(IDNO, DIABET07, DIABET12, DIAB01, DIABETES, GLUC12, FAST12, INSUL12, INSLN06, OHGA06))
base1_trim$new = 0

# new is the indicator that will say whether the FUP needs adjusting by 5 years

base2 = read.csv2(file = "BASELINE/base2final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))

base2_trim = subset.data.frame(base2, select = c(idno, GLU44, GLU244))
colnames(base2_trim) = c('IDNO', 'GLU44', 'GLU244')

base5y = read.csv2(file = "YEAR5/yr5newfinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))

base5y_trim = subset.data.frame(base5y, select = c(IDNO, DIAB01, DIABET57, GLU44, INSLN06, OHGA06))
colnames(base5y_trim) = c('IDNO', 'DIAB01', 'DIABET07', 'GLU44', 'INSLN06', 'OHGA06')
base5y_trim$new = 1

base_merge = merge(base1_trim, base2_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE)

base_merge2 = merge(base_merge, base5y_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )

base_merge2$GLU44.y[ is.na(base_merge2$GLU44.y) ] <- base_merge2$GLU44.x[is.na(base_merge2$GLU44.y)]
base_merge2$GLU44 = base_merge2$GLU44.y
base_merge2$DIAB01.y[ is.na(base_merge2$DIAB01.y) ] <- base_merge2$DIAB01.x[is.na(base_merge2$DIAB01.y)]
base_merge2$DIAB01 = base_merge2$DIAB01.y
base_merge2$DIABET07.y[ is.na(base_merge2$DIABET07.y) ] <- base_merge2$DIABET07.x[is.na(base_merge2$DIABET07.y)]
base_merge2$DIABET07 = base_merge2$DIABET07.y
base_merge2$INSLN06.y[ is.na(base_merge2$INSLN06.y) ] <- base_merge2$INSLN06.x[is.na(base_merge2$INSLN06.y)]
base_merge2$INSLN06 = base_merge2$INSLN06.y
base_merge2$OHGA06.y[ is.na(base_merge2$OHGA06.y) ] <- base_merge2$OHGA06.x[is.na(base_merge2$OHGA06.y)]
base_merge2$OHGA06 = base_merge2$OHGA06.y
base_merge2$new.y[ is.na(base_merge2$new.y) ] <- base_merge2$new.x[is.na(base_merge2$new.y)]
base_merge2$new = base_merge2$new.y
base_merge3 = subset(base_merge2 , select = -c(GLU44.y,GLU44.x,DIAB01.y,DIAB01.x, INSLN06.y, INSLN06.x, OHGA06.y, OHGA06.x
                                               , DIABET07.y, DIABET07.x, new.y, new.x))

# threshold for fasting plasma glucose  (≥ 126 mg/dL), 
# 2-hour postload plasma glucose (≥ 200 mg/dL), 

rm(base1)
rm(base2)
rm(base5y)

### YEAR 3
y3 = read.csv2(file = "YEAR3/yr3final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y3_trim = subset.data.frame(y3, select = c(IDNO, DIABET37, INSLN06, OHGA06))
rm(y3)
colnames(y3_trim) = c('IDNO', 'DIABET_3', 'INSLN06_3', 'OHGA06_3')

### YEAR 4
y4 = read.csv2(file = "YEAR4/yr4final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y4_trim = subset.data.frame(y4, select = c(IDNO, DIABET39, INSLN06, OHGA06))
rm(y4)
colnames(y4_trim) = c('IDNO', 'DIABET_4', 'INSLN06_4', 'OHGA06_4')

### YEAR 5
y5 = read.csv2(file = "YEAR5/yr5oldfinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y5_trim = subset.data.frame(y5, select = c(IDNO, DIABET29, INSLN06, OHGA06, GLU44))
rm(y5)
colnames(y5_trim) = c('IDNO', 'DIABET_5', 'INSLN06_5', 'OHGA06_5', 'GLU44_5')

### YEAR 6
y6 = read.csv2(file = "YEAR6/yr6final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y6_trim = subset.data.frame(y6, select = c(IDNO, DIABET59, INSLN06, OHGA06))
rm(y6)
colnames(y6_trim) = c('IDNO', 'DIABET_6', 'INSLN06_6', 'OHGA06_6')

### YEAR 7
y7 = read.csv2(file = "YEAR7/yr7final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y7_trim = subset.data.frame(y7, select = c(IDNO, DIABET59, INSLN06, OHGA06))
rm(y7)
colnames(y7_trim) = c('IDNO', 'DIABET_7', 'INSLN06_7', 'OHGA06_7')

### YEAR 8
y8 = read.csv2(file = "YEAR8/yr8final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y8_trim = subset.data.frame(y8, select = c(IDNO, DIABET59, INSLN06, OHGA06))
rm(y8)
colnames(y8_trim) = c('IDNO', 'DIABET_8', 'INSLN06_8', 'OHGA06_8')

### YEAR 9
y9 = read.csv2(file = "YEAR9/yr9final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y9_trim = subset.data.frame(y9, select = c(IDNO, DIABET59, INSLN06, OHGA06, GLU244))
rm(y9)
colnames(y9_trim) = c('IDNO', 'DIABET_9', 'INSLN06_9', 'OHGA06_9', 'GLU244_9')

### YEAR 10
y10 = read.csv2(file = "YEAR10/yr10final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y10_trim = subset.data.frame(y10, select = c(IDNO, DIABET59, INSLN06, OHGA06))
rm(y10)
colnames(y10_trim) = c('IDNO', 'DIABET_10', 'INSLN06_10', 'OHGA06_10')

### YEAR 11
y11 = read.csv2(file = "YEAR11/yr11final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y11_trim = subset.data.frame(y11, select = c(IDNO, DIABET59, INSLN06, OHGA06))
rm(y11)
colnames(y11_trim) = c('IDNO', 'DIABET_11', 'INSLN06_11', 'OHGA06_11')

### YEAR 12
y12 = read.csv2(file = "YEAR12/year12phonefinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y12_trim = subset.data.frame(y12, select = c(IDNO, DIABET32, INSLN06, OHGA06))
rm(y12)
colnames(y12_trim) = c('IDNO', 'DIABET_12', 'INSLN06_12', 'OHGA06_12')

### YEAR 13
y13 = read.csv2(file = "YEAR13/year13phonefinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y13_trim = subset.data.frame(y13, select = c(IDNO, DIABET32, INSLN06, OHGA06))
rm(y13)
colnames(y13_trim) = c('IDNO', 'DIABET_13', 'INSLN06_13', 'OHGA06_13')

### YEAR 14
y14 = read.csv2(file = "YEAR14/year14phonefinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y14_trim = subset.data.frame(y14, select = c(IDNO, DIABET32, INSLN06, OHGA06))
rm(y14)
colnames(y14_trim) = c('IDNO', 'DIABET_14', 'INSLN06_14', 'OHGA06_14')

### YEAR 15
y15 = read.csv2(file = "YEAR15/year15phonefinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y15_trim = subset.data.frame(y15, select = c(IDNO, DIABET32, INSLN06, OHGA06))
rm(y15)
colnames(y15_trim) = c('IDNO', 'DIABET_15', 'INSLN06_15', 'OHGA06_15')

### YEAR 16
y16 = read.csv2(file = "YEAR16/year16phonefinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y16_trim = subset.data.frame(y16, select = c(IDNO, DIABET32, INSLN06, OHGA06))
rm(y16)
colnames(y16_trim) = c('IDNO', 'DIABET_16', 'INSLN06_16', 'OHGA06_16')

### YEAR 17
y17 = read.csv2(file = "YEAR17/year17phonefinal.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
y17_trim = subset.data.frame(y17, select = c(IDNO, DIABET32, INSLN06, OHGA06))
rm(y17)
colnames(y17_trim) = c('IDNO', 'DIABET_17', 'INSLN06_17', 'OHGA06_17')

fup_merge = merge(y3_trim, y4_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y5_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y6_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y7_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y8_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y9_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y10_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y11_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y12_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y13_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y14_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y15_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y16_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
fup_merge = merge(fup_merge, y17_trim, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )

end_merge = merge(base_merge3, fup_merge, by.x = 'IDNO', by.y = 'IDNO', all = TRUE )
