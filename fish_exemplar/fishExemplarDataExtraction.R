# This file is used to create a dataframe for the fish exemplar
# Author: Paul Scherer

setwd("V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/cardia")


#  _____       _                                 
# |  _  |     | |                                
# | | | |_   _| |_ ___ ___  _ __ ___   ___  ___  
# | | | | | | | __/ __/ _ \| '_ ` _ \ / _ \/ __| 
# \ \_/ / |_| | || (_| (_) | | | | | |  __/\__ \ 
#  \___/ \__,_|\__\___\___/|_| |_| |_|\___||___/ 

# AGE_BASE
dfa = read.csv2(file = "Y00/DATA/csv/aaref.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y00/DATA/csv/aaf01.csv", header = TRUE, sep = ",")
EXAMAGE = subset.data.frame(dfa, select = c(EXAMAGE, PID))
A01AGE1 = subset.data.frame(dfb, select = c(A01AGE1, PID))

# TYPE_DIAB

# PREV_DIAB
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y00/DATA/csv/aaf09gen.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "Y00/DATA/csv/aachem.csv", header = TRUE, sep = ",")
A08DIAB = subset.data.frame(dfa, select = c(A08DIAB, PID))
A09DIBST = subset.data.frame(dfb, select = c(A09DIBST, PID))
AL3_GLU = subset.data.frame(dfc, select = c(AL3_GLU, PID))

A08DIAB$A08DIAB = as.factor(A08DIAB$A08DIAB)
levels(x = A08DIAB$A08DIAB) = c("No","Yes", "Not Sure")

A09DIBST$A09DIBST = as.factor(A09DIBST$A09DIBST)
levels(x = A09DIBST$A09DIBST) = c("Still Have","Under Control", "Cured or Gone") 

# CASE_OBJ and CASE_OBJ_SELF
dfa = read.csv2(file = "Y02/DATA/csv/baf08v2.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y02/DATA/csv/baf09dib.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
B08DIAB = subset.data.frame(dfa, select = c(B08DIAB, PID))
B09DIBAG = subset.data.frame(dfb, select = c(B09DIBAG, PID))
EXAMAGE = subset.data.frame(dfc, select = c(EXAMAGE, PID))

B08DIAB$B08DIAB = as.factor(B08DIAB$B08DIAB)
levels(x = B08DIAB$B08DIAB) = c("No","Yes", "Not Sure")

dfd = read.csv2(file = "Y05/DATA/csv/caf08.csv", header = TRUE, sep = ",")
dfe = read.csv2(file = "Y05/DATA/csv/caf08.csv", header = TRUE, sep = ",")
dff = read.csv2(file = "Y05/DATA/csv/caref.csv", header = TRUE, sep = ",")
C08DIAB = subset.data.frame(dfd, select = c(C08DIAB, PID))
C08DIBAG = subset.data.frame(dfe, select = c(C08DIBAG, PID))
EXAMAGE = subset.data.frame(dff, select = c(EXAMAGE, PID))

C08DIAB$C08DIAB = as.factor(C08DIAB$C08DIAB)
levels(x = C08DIAB$C08DIAB) = c("No","Yes", "Not Sure")

dfg = read.csv2(file = "Y07/DATA/csv/daf08.csv", header = TRUE, sep = ",")
dfh = read.csv2(file = "Y07/DATA/csv/daf08.csv", header = TRUE, sep = ",")
dfi = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
dfj = read.csv2(file = "Y07/DATA/csv/daglu.csv", header = TRUE, sep = ",")
D08DIAB = subset.data.frame(dfg, select = c(D08DIAB, PID))
D08DIBAG = subset.data.frame(dfh, select = c(D08DIBAG, PID))
EXAMAGE = subset.data.frame(dfi, select = c(EXAMAGE, PID))
DL7GLU = subset.data.frame(dfj, select = c(DL7GLU, PID))

D08DIAB$D08DIAB = as.factor(D08DIAB$D08DIAB)
levels(x = D08DIAB$D08DIAB) = c("No","Yes", "Not Sure")

dfk = read.csv2(file = "Y10/DATA/csv/eaf08.csv", header = TRUE, sep = ",")
dfl = read.csv2(file = "Y10/DATA/csv/eaf08.csv", header = TRUE, sep = ",")
dfm = read.csv2(file = "Y10/DATA/csv/earef.csv", header = TRUE, sep = ",")
dfn = read.csv2(file = "Y10/DATA/csv/eaglu.csv", header = TRUE, sep = ",")
dfo = read.csv2(file = "Y10/DATA/csv/eaglu.csv", header = TRUE, sep = ",")
E08DIAB = subset.data.frame(dfk, select = c(E08DIAB, PID))
E08DIBAG = subset.data.frame(dfl, select = c(E08DIBAG, PID))
EXAMAGE = subset.data.frame(dfm, select = c(EXAMAGE, PID))
EL7GLU = subset.data.frame(dfn, select = c(EL7GLU, PID))
EL7GLU2H = subset.data.frame(dfo, select = c(EL7GLU2H, PID))

E08DIAB$E08DIAB = as.factor(E08DIAB$E08DIAB)
levels(x = E08DIAB$E08DIAB) = c("No","Yes", "Not Sure")

dfp = read.csv2(file = "Y15/DATA/csv/faf08.csv", header = TRUE, sep = ",")
dfq = read.csv2(file = "Y15/DATA/csv/faf08.csv", header = TRUE, sep = ",")
dfr = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
dfs = read.csv2(file = "Y15/DATA/csv/faglu.csv", header = TRUE, sep = ",")
dft = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
F08DIAB = subset.data.frame(dfp, select = c(F08DIAB, PID))
F08DIBAG = subset.data.frame(dfq, select = c(F08DIBAG, PID))
EXAMAGE = subset.data.frame(dfr, select = c(EXAMAGE, PID))
FL7GLU = subset.data.frame(dfs, select = c(FL7GLU, PID))
FDIABMED = subset.data.frame(dft, select = c(FDIABMED, PID))

F08DIAB$F08DIAB = as.factor(F08DIAB$F08DIAB)
levels(x = F08DIAB$F08DIAB) = c("No","Yes", "Not Sure")

FDIABMED$FDIABMED = as.factor(FDIABMED$FDIABMED)
levels(x = FDIABMED$FDIABMED) = c("No","Yes")

dfu = read.csv2(file = "Y20/DATA/csv/gaf08.csv", header = TRUE, sep = ",")
dfv = read.csv2(file = "Y20/DATA/csv/gaf08.csv", header = TRUE, sep = ",")
dfw = read.csv2(file = "Y20/DATA/csv/garef.csv", header = TRUE, sep = ",")
dfx = read.csv2(file = "Y20/DATA/csv/gaglu.csv", header = TRUE, sep = ",")
dfy = read.csv2(file = "Y20/DATA/csv/gaglu.csv", header = TRUE, sep = ",")
G08DIAB = subset.data.frame(dfu, select = c(G08DIAB, PID))
G08DIBAG = subset.data.frame(dfv, select = c(G08DIBAG, PID))
EXAMAGE = subset.data.frame(dfw, select = c(EXAMAGE, PID))
GL7GLU = subset.data.frame(dfx, select = c(GL7GLU, PID))
GL7GLU2H = subset.data.frame(dfy, select = c(GL7GLU2H, PID))
G83LDIA = NULL

G08DIAB$G08DIAB = as.factor(G08DIAB$G08DIAB)
levels(x = G08DIAB$G08DIAB) = c("No","Yes", "Not Sure")

dfz = read.csv2(file = "Y25/DATA/csv/haf08.csv", header = TRUE, sep = ",")
dfaa = read.csv2(file = "Y25/DATA/csv/haf08.csv", header = TRUE, sep = ",")
dfab = read.csv2(file = "Y25/DATA/csv/haref.csv", header = TRUE, sep = ",")
dfac = read.csv2(file = "Y25/DATA/csv/haglu.csv", header = TRUE, sep = ",")
dfad = read.csv2(file = "Y25/DATA/csv/haglu.csv", header = TRUE, sep = ",")
H08DIAB = subset.data.frame(dfz, select = c(H08DIAB, PID))
H08DIBAG = subset.data.frame(dfaa, select = c(H08DIBAG, PID))
EXAMAGE = subset.data.frame(dfab, select = c(EXAMAGE, PID))
HL7GLU = subset.data.frame(dfac, select = c(HL7GLU, PID))
HL7GLU2H = subset.data.frame(dfad, select = c(HL7GLU2H, PID))
H08MEDDIAA = NULL

H08DIAB$H08DIAB = as.factor(H08DIAB$H08DIAB)
levels(x = H08DIAB$H08DIAB) = c("No","Yes", "Not Sure")

# AGE_END_OBJ 
# AGE_END_OBJ_SELF
dfa = read.csv2(file = "Y02/DATA/csv/baf09dib.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
B09DIBAG = subset.data.frame(dfa, select = c(B09DIBAG, PID))
EXAMAGE = subset.data.frame(dfb, select = c(EXAMAGE, PID))

dfc = read.csv2(file = "Y05/DATA/csv/caf08.csv", header = TRUE, sep = ",")
dfd = read.csv2(file = "Y05/DATA/csv/caref.csv", header = TRUE, sep = ",")
C08DIBAG = subset.data.frame(dfc, select = c(C08DIBAG, PID))
EXAMAGE = subset.data.frame(dfd, select = c(EXAMAGE, PID))

dfe = read.csv2(file = "Y07/DATA/csv/daf08.csv", header = TRUE, sep = ",")
dff = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
D08DIBAG = subset.data.frame(dfe, select = c(D08DIBAG, PID))
EXAMAGE = subset.data.frame(dff, select = c(EXAMAGE, PID))

dfg = read.csv2(file = "Y10/DATA/csv/eaf08.csv", header = TRUE, sep = ",")
dfh = read.csv2(file = "Y10/DATA/csv/earef.csv", header = TRUE, sep = ",")
E08DIBAG = subset.data.frame(dfg, select = c(E08DIBAG, PID))
EXAMAGE = subset.data.frame(dfh, select = c(EXAMAGE, PID))

dfi = read.csv2(file = "Y15/DATA/csv/faf08.csv", header = TRUE, sep = ",")
dfj = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
F08DIBAG = subset.data.frame(dfi, select = c(F08DIBAG, PID))
EXAMAGE = subset.data.frame(dfj, select = c(EXAMAGE, PID))

dfk = read.csv2(file = "Y20/DATA/csv/gaf08.csv", header = TRUE, sep = ",")
dfl = read.csv2(file = "Y20/DATA/csv/garef.csv", header = TRUE, sep = ",")
G08DIBAG = subset.data.frame(dfk, select = c(G08DIBAG, PID))
EXAMAGE = subset.data.frame(dfl, select = c(EXAMAGE, PID))

dfm = read.csv2(file = "Y25/DATA/csv/haf08.csv", header = TRUE, sep = ",")
dfn = read.csv2(file = "Y25/DATA/csv/haref.csv", header = TRUE, sep = ",")
H08DIBAG = subset.data.frame(dfm, select = c(H08DIBAG, PID))
EXAMAGE = subset.data.frame(dfn, select = c(EXAMAGE, PID))

dfo = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
dfp = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
EX2_AGE = subset.data.frame(dfo, select = c(EX2_AGE, PID))
EXAMAGE = subset.data.frame(dfp, select = c(EXAMAGE, PID))

dfq = read.csv2(file = "Y05/DATA/csv/caref.csv", header = TRUE, sep = ",")
dfr = read.csv2(file = "Y05/DATA/csv/caref.csv", header = TRUE, sep = ",")
EX3_AGE = subset.data.frame(dfq, select = c(EX3_AGE, PID))
EXAMAGE = subset.data.frame(dfr, select = c(EXAMAGE, PID))

dfs = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
dft = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
EX4_AGE = subset.data.frame(dfs, select = c(EX4_AGE, PID))
EXAMAGE = subset.data.frame(dft, select = c(EXAMAGE, PID))

dfu = read.csv2(file = "Y10/DATA/csv/earef.csv", header = TRUE, sep = ",")
dfv = read.csv2(file = "Y10/DATA/csv/earef.csv", header = TRUE, sep = ",")
EX5_AGE = subset.data.frame(dfu, select = c(EX5_AGE, PID))
EXAMAGE = subset.data.frame(dfv, select = c(EXAMAGE, PID))

dfw = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
dfx = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
EX6_AGE = subset.data.frame(dfw, select = c(EX6_AGE, PID))
EXAMAGE = subset.data.frame(dfx, select = c(EXAMAGE, PID))

dfy = read.csv2(file = "Y20/DATA/csv/garef.csv", header = TRUE, sep = ",")
dfz = read.csv2(file = "Y20/DATA/csv/garef.csv", header = TRUE, sep = ",")
EX7_AGE = subset.data.frame(dfy, select = c(EX7_AGE, PID))
EXAMAGE = subset.data.frame(dfz, select = c(EXAMAGE, PID))

dfaa = read.csv2(file = "Y25/DATA/csv/haref.csv", header = TRUE, sep = ",")
dfab = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
EX8_AGE = subset.data.frame(dfaa, select = c(EX8_AGE, PID))
EXAMAGE = subset.data.frame(dfab, select = c(EXAMAGE, PID))
                                          
# _____                                         
# |  ___|                                        
# | |____  ___ __   ___  ___ _   _ _ __ ___  ___ 
# |  __\ \/ / '_ \ / _ \/ __| | | | '__/ _ \/ __|
# | |___>  <| |_) | (_) \__ \ |_| | | |  __/\__ \
# \____/_/\_\ .__/ \___/|___/\__,_|_|  \___||___/
#           | |                                  
#           |_|                                  
# FATTY
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
fatty = subset.data.frame(dfa, select = c(A06MFF0100, PID))

# FRIED
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
fried = subset.data.frame(dfa, select = c(A06MFF0200, A06MSF0100, PID))

# LEAN
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
lean = subset.data.frame(dfa, select = c(A06MFL0100, PID))

# NONFISH
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
nonfish = subset.data.frame(dfa, select = c(A06MSL0100, PID))

# TOTAL
dfa = read.csv2(file = "Y00/DATA/csv/aaf06mj.csv", header = TRUE, sep = ",")
total = subset.data.frame(dfa, select = c(A06FISH, PID))

# ___  ___          _ _  __ _               
# |  \/  |         | (_)/ _(_)              
# | .  . | ___   __| |_| |_ _  ___ _ __ ___ 
# | |\/| |/ _ \ / _` | |  _| |/ _ \ '__/ __|
# | |  | | (_) | (_| | | | | |  __/ |  \__ \
# \_|  |_/\___/ \__,_|_|_| |_|\___|_|  |___/
# SEX
dfa = read.csv2(file = "Y00/DATA/csv/aaf01.csv", header = TRUE, sep = ",")
A01SEX = subset.data.frame(dfa, select = c(A01SEX, PID))

H08DIAB$H08DIAB = as.factor(H08DIAB$H08DIAB)
levels(x = H08DIAB$H08DIAB) = c("Male","Female")

# BMI
dfa = read.csv2(file = "Y00/DATA/csv/aaf20.csv", header = TRUE, sep = ",")
A20BMI = subset.data.frame(dfa, select = c(A20BMI, PID))
             
#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/

# Education a bit weird have to or them
dfa = read.csv2(file = "Y00/DATA/csv/aaf01.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y00/DATA/csv/aaf03.csv", header = TRUE, sep = ",")
A01ED1 = subset.data.frame(dfa, select = c(A01ED1, PID))
A03ED = subset.data.frame(dfb, select = c(A03ED, PID))

A01ED1$A01ED1 = as.factor(A01ED1$A01ED1)
A03ED$A03ED = as.factor(A03ED$A03ED)


# Smoking also or them
dfa = read.csv2(file = "Y00/DATA/csv/aaf10.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y00/DATA/csv/aaf01.csv", header = TRUE, sep = ",")
A10SMOKE = subset.data.frame(dfa, select = c(A10SMOKE, PID))
A01SMNOW = subset.data.frame(dfb, select = c(A01SMNOW, PID))

A10SMOKE$A10SMOKE = as.factor(A10SMOKE$A10SMOKE)
levels(x = A10SMOKE$A10SMOKE) = c("No","Yes")

A01SMNOW$A01SMNOW = as.factor(A01SMNOW$A01SMNOW)
levels(x = A01SMNOW$A01SMNOW) = c("No","Yes")

# PA
dfa = read.csv2(file = "Y00/DATA/csv/aaf19.csv", header = TRUE, sep = ",")
A19MODWK = subset.data.frame(dfa, select = c(A19MODWK, PID))

# ALCOHOL
dfa = read.csv2(file = "Y00/DATA/csv/aaf06a.csv", header = TRUE, sep = ",")
A06ALCHL = subset.data.frame(dfa, select = c(A06ALCHL, PID))

# FAM_DIAB
dfa = read.csv2(file = "Y00/DATA/csv/aaf11.csv", header = TRUE, sep = ",")
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
dfa = read.csv2(file = "Y00/DATA/csv/aaf09gen.csv", header = TRUE, sep = ",")
A09HRTAK = subset.data.frame(dfa, select = c(A09HRTAK, PID))
A09HRTAK$A09HRTAK = as.factor(A09HRTAK$A09HRTAK)
levels(x = A09HRTAK$A09HRTAK) = c("No","Yes", "Not Sure")

# STROKE
# nothing here mate

# CANCER
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
A08CANCR = subset.data.frame(dfa, select = c(A08CANCR, PID))
A08CANCR$A08CANCR = as.factor(A08CANCR$A08CANCR)
levels(x = A08CANCR$A08CANCR) = c("No","Yes", "Not Sure") 

# HYPERTENSION
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
A08HBP = subset.data.frame(dfa, select = c(A08HBP, PID))
A08HBP$A08HBP = as.factor(A08HBP$A08HBP)
levels(x = A08HBP$A08HBP) = c("No","Yes", "Not Sure") 

# E_INTAKE
dfa = read.csv2(file = "Y00/DATA/csv/aaf06a.csv", header = TRUE, sep = ",")
A06CALO = subset.data.frame(dfa, select = c(A06CALO, PID))

# FRUIT
dfa = read.csv2(file = "Y00/DATA/csv/aaf06mj.csv", header = TRUE, sep = ",")
A06FRUIT = subset.data.frame(dfa, select = c(A06FRUIT, PID))

# VEG
dfa = read.csv2(file = "Y00/DATA/csv/aaf06mj.csv", header = TRUE, sep = ",")
A06VEGETABLE = subset.data.frame(dfa, select = c(A06VEGETABLE, PID))

# FIBER
dfa = read.csv2(file = "Y00/DATA/csv/aaf06a.csv", header = TRUE, sep = ",")
A06FIBER = subset.data.frame(dfa, select = c(A06FIBER, PID))

# RED_MEAT
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
red = subset.data.frame(dfa, select = c(A06MRF0100,A06MRF0200,A06MRF0300, A06MRF0400, PID))

# PROC-MEAT
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
proc = subset.data.frame(dfa, select = c(A06MCF0200,A06MCF0100 , PID))

# SUG_BEVS
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
A06BVS0400 = subset.data.frame(dfa, select = c(A06BVS0400, PID))

# MEDS
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
A08BPMED = subset.data.frame(dfa, select = c(A08BPMED, PID))
A08BPMED$A08BPMED = as.factor(A08BPMED$A08BPMED)
levels(x = A08BPMED$A08BPMED) = c("No","Yes", "Not Sure")

# WAIST
dfa = read.csv2(file = "Y00/DATA/csv/aaf20.csv", header = TRUE, sep = ",")
A20WST = subset.data.frame(dfa, select = c(A20WST, PID))

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