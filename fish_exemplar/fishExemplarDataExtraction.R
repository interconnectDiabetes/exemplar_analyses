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
age_base = subset.data.frame(dfa, select = c(EXAMAGE, PID))
age_base1 = subset.data.frame(dfb, select = c(A01AGE1, PID))

# TYPE_DIAB

# PREV_DIAB
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y00/DATA/csv/aaf09gen.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "Y00/DATA/csv/aachem.csv", header = TRUE, sep = ",")
a08diab = subset.data.frame(dfa, select = c(A08DIAB, PID))
a09dibst = subset.data.frame(dfb, select = c(A09DIBST, PID))
al3glu = subset.data.frame(dfb, select = c(AL3_GLU, PID))


# CASE_OBJ and CASE_OBJ_SELF
dfa = read.csv2(file = "Y02/DATA/csv/baf08v2.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y02/DATA/csv/baf09dib.csv", header = TRUE, sep = ",")
dfc = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
B08DIAB = subset.data.frame(dfa, select = c(B08DIAB, PID))
B09DIBAG = subset.data.frame(dfb, select = c(B09DIBAG, PID))
EXAMAGE = subset.data.frame(dfc, select = c(EXAMAGE, PID))

dfd = read.csv2(file = "Y05/DATA/csv/caf08.csv", header = TRUE, sep = ",")
dfe = read.csv2(file = "Y05/DATA/csv/caf08.csv", header = TRUE, sep = ",")
dff = read.csv2(file = "Y05/DATA/csv/caref.csv", header = TRUE, sep = ",")
C08DIAB = subset.data.frame(dfd, select = c(C08DIAB, PID))
C08DIBAG = subset.data.frame(dfe, select = c(C08DIBAG, PID))
EXAMAGE = subset.data.frame(dff, select = c(EXAMAGE, PID))

dfg = read.csv2(file = "Y07/DATA/csv/daf08.csv", header = TRUE, sep = ",")
dfh = read.csv2(file = "Y07/DATA/csv/daf08.csv", header = TRUE, sep = ",")
dfi = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
dfj = read.csv2(file = "Y07/DATA/csv/daglu.csv", header = TRUE, sep = ",")
D08DIAB = subset.data.frame(dfg, select = c(D08DIBAG, PID))
D08DIBAG = subset.data.frame(dfh, select = c(D08DIBAG, PID))
EXAMAGE = subset.data.frame(dfi, select = c(EXAMAGE, PID))
DL7GLU = subset.data.frame(dfj, select = c(DL7GLU, PID))

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

dfz = read.csv2(file = "Y25/DATA/csv/haf08.csv", header = TRUE, sep = ",")
dfaa = read.csv2(file = "Y25/DATA/csv/haf08.csv", header = TRUE, sep = ",")
dfab = read.csv2(file = "Y25/DATA/csv/haref.csv", header = TRUE, sep = ",")
dfac = read.csv2(file = "Y25/DATA/csv/haglu.csv", header = TRUE, sep = ",")
dfad = read.csv2(file = "Y25/DATA/csv/haglu.csv", header = TRUE, sep = ",")
H08DIAB = subset.data.frame(dfaz, select = c(H08DIAB, PID))
H08DIBAG = subset.data.frame(dfaa, select = c(H08DIBAG, PID))
EXAMAGE = subset.data.frame(dfab, select = c(EXAMAGE, PID))
HL7GLU = subset.data.frame(dfac, select = c(HL7GLU, PID))
HL7GLU2H = subset.data.frame(dfad, select = c(HL7GLU2H, PID))
H08MEDDIAA = NULL


# AGE_END_OBJ 
# AGE_END_OBJ_SELF
dfa = read.csv2(file = "Y02/DATA/csv/baf09dib.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
B09DIBAG = subset.data.frame(dfaz, select = c(B09DIBAG, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfc = read.csv2(file = "Y02/DATA/csv/caf08.csv", header = TRUE, sep = ",")
dfd = read.csv2(file = "Y02/DATA/csv/caref.csv", header = TRUE, sep = ",")
C08DIBAG = subset.data.frame(dfaz, select = c(C08DIBAG, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfe = read.csv2(file = "Y07/DATA/csv/daf08.csv", header = TRUE, sep = ",")
dff = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
D08DIBAG = subset.data.frame(dfaz, select = c(D08DIBAG, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfg = read.csv2(file = "Y10/DATA/csv/eaf08.csv", header = TRUE, sep = ",")
dfh = read.csv2(file = "Y10/DATA/csv/earef.csv", header = TRUE, sep = ",")
E08DIBAG = subset.data.frame(dfaz, select = c(E08DIBAG, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfi = read.csv2(file = "Y15/DATA/csv/faf08.csv", header = TRUE, sep = ",")
dfj = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
F08DIBAG = subset.data.frame(dfaz, select = c(F08DIBAG, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfk = read.csv2(file = "Y20/DATA/csv/gaf08.csv", header = TRUE, sep = ",")
dfl = read.csv2(file = "Y20/DATA/csv/garef.csv", header = TRUE, sep = ",")
G08DIBAG = subset.data.frame(dfaz, select = c(G08DIBAG, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfm = read.csv2(file = "Y25/DATA/csv/haf08.csv", header = TRUE, sep = ",")
dfn = read.csv2(file = "Y25/DATA/csv/haref.csv", header = TRUE, sep = ",")
H08DIBAG = subset.data.frame(dfaz, select = c(H08DIBAG, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfo = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
dfp = read.csv2(file = "Y02/DATA/csv/baref.csv", header = TRUE, sep = ",")
EX2_AGE = subset.data.frame(dfaz, select = c(EX2_AGE, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfq = read.csv2(file = "Y05/DATA/csv/caref.csv", header = TRUE, sep = ",")
dfr = read.csv2(file = "Y05/DATA/csv/caref.csv", header = TRUE, sep = ",")
EX3_AGE = subset.data.frame(dfaz, select = c(EX3_AGE, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfs = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
dft = read.csv2(file = "Y07/DATA/csv/daref.csv", header = TRUE, sep = ",")
EX4_AGE = subset.data.frame(dfaz, select = c(EX4_AGE, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfu = read.csv2(file = "Y10/DATA/csv/earef.csv", header = TRUE, sep = ",")
dfv = read.csv2(file = "Y10/DATA/csv/earef.csv", header = TRUE, sep = ",")
EX5_AGE = subset.data.frame(dfaz, select = c(EX5_AGE, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfw = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
dfx = read.csv2(file = "Y15/DATA/csv/faref.csv", header = TRUE, sep = ",")
EX6_AGE = subset.data.frame(dfaz, select = c(EX6_AGE, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfy = read.csv2(file = "Y20/DATA/csv/garef.csv", header = TRUE, sep = ",")
dfz = read.csv2(file = "Y20/DATA/csv/garef.csv", header = TRUE, sep = ",")
EX7_AGE = subset.data.frame(dfaz, select = c(EX7_AGE, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))

dfaa = read.csv2(file = "Y25/DATA/csv/haref.csv", header = TRUE, sep = ",")
dfab = read.csv2(file = "Y25/DATA/csv/haref.csv", header = TRUE, sep = ",")
EX8_AGE = subset.data.frame(dfaz, select = c(EX8_AGE, PID))
EXAMAGE = subset.data.frame(dfaa, select = c(EXAMAGE, PID))
                                          
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
sex = subset.data.frame(dfa, select = c(A01SEX, PID))

# BMI_CAT
dfa = read.csv2(file = "Y00/DATA/csv/aaf20.csv", header = TRUE, sep = ",")
bmicat = subset.data.frame(dfa, select = c(A20BMI, PID))
             
#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/

# Education a bit weird have to or them
dfa = read.csv2(file = "Y00/DATA/csv/aaf01.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y00/DATA/csv/aaf03.csv", header = TRUE, sep = ",")
education = subset.data.frame(dfa, select = c(A01ED1, PID))
education1 = subset.data.frame(dfb, select = c(A03ED, PID))

# Smoking also or them
dfa = read.csv2(file = "Y00/DATA/csv/aaf10.csv", header = TRUE, sep = ",")
dfb = read.csv2(file = "Y00/DATA/csv/aaf01.csv", header = TRUE, sep = ",")
smoking = subset.data.frame(dfa, select = c(A10SMOKE, PID))
smoking1 = subset.data.frame(dfb, select = c(A01SMNOW, PID))

# PA
dfa = read.csv2(file = "Y00/DATA/csv/aaf10.csv", header = TRUE, sep = ",")
pa = subset.data.frame(dfa, select = c(A19MODWK, PID))

# ALCOHOL
dfa = read.csv2(file = "Y00/DATA/csv/aaf06a.csv", header = TRUE, sep = ",")
pa = subset.data.frame(dfa, select = c(A06ALCHL, PID))

# FAM_DIAB
dfa = read.csv2(file = "Y00/DATA/csv/aaf11.csv", header = TRUE, sep = ",")
famdiab = subset.data.frame(dfa, select = c(A11MDIAB,A11FDIAB,A11BDIAB, A11SDIAB, PID))

# MI
dfa = read.csv2(file = "Y00/DATA/csv/aaf09gen.csv", header = TRUE, sep = ",")
pa = subset.data.frame(dfa, select = c(A09HRTAK, PID))

# STROKE
# nothing here mate

# CANCER
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
cancer = subset.data.frame(dfa, select = c(A08CANCR, PID))

# HYPERTENSION
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
hyper = subset.data.frame(dfa, select = c(A08HBP, PID))

# E_INTAKE
dfa = read.csv2(file = "Y00/DATA/csv/aaf06a.csv", header = TRUE, sep = ",")
e_intake = subset.data.frame(dfa, select = c(A06CALO, PID))

# FRUIT
dfa = read.csv2(file = "Y00/DATA/csv/aaf06mj.csv", header = TRUE, sep = ",")
fruit = subset.data.frame(dfa, select = c(A06FRUIT, PID))

# VEG
dfa = read.csv2(file = "Y00/DATA/csv/aaf06mj.csv", header = TRUE, sep = ",")
veg = subset.data.frame(dfa, select = c(A06VEGETABLE, PID))

# FIBER
dfa = read.csv2(file = "Y00/DATA/csv/aaf06a.csv", header = TRUE, sep = ",")
fiber = subset.data.frame(dfa, select = c(A06FIBER, PID))

# RED_MEAT
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
red = subset.data.frame(dfa, select = c(A06MRF0100,A06MRF0200,A06MRF0300, A06MRF0400, PID))

# PROC-MEAT
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
proc = subset.data.frame(dfa, select = c(A06MCF0200,A06MCF0100 , PID))

# SUG_BEVS
dfa = read.csv2(file = "Y00/DATA/csv/aaf06fg.csv", header = TRUE, sep = ",")
sugbev = subset.data.frame(dfa, select = c(A06BVS0400, PID))

# MEDS
dfa = read.csv2(file = "Y00/DATA/csv/aaf08v2.csv", header = TRUE, sep = ",")
meds = subset.data.frame(dfa, select = c(A08BPMED, PID))

# WAIST
dfa = read.csv2(file = "Y00/DATA/csv/aaf20.csv", header = TRUE, sep = ",")
waist = subset.data.frame(dfa, select = c(A20WST, PID))

#################################################################################################################
#################################################################################################################
#################################################################################################################