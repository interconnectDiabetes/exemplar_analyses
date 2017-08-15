# This file is used to create a dataframe for the fish exemplar
# Author: Paul Scherer

setwd("V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/cardia")

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
