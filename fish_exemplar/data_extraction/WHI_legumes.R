library(tibble) # for easier dataframes, and rather the saving to dta
library(haven)
setwd("V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/whios/WHI_OS_2014b/WHI_OS_2014a/data/whi/sas_data/")


#  _____       _                                 
# |  _  |     | |                                
# | | | |_   _| |_ ___ ___  _ __ ___   ___  ___  
# | | | | | | | __/ __/ _ \| '_ ` _ \ / _ \/ __| 
# \ \_/ / |_| | || (_| (_) | | | | | |  __/\__ \ 
#  \___/ \__,_|\__\___\___/|_| |_| |_|\___||___/ 

#F44_OS_PUB

F44_OS_PUB = read_sas(data_file = 'F44_OS_PUB.sas7bdat')

F44_OS_PUB = F44_OS_PUB[order(F44_OS_PUB$ID),]

#keep only diabetes-related observations
F44_OS_PUB_dedupe = F44_OS_PUB[which(F44_OS_PUB$F44VTYP == 1 & F44_OS_PUB$TCCODE >= 270000 
                                     & F44_OS_PUB$TCCODE <= 279999), ]

# keep only a single diabetes-related observation
F44_OS_PUB_dedupe2 = F44_OS_PUB_dedupe[!duplicated(F44_OS_PUB_dedupe$ID),]

F44_OS_PUB = F44_OS_PUB_dedupe2[,c("ID","TCCODE")]

F44_OS_PUB$TCCODE = as.numeric(F44_OS_PUB$TCCODE)

#OUTC_SELF_OS_PUB

OUTC_SELF_OS_PUB = read_sas(data_file = 'OUTC_SELF_OS_PUB.sas7bdat')

OUTC_SELF_OS_PUB = OUTC_SELF_OS_PUB[,c("ID", "F33DIABPILLS","F33DIABPILLSDY","F33DIABINSULIN",
                                       "F33DIABINSULINDY","F33DIABTX","F33DIABTXDY")]

OUTC_SELF_OS_PUB$F33DIABPILLS = as.factor(OUTC_SELF_OS_PUB$F33DIABPILLS)
levels(OUTC_SELF_OS_PUB$F33DIABPILLS) = c("No","Yes")

OUTC_SELF_OS_PUB$F33DIABINSULIN = as.factor(OUTC_SELF_OS_PUB$F33DIABINSULIN)
levels(OUTC_SELF_OS_PUB$F33DIABINSULIN) = c("No","Yes")

OUTC_SELF_OS_PUB$F33DIABTX = as.factor(OUTC_SELF_OS_PUB$F33DIABTX)
levels(OUTC_SELF_OS_PUB$F33DIABTX) = c("No","Yes")

# _____                                         
# |  ___|                                        
# | |____  ___ __   ___  ___ _   _ _ __ ___  ___ 
# |  __\ \/ / '_ \ / _ \/ __| | | | '__/ _ \/ __|
# | |___>  <| |_) | (_) \__ \ |_| | | |  __/\__ \
# \____/_/\_\ .__/ \___/|___/\__,_|_|  \___||___/
#           | |                                  
#           |_| 

F60_ITEM_OS_PUB = read_sas(data_file = 'F60_ITEM_OS_PUB.sas7bdat')

F60_ITEM_OS_PUB = F60_ITEM_OS_PUB[order(F60_ITEM_OS_PUB$ID),]

#keep only baseline observations
F60_ITEM_OS_PUB = F60_ITEM_OS_PUB[which(F60_ITEM_OS_PUB$F60VTYP == 1), ]

F60_ITEM_OS_PUB = F60_ITEM_OS_PUB[,c("ID","GRNPEAS","BEANS","BEANSOUP","REFRIED","GRNBEAN","SOY","NUTS")]

#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/

#DEM_OS_PUB

DEM_OS_PUB = read_sas(data_file = 'DEM_OS_PUB.sas7bdat')

DEM_OS_PUB = DEM_OS_PUB[,c("ID","AGE", "EDUC")]
DEM_OS_PUB$EDUC = as.factor(DEM_OS_PUB$EDUC)
levels(DEM_OS_PUB$EDUC) = c("Didn't go to school","Grade school (1-4 years)",
                            "Grade school (5-8 years)", "Some high school (9-11 years)",
                            "High school diploma or GED","Vocational or training school",
                            "Some college or Associate Degree","College graduate or Baccalaureate Degree",
                            "Some post-graduate or professional","Master's Degree",
                            "Doctoral Degree (Ph.D,M.D.,J.D.,etc.)")

#F80_OS_PUB

F80_OS_PUB = read_sas(data_file = 'F80_OS_PUB.sas7bdat')

F80_OS_PUB = F80_OS_PUB[order(F80_OS_PUB$ID),]

#keep only baseline observations
F80_OS_PUB = F80_OS_PUB[which(F80_OS_PUB$F80VTYP == 1), ]

F80_OS_PUB = F80_OS_PUB[,c("ID","BMIX","WAISTX")]

#F34_OS_PUB

F34_OS_PUB = read_sas(data_file = 'f34_os_pub.sas7bdat')
F34_OS_PUB = F34_OS_PUB[,c("ID","SMOKING","TEXPWK")]

F34_OS_PUB$SMOKING = as.factor(F34_OS_PUB$SMOKING)
levels(F34_OS_PUB$SMOKING) = c("Never Smoked","Past Smoker","Current Smoker")

#F60_NUTR_OS_PUB

F60_NUTR_OS_PUB = read_sas(data_file = 'F60_NUTR_OS_PUB.sas7bdat')

F60_NUTR_OS_PUB = F60_NUTR_OS_PUB[order(F60_NUTR_OS_PUB$ID),]

#keep only baseline observations
F60_NUTR_OS_PUB = F60_NUTR_OS_PUB[which(F60_NUTR_OS_PUB$F60VTYP == 1), ]

F60_NUTR_OS_PUB = F60_NUTR_OS_PUB[,c("ID","F60ENRGYJ","F60ALC","F60FIBER")]

#F32_OS_PUB

F32_OS_PUB = read_sas(data_file = 'F32_OS_PUB.sas7bdat')
F32_OS_PUB = F32_OS_PUB[,c("ID","DIABREL")]

F32_OS_PUB$DIABREL = as.factor(F32_OS_PUB$DIABREL)
levels(F32_OS_PUB$DIABREL) = c("No","Yes","Don't know")

#F30_OS_PUB

F30_OS_PUB = read_sas(data_file = 'F30_OS_PUB.sas7bdat')
F30_OS_PUB = F30_OS_PUB[,c("ID","HYPTPILN","CANC_F30","CARDREST" , "ANGINA", "HYPT", "CHF_F30")]

F30_OS_PUB$HYPTPILN = as.factor(F30_OS_PUB$HYPTPILN)
levels(F30_OS_PUB$HYPTPILN) = c("No","Yes")

F30_OS_PUB$CANC_F30 = as.factor(F30_OS_PUB$CANC_F30)
levels(F30_OS_PUB$CANC_F30) = c("No","Yes")

F30_OS_PUB$CARDREST = as.factor(F30_OS_PUB$CARDREST)
levels(F30_OS_PUB$CARDREST) = c("No","Yes")

F30_OS_PUB$ANGINA = as.factor(F30_OS_PUB$ANGINA)
levels(F30_OS_PUB$ANGINA) = c("No","Yes")

F30_OS_PUB$HYPT = as.factor(F30_OS_PUB$HYPT)
levels(F30_OS_PUB$HYPT) = c("No","Yes")

F30_OS_PUB$CHF_F30 = as.factor(F30_OS_PUB$CHF_F30)
levels(F30_OS_PUB$CHF_F30) = c("No","Yes")

#################################################################################################################
#################################################################################################################
#################################################################################################################

dataframes_list = list(DEM_OS_PUB,OUTC_SELF_OS_PUB,F60_ITEM_OS_PUB,F80_OS_PUB,F34_OS_PUB,
                       F60_NUTR_OS_PUB,F32_OS_PUB,F30_OS_PUB,F44_OS_PUB)

whi_legumes = Reduce(function(...) merge(..., by="ID", all=TRUE), dataframes_list)
colnames(whi_legumes)[1] <- "ID"

whi_legumes_tibble = as_tibble(whi_legumes)
save(whi_legumes,file="V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/whios/WHI_OS_2014b/whi_legumes.Rdata")
write_dta(data = whi_legumes_tibble, path = "V:/Studies/InterConnect/Internal/Other data sharing mechanisms/BioLINCC data_ US data/whios/WHI_OS_2014b/whi_legumes.dta")
