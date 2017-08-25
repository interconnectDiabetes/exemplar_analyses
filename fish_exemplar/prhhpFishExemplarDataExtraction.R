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

# _____                                         
# |  ___|                                        
# | |____  ___ __   ___  ___ _   _ _ __ ___  ___ 
# |  __\ \/ / '_ \ / _ \/ __| | | | '__/ _ \/ __|
# | |___>  <| |_) | (_) \__ \ |_| | | |  __/\__ \
# \____/_/\_\ .__/ \___/|___/\__,_|_|  \___||___/
#           | |                                  
#           |_|                                  
# FATTY


# ___  ___          _ _  __ _               
# |  \/  |         | (_)/ _(_)              
# | .  . | ___   __| |_| |_ _  ___ _ __ ___ 
# | |\/| |/ _ \ / _` | |  _| |/ _ \ '__/ __|
# | |  | | (_) | (_| | | | | |  __/ |  \__ \
# \_|  |_/\___/ \__,_|_|_| |_|\___|_|  |___/

             
#  _____              __                      _               
# /  __ \            / _|                    | |              
# | /  \/ ___  _ __ | |_ ___  _   _ _ __   __| | ___ _ __ ___ 
# | |    / _ \| '_ \|  _/ _ \| | | | '_ \ / _` |/ _ \ '__/ __|
# | \__/\ (_) | | | | || (_) | |_| | | | | (_| |  __/ |  \__ \
#  \____/\___/|_| |_|_| \___/ \__,_|_| |_|\__,_|\___|_|  |___/




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

