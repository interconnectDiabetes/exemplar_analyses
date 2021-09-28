library(tibble) # for easier dataframes, and rather the saving to dta
library(haven)
library(dplyr)


setwd("~/mesa/Exam1/")

dres1 = read_sas(data_file = 'mesae1dres06192012.sas7bdat', catalog_file = 'formats.sas7bcat')
diet1 = read_sas(data_file = 'mesae1dietdres06192012.sas7bdat', catalog_file = 'formats.sas7bcat')

dres_temp = dres1[c("MESAID", "age1c", "diabet1", "dbhxtyp1",  "insln1c", "ohga1c", "diabins1", "glucos1c",  "gender1", "bmi1c",
"educ1", "cig1c", "leismt1c", "htnmed1c", "waistcm1", "cancer1", "highbp1", "race1c")]

dres_temp = dres_temp %>% 
  rename(
    mesaid = MESAID)

diet_vars = c(
  "MESAID",
  "alcn1c",
  "alc_day1c",
  "enrgyn1c",
  "svdnoodles1c",
  "svdstirfrdbeef1c",
  "svdmburrito1c",
  "svdmenchilada1c",
  "svdpicadillo1c",
  "svdmchile1c",
  "svdredchile1c",
  "svdgreenchile1c",
  "svdmeatpasta1c",
  "svdmtomatopasta1c",
  "svdmeatstew1c",
  "svdhamburger1c",
  "svdsteak1c",
  "svdsausage1c",
  "svdham1c",
  "svdhocks1c",
  "svdmburrito1c",
  "svdmenchilada1c",
  "svdarrozpollo1c",
  "svdmeatpasta1c",
  "svdmeatstew1c",
  "svdroastchicken1c",
  "svdfriedchicken1c",
  "fgfruit1c",
  "fgvgreenleafy1c",
  "fgvcrucifer1c", "fgvdyellow1c", "fgvother1c", "fgavocado1c", "fgtomato1c",
  'svdlettuce1c', 'svdspinach1c', 'svdcarrot1c', 'svdsquash1c', 'svdavacado1c', 'svdotherveg1c', 'svdstirfrdveg1c', 
  'svdhominy1c', 'svdbroccoli1c', 'svdtomato1c', "tfibrn1c", "fgsoda1c",
  'svdfriedfish1c', 'svdtuna1c', 'svdboiledfish1c', 'svdshrimp1c',
  'svdgreenbean1c', 'svdbean1c', 'svdpeasoup1c', 'svdfriedbeans1c', 'fglegumes1c',
  'svdsoymilk1c', 'svdstirfrdtofu1c', 'svdtofudessert1c', "fgsoy1c",
  'svdpotato1c', 'svdsweetpotato1c',
  "frqpeanuts1","svdpeanuts1c", "frqnuts1", "svdnuts1c", "svdsunflower1c", "frqsunflower1",
  'svdwholemilk1c', 'svdmilk2pct1c', 'svdskimmilk1c', 'svdplainyogurt1c', 'svdflavyogurt1c', 'svdfrozenyogurt1c', 'svdicecream1c',
  'svdcottage1c', 'svdcheddar1c', 'svdsweetmilk1c', 'svdmilkcoffeetea1c', 'svdcreamcoffeetea1c',
  'svdwholemilk1c', 'svdmilk2pct1c', 'svdskimmilk1c',
  'svdcottage1c', 'svdcheddar1c',
  'svdplainyogurt1c', 'svdflavyogurt1c', 'svdfrozenyogurt1c',
  'fgeggs1c', 'svdbrownrice1c', 'svddarkbread1c', 'svdnoodles1c', 'svdchowmein1c', 'svdpastasalad1c', 'svdcreampasta1c', 
  'svdmeatpasta1c', 'svdtomatopasta1c', 'svdmtomatopasta1c',
  'svdwhiterice1c', 'svdfriedrice1c', 'svdbrownrice1c', 'fgcoffee1c', 'fgtea1c', 'supdurcodoil1',
  'fgfish1c', 'fgvpotato1c', 'fgfriedpotato1c', 'fgppsalad1c', 'fgseedsnuts1c',
  'fgyogurt1c', 'fglfdairydessert1c', 'fgicecream1c', 'fgcottage1c', 'fglfmilk1c', 'fgwholemilk1c', 'fgcoffeecream1c', 'fghfcheese1c', 'fgcreamsoup1c',
  'fgeggs1c', 'fgwholegrain1c', 'fgwhitebread1c', 'fgsweetbread1c', 'fgwholegrain1c', 'fgtea1c', 'fgfatsoils1c', 'fgredmeat1c',
  'svdmromatopasta1c', 'fghfprocmeat1c', 'fgpoultry1c', 'svdliver1c'
)


diet_vars = unique(diet_vars)
diet_temp = diet1[diet_vars]
diet_temp = diet_temp %>% 
  rename(
    mesaid = MESAID)

#'fgfatoils1c'

setwd("~/mesa/Exam2/")
dres2 = read_sas(data_file = 'mesae2dres06222012.sas7bdat', catalog_file = 'formats.sas7bcat')
dres2_temp = dres2[c(
"mesaid", "diabins2", "glucos2c", "ohga2c", "insln2c", "age2c")]

setwd("~/mesa/Exam3/")
dres3 = read_sas(data_file = 'mesae3dres06222012.sas7bdat', catalog_file = 'formats.sas7bcat')
dres3_temp = dres3[c(
"mesaid", "glucos3c", "ohga3c", "insln3c", "diabins3", "age3c")]

setwd("~/mesa/Exam4/")
dres4 = read_sas(data_file = 'mesae4dres06222012.sas7bdat', catalog_file = 'formats.sas7bcat')
dres4_temp = dres4[c(
"mesaid", "glucos4c", "ohga4c", "insln4c", "diabins4", "age4c")]

setwd("~/mesa/Exam5/")
dres5 = read_sas(data_file = 'mesae5_drepos_20151101.sas7bdat')
dres5_temp = dres5[c(
"mesaid", "glucose5", "ohga5c", "insln5c", "diabins5", "age5c")]

setwd("~/mesa/event/")
events = read_sas(data_file = 'mesaevnoncvddres06192012.sas7bdat', catalog_file = 'formats.sas7bcat')
event_temp = events[c(
"MESAID", "diabtt")]

event_temp = event_temp %>% 
  rename(
    mesaid = MESAID)

dataframes_list = list(dres_temp, diet_temp, dres2_temp, dres3_temp, dres4_temp, dres5_temp, event_temp)

mesa = Reduce(function(...) merge(..., by="mesaid", all=TRUE), dataframes_list)

write_dta(data = mesa, path = "~/mesa_meat.dta")

