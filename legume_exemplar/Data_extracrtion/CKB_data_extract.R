
library(tibble) # for easier dataframes, and rather the saving to dta
library(haven)
setwd("C:\\Users\\trpb2\\Downloads\\DAR-00095-UK")

baseline = read.csv2(file = "export.BASELINE.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
endpoints = read.csv2(file = "export.ENDPOINTS.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))
extra = read.csv2(file = "extra_column.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = (""))

baseline$study_date = as.Date(baseline$study_date, format='%d%b%Y')
baseline$censoring_date = as.Date(baseline$censoring_date, format='%d%b%y')
endpoints$c_ep0042_date = as.Date(endpoints$c_ep0042_date, format='%d%b%y')
endpoints$c_ep0048_date = as.Date(endpoints$c_ep0048_date, format='%d%b%y')

baseline$bmi_calc = as.numeric(as.character(baseline$bmi_calc))
baseline$met = as.numeric(as.character(baseline$met))

dataframes_list = list(baseline,endpoints,extra)

CKB = Reduce(function(...) merge(..., by="csid", all=TRUE), dataframes_list)
colnames(CKB)[1] <- "ID"

CKB_tibble = as_tibble(CKB)
# WAIST
save(CKB,file="C:\\Users\\trpb2\\Downloads\\DAR-00095-UK\\CKB_r.Rdata")
write_dta(data = CKB_tibble, path = "C:\\Users\\trpb2\\Downloads\\DAR-00095-UK\\CKB.dta")
