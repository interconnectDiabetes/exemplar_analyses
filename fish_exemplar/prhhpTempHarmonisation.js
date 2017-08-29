dataframes_list = list(PA17,PE3,prevalence,case_b,case_c,age_end_b,age_end_c,
  PA229,PA228,PA227,BMI1,PA28,PA42,PA52,PA78,PE34,PA60,PA194,PE66,PA62,PA237,
  PA238,PA239,PA233,PA235,PA236,PA77,PA221,PA223,PA224,PA225,PA250,PA125)


// agebase
// ages are represented in ranges in this dataset
// hence we set the midpoint of the ages as the value
age = $('PA17').value();
if (age == 1){
  val = (35+39)/2;
} else if (age == 2) {
  val = (40+44)/2;
} else if (age == 3) {
  val = (45+49)/2;
} else if (age == 4) {
  val = (50+54)/2;
} else if (age == 5) {
  val = (55+59)/2;
} else if (age == 6) {
  val = (60+64)/2;
} else if (age == 7) {
  val = (65+69)/2;
} else if (age == 8) {
  val = (70+74)/2;
} else if (age == 9) {
  val = (75+79)/2;
} else {
  val = -1;
}

// type_diab assume all type 2

// prevdiab
PA118 = $('PA118').value();
PA119 = $('PA119').value();
PA199 = $('PA199').value();
PA211 = $('PA211').value();
PA214 = $('PA214').value();

prevalent if: (PA118=1) OR (PA119= 2or 3) OR (PA199=1) 
OR (if PA214=0 & PA211 ≥ 140md/dl) OR 
(if PA214=1 & PA211 ≥126mg/dl) 

if (PA118 == 1 || PA119 == 2 || PA119 == 3 || PA199 == 1){
  out = 1;
} else if (PA214 == 0 && PA211 >= 140) {
  out = 1;
} else if (PA214 == 1 && PA211 >= 126) {
  out = 1;
} else if (PA118 == null && PA119 == null && PA119 == null && PA199 == null && PA214 == null && PA211 == null) {
  out = -1
} else {
  out = 0;
}

// case obj
PB146 = $('PB146').value();
PB160 = $('PB160').value();
PC106 = $('PC106').value();
PC21 = $('PC21').value();
PC22 = $('PC22').value();
PC120 = $('PC120').value();
PC119 = $('PC119').value();

if (PB146 == null && PB160 == null && PC106 == null && PC21 == null && PC22 == null && PC120 == null && PC119 == null) {
  out = -1;
} else if (PB146 == 1 && PB160 >= 126) {
  out = 1;
} else if (PC106==1 && PC21==1 || PC22==1 && PC120==0 && PC119 >= 140 || PC120==1 && PC119 >= 126){
  out = 1;
} else {
  out = 0;
}

// case obj self
PB146 = $('PB146').value();
PB160 = $('PB160').value();
PC106 = $('PC106').value();
PC21 = $('PC21').value();
PC22 = $('PC22').value();
PC120 = $('PC120').value();
PC119 = $('PC119').value();

if (PB146 == null && PB160 == null && PC106 == null && PC21 == null && PC22 == null && PC120 == null && PC119 == null) {
  out = -1;
} else if (PB146 == 1 || PB160 >= 126) {
  out = 1;
} else if (PC106==1 && PC21==1 || PC22==1 && PC120==0 && PC119 >= 140 || PC120==1 && PC119 >= 126){
  out = 1;
} else {
  out = 0;
} 

// FUP obj and fup self
if case= (PB216/2) or (PC121/2) 
if non-case=  (PB13 - PA17) or (PC5 - PA17)

PB216 = $('PB216').value();
PC121 = $('PC121').value();
PB13 = $('PB13').value();
PA17 = $('PA17').value();

PB146 = $('PB146').value();
PB160 = $('PB160').value();
PC106 = $('PC106').value();
PC21 = $('PC21').value();
PC22 = $('PC22').value();
PC120 = $('PC120').value();
PC119 = $('PC119').value();

if (PB146 == null && PB160 == null && PC106 == null && PC21 == null && PC22 == null && PC120 == null && PC119 == null) {
  case_sign = -1;
} else if (PB146 == 1 || PB160 >= 126) {
  case_sign = 1;
} else if (PC106==1 && PC21==1 || PC22==1 && PC120==0 && PC119 >= 140 || PC120==1 && PC119 >= 126){
  case_sign = 1;
} else {
  case_sign = 0;
} 

if (case_sign == 1){
  if (PB216 == null && PB121 == null){
    fup = -1;
  } else {
    fup = Math.max((PB216/2), (PB121/2));
  }
} else if (case_sign == 0) {
  if (PB13 == null && PA17 == null && PC5 == null) {
    fup = -1;
  } else {
    fup = Math.max((PB13 - PA17), (PC5 - PA17));
  }
} else {
  fup = -1;
}


// fatty

// fresh

// fried

// lean

// nonfish

// salt

// ssd

// total

// sex

// bmi

// bmi_cat

// EDUCATION

// SMOKING

// PA

// ALCOHOL

// FAM_DIAB

// MI

// STROKE

// CANCR

// HYPERTENSION

// E_INTAKE

// COV_FRUIT

// COV_VEG

// COV_FIBER

// COV_RED_MEAT

// COV_PROC_MEAT

// COV_SUG_BEVS

// MEDS

// WAIST

// SUPPLEMENTS

// COMORBID

// MEAT
