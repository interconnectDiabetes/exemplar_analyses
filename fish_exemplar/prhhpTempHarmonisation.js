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

PB216 = $('PB216').value();
PC121 = $('PC121').value();
PB13 = $('PB13').value();
PC5 = $('PC5').value();
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
  if (PB216 == null && PC121 == null){
    fup = -1;
  } else {
    fup = Math.min((PB216/2), (PC121/2));
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
PA229 = $('PA229').value();
PA229 = PA229 * 28.3;

//salt

// ssd
PA228 = $('PA228').value();
PA228 = PA228 * 20;

// total
PA227 = $('PA227').value();
PA227 = PA227 * 28.3;
PA229 = $('PA229').value();
PA229 = PA229 * 28.3;
PA228 = $('PA228').value();
PA228 = PA228 * 20;

sum = PA227 + PA229 + PA228;


// sex all men

// bmi
bmi = $('BMI01').value();

// bmi_cat
bmi = $('BMI01').value();
if (bmi >= 25){
  bmi_cat = 1;
} else if (bmi > 0 && bmi < 25){
  bmi_cat = 0;
} else {
  bmi_cat = -1;
}

// EDUCATION
education = $('PA28').value();

// SMOKING
smoking = $('PA42').value();

// PA
pa = $('PA52').value();

// ALCOHOL
pa = $('PA78').value();

// FAM_DIAB
famhist = $('PA78').value();

if (famhist == 0){
  fam_diab = 0;
} else if (famhist == 1 || famhist == 2 || famhist == 3) {
  fam_diab = 1;
} else {
  fam_diab = -1;
}


// MI

mi = $('PA60').value();

if (mi == 2){
  miout = 1;
} else if (mi == 0 || mi == 1 || mi == 3) {
  miout = 0;
} else {
  miout = -1;
}

// STROKE
stroke = $('PA194').value();

if (stroke == 1){
  stro = 1;
} else if (stroke == 0 || stroke == 2) {
  stro = 0;
} else {
  stro = -1;
}

// CANCER NOT RECORDED

// HYPERTENSION
hypertension = $('PE66').value();

if (hypertension == 1){
  hype = 1;
} else if (hypertension == 0 || hypertension == 2) {
  hype = 0;
} else {
  hype = -1;
}

// E_INTAKE
etake = $('PA63').value();

// COV_FRUIT
a = $('PA237').value();
b = $('PA238').value();
c = $('PA239').value();

if (a == null && b == null && c == null) {
  sum = -1;
} else {
  sum = a+b+c;
}

// COV_VEG
a = $('PA233').value();
b = $('PA235').value();
c = $('PA236').value();

if (a == null && b == null && c == null) {
  sum = -1;
} else {
  sum = a+b+c;
}

// COV_FIBER
fiber = $('PA77').value();

// COV_RED_MEAT
a = $('PA221').value();
b = $('PA223').value();

if (a == null && b == null ) {
  sum = -1;
} else {
  sum = a+b;
}

// COV_PROC_MEAT
a = $('PA224').value();
b = $('PA225').value();

if (a == null && b == null ) {
  sum = -1;
} else {
  sum = a+b;
}

// COV_SUG_BEVS
bevs = $('PA250').value();

// MEDS
meds = $('PA125').value();

if (meds == 1){
  hype = 1;
} else if (meds == 0 || meds == 2) {
  hype = 0;
} else {
  hype = -1;
}


// WAIST
// SUPPLEMENTS

// COMORBID

//hypertension
hypertension = $('PE66').value();

if (hypertension == 1){
  hype = 1;
} else if (hypertension == 0 || hypertension == 2) {
  hype = 0;
} else {
  hype = -1;
}

//mi
mi = $('PA60').value();

if (mi == 2){
  miout = 1;
} else if (mi == 0 || mi == 1 || mi == 3) {
  miout = 0;
} else {
  miout = -1;
}

//stroke
stroke = $('PA194').value();

if (stroke == 1){
  stro = 1;
} else if (stroke == 0 || stroke == 2) {
  stro = 0;
} else {
  stro = -1;
}

/////////////////////////////////////////////////
/////////////////////////////////////////////////
if (hype == -1 && stro == -1 && miout == -1 ) {
  output = -1;
} else if (hype == 1 || stro == 1 || miout == 1) {
  output = 1;
} else{
  output = 0;
}

output;


// MEAT
reda = $('PA221').value();
redb = $('PA223').value();

if (reda == null && redb == null ) {
  sum_red = -1;
} else {
  sum_red = reda+redb;
}

proca = $('PA224').value();
procb = $('PA225').value();

if (proca == null && procb == null ) {
  sum_proc = -1;
} else {
  sum_proc = proca+procb;
}