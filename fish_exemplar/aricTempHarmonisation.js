dataframes_list = list(HOM10E,MSRA08F,GLUCOS01,CHMB07,HHXB05D,MSRB24F,LIPC4A,PHXA8K,MSRC24G,LIPD4A,PHXB6C,MSRD24G,
	FASTING_GLUCOSE,MSRF33C,V2AGE22,V1AGE01,V3AGE31,V4AGE41,V5AGE51,V2DAYS,V3DAYS,V4DAYS,V5DATE51_DAYS,DTIA35,DTIA36,DTIA37,
	DTIA34,GENDER,BMI01,ELEVEL02,ETHANL03,famdiab,MHXA28,HOM10C,HOM10D,HOM10F,HOM10A,TCAL,fruit,veg,DFIB,red,proc,
	bevs,HYPTMDCODE01,ANTA07A)

// agebase

age = $("V1AGE01").value();

// type_diab assume all type 2

// prevdiab

HOM10E = $('HOM10E').value();
MSRA08F = $('MSRA08F').value();
GLUCOS01 = $('GLUCOS01').value();

if (HOM10E == 3 || MSRA08F == 3 || GLUCOS01 >= 126) {
	prevalence = 1;
} else if (HOM10E == null || MSRA08F == null || GLUCOS01 == null) {
	prevalence = -1;
} else if  (HOM10E == 2 || MSRA08F == 2 || GLUCOS01 == 2) {
	prevalence = -1;
} else {
	prevalence = 0;
}

// case obj

CHMB07 = $('CHMB07').value();
HHXB05D = $('HHXB05D').value();
MSRB24F = $('MSRB24F').value();

LIPC4A = $('LIPC4A').value();
PHXA8K = $('PHXA8K').value();
MSRC24G = $('MSRC24G').value();

LIPD4A = $('LIPD4A').value();
PHXB6C = $('PHXB6C').value();
MSRD24G = $('MSRD24G').value();

FASTING_GLUCOSE = $('FASTING_GLUCOSE').value();
MSRF33C = $('MSRF33C').value();

if (CHMB07 >= 126 && HHXB05D == 3 || MSRB24F == 3) {
	diab = 1;
} else if (LIPC4A >= 126 && PHXA8K == 3 || MSRC24G == 3) {
	diab = 1;
} else if (LIPD4A >= 126 && PHXB6C == 3 || MSRD24G == 3) {
	diab = 1;
} else if (FASTING_GLUCOSE >= 126  && MSRF33C == 3) {
	diab = 1;
} else {
	diab = 0;
}

// case obj self
CHMB07 = $('CHMB07').value();
HHXB05D = $('HHXB05D').value();
MSRB24F = $('MSRB24F').value();

LIPC4A = $('LIPC4A').value();
PHXA8K = $('PHXA8K').value();
MSRC24G = $('MSRC24G').value();

LIPD4A = $('LIPD4A').value();
PHXB6C = $('PHXB6C').value();
MSRD24G = $('MSRD24G').value();

FASTING_GLUCOSE = $('FASTING_GLUCOSE').value();
MSRF33C = $('MSRF33C').value();

if (CHMB07 >= 126 || HHXB05D == 3 || MSRB24F == 3) {
	diab = 1;
} else if (LIPC4A >= 126 || PHXA8K == 3 || MSRC24G == 3) {
	diab = 1;
} else if (LIPD4A >= 126 || PHXB6C == 3 || MSRD24G == 3) {
	diab = 1;
} else if (FASTING_GLUCOSE >= 126  || MSRF33C == 3) {
	diab = 1;
} else {
	diab = 0;
}


// FUP obj and fup self

CHMB07 = $('CHMB07').value();
HHXB05D = $('HHXB05D').value();
MSRB24F = $('MSRB24F').value();

LIPC4A = $('LIPC4A').value();
PHXA8K = $('PHXA8K').value();
MSRC24G = $('MSRC24G').value();

LIPD4A = $('LIPD4A').value();
PHXB6C = $('PHXB6C').value();
MSRD24G = $('MSRD24G').value();

FASTING_GLUCOSE = $('FASTING_GLUCOSE').value();
MSRF33C = $('MSRF33C').value();

V2DAYS = $('V2DAYS').value();
V3DAYS = $('V3DAYS').value();
V4DAYS = $('V4DAYS').value();
V5DATE51_DAYS = $('V5DATE51_DAYS').value();

if (CHMB07 >= 126 || HHXB05D == 3 || MSRB24F == 3) {
	diab = 1;
	fup = (V2DAYS/2)/365;
} else if (LIPC4A >= 126 || PHXA8K == 3 || MSRC24G == 3) {
	diab = 1;
	fup = (V2DAYS + (V3DAYS - V2DAYS))/2/365;
} else if (LIPD4A >= 126 || PHXB6C == 3 || MSRD24G == 3) {
	diab = 1;
	fup = (V3DAYS + (V4DAYS - V3DAYS))/2/365
} else if (FASTING_GLUCOSE >= 126  || MSRF33C == 3) {
	diab = 1;
	fup = (V4DAYS + (V5DATE51_DAYS - V4DAYS))/2/365;
} else {
	diab = 0;
	a = V2DAYS
	b = V3DAYS
	c = V4DAYS
	d = V5DATE51_DAYS
	// by availability of value
	if (a == null && b == null && c == null && d == null){
		fup = -1;
	}
	if (a) {
		fup = a / 365;
	} 
	if (b) {
		fup = b / 365;
	} 
	if (c) {
		fup = c / 365;
	} 
	if (d) {
		fup = d / 365;
	} 
}

fup;


// fatty
fatty = $('DTIA35').value();
if (fatty == 1){
	val = 115*7;
} else if (fatty == 2) {
	val = 115*5;
} else if (fatty == 3) {
	val = 115*2.5;
} else if (fatty == 4) {
	val = 115;
} else if (fatty == 5) {
	val = (115*5.5)/7;
} else if (fatty == 6) {
	val = (115*3)/7;
} else if (fatty == 7) {
	val = 115/7;
} else if (fatty == 8) {
	val = (115*2)/30;
} else if (fatty == 9) {
	val = (115*6)/365;
} else {
	val = -1;
}

// fresh
val = 9999;

// fried
val = 9999;

// lean
lean = $('DTIA36').value();
if (lean == 1){
	val = 115*7;
} else if (lean == 2) {
	val = 115*5;
} else if (lean == 3) {
	val = 115*2.5;
} else if (lean == 4) {
	val = 115;
} else if (lean == 5) {
	val = (115*5.5)/7;
} else if (lean == 6) {
	val = (115*3)/7;
} else if (lean == 7) {
	val = 115/7;
} else if (lean == 8) {
	val = (115*2)/30;
} else if (lean == 9) {
	val = (115*6)/365;
} else {
	val = -1;
}

// nonfish
nonfish = $('DTIA37').value();
if (nonfish == 1){
	val = 115*7;
} else if (nonfish == 2) {
	val = 115*5;
} else if (nonfish == 3) {
	val = 115*2.5;
} else if (nonfish == 4) {
	val = 115;
} else if (nonfish == 5) {
	val = (115*5.5)/7;
} else if (nonfish == 6) {
	val = (115*3)/7;
} else if (nonfish == 7) {
	val = 115/7;
} else if (nonfish == 8) {
	val = (115*2)/30;
} else if (nonfish == 9) {
	val = (115*6)/365;
} else {
	val = -1;
}

// salt


// ssd


// total
canned = $('DTIA34').value();
if (canned == 1){
	val_canned = 100*7;
} else if (canned == 2) {
	val_canned = 100*5;
} else if (canned == 3) {
	val_canned = 100*2.5;
} else if (canned == 4) {
	val_canned = 100;
} else if (canned == 5) {
	val_canned = (100*5.5)/7;
} else if (canned == 6) {
	val_canned = (100*3)/7;
} else if (canned == 7) {
	val_canned = 100/7;
} else if (canned == 8) {
	val_canned = (100*2)/30;
} else if (canned == 9) {
	val_canned = (100*6)/365;
} else {
	val_canned = null;
}

fatty = $('DTIA35').value();
if (fatty == 1){
	val_fatty = 115*7;
} else if (fatty == 2) {
	val_fatty = 115*5;
} else if (fatty == 3) {
	val_fatty = 115*2.5;
} else if (fatty == 4) {
	val_fatty = 115;
} else if (fatty == 5) {
	val_fatty = (115*5.5)/7;
} else if (fatty == 6) {
	val_fatty = (115*3)/7;
} else if (fatty == 7) {
	val_fatty = 115/7;
} else if (fatty == 8) {
	val_fatty = (115*2)/30;
} else if (fatty == 9) {
	val_fatty = (115*6)/365;
} else {
	val_fatty = null;
}

lean = $('DTIA36').value();
if (lean == 1){
	val_lean = 115*7;
} else if (lean == 2) {
	val_lean = 115*5;
} else if (lean == 3) {
	val_lean = 115*2.5;
} else if (lean == 4) {
	val_lean = 115;
} else if (lean == 5) {
	val_lean = (115*5.5)/7;
} else if (lean == 6) {
	val_lean = (115*3)/7;
} else if (lean == 7) {
	val_lean = 115/7;
} else if (lean == 8) {
	val_lean = (115*2)/30;
} else if (lean == 9) {
	val_lean = (115*6)/365;
} else {
	val_lean = null;
}

nonfish = $('DTIA37').value();
if (nonfish == 1){
	val_nonfish = 115*7;
} else if (nonfish == 2) {
	val_nonfish = 115*5;
} else if (nonfish == 3) {
	val_nonfish = 115*2.5;
} else if (nonfish == 4) {
	val_nonfish = 115;
} else if (nonfish == 5) {
	val_nonfish = (115*5.5)/7;
} else if (nonfish == 6) {
	val_nonfish = (115*3)/7;
} else if (nonfish == 7) {
	val_nonfish = 115/7;
} else if (nonfish == 8) {
	val_nonfish = (115*2)/30;
} else if (nonfish == 9) {
	val_nonfish = (115*6)/365;
} else {
	val_nonfish = null;
}

if (val_canned == null && val_fatty == null && val_lean == null && val_nonfish == null){
	sum = -1;
} else {
	sum = val_canned+val_fatty+val_lean+val_nonfish;
}


// sex
sex = $('GENDER').value();

// bmi
bmi = $('BMI01').value();

// bmi_cat
bmi = $('BMI01').value();

if (bmi >= 25) {
	out = 1;
} else if (bmi < 25 && bmi > 0) {
	out = 0;
} else {
	out = -1;
}

// EDUCATION
edu = $('ELEVEL02').value();

// SMOKING
smoking = $('CIGT01').value();

// PA
pa = $('SPRT_I02').value();

// ALCOHOL
alc = $('ETHANL03').value();

// FAM_DIAB
mom = $('MOMHISTORYDIA').value();
dad = $('DADHISTORYDIA').value();

if (mom == null && dad == null){
	out = -1;
} else if (mom == 1 || dad == 1) {
	out = 1;
} else {
	out = 0;
}

// MI
m = $('MHXA28').value();
h = $('HOM10C').value();

if (m == null && h == null) {
	out = -1;
} else if (m == 2 && h == 2) {
	out = -1;
} else if (m == 3 || h == 3) {
	out = 1;
} else {
	out = 0;
}

// STROKE
h = $('HOM10D').value();

if (h == null || h == 2){
	out = -1;
} else if (h == 3) {
	out = 1;
} else {
	out = 0;
}

// CANCER
h = $('HOM10F').value();

if (h == null || h == 2){
	out = -1;
} else if (h == 3) {
	out = 1;
} else {
	out = 0;
}
// HYPERTENSION
h = $('HOM10A').value();

if (h == null || h == 2){
	out = -1;
} else if (h == 3) {
	out = 1;
} else {
	out = 0;
}
// E_INTAKE
cal = $('TCAL').value();

// COV_FRUIT
oranges = $('DTIA36').value();
if (oranges == 1){
	val_oranges = 80*7;
} else if (oranges == 2) {
	val_oranges = 80*5;
} else if (oranges == 3) {
	val_oranges = 80*2.5;
} else if (oranges == 4) {
	val_oranges = 80;
} else if (oranges == 5) {
	val_oranges = (80*5.5)/7;
} else if (oranges == 6) {
	val_oranges = (80*3)/7;
} else if (oranges == 7) {
	val_oranges = 80/7;
} else if (oranges == 8) {
	val_oranges = (80*2)/30;
} else if (oranges == 9) {
	val_oranges = (80*6)/365;
} else {
	val_oranges = null;
}


apples = $('DTIA36').value();
if (apples == 1){
	val_apples = 80*7;
} else if (apples == 2) {
	val_apples = 80*5;
} else if (apples == 3) {
	val_apples = 80*2.5;
} else if (apples == 4) {
	val_apples = 80;
} else if (apples == 5) {
	val_apples = (80*5.5)/7;
} else if (apples == 6) {
	val_apples = (80*3)/7;
} else if (apples == 7) {
	val_apples = 80/7;
} else if (apples == 8) {
	val_apples = (80*2)/30;
} else if (apples == 9) {
	val_apples = (80*6)/365;
} else {
	val_apples = null;
}



banana = $('DTIA36').value();
if (banana == 1){
	val_banana = 80*7;
} else if (banana == 2) {
	val_banana = 80*5;
} else if (banana == 3) {
	val_banana = 80*2.5;
} else if (banana == 4) {
	val_banana = 80;
} else if (banana == 5) {
	val_banana = (80*5.5)/7;
} else if (banana == 6) {
	val_banana = (80*3)/7;
} else if (banana == 7) {
	val_banana = 80/7;
} else if (banana == 8) {
	val_banana = (80*2)/30;
} else if (banana == 9) {
	val_banana = (80*6)/365;
} else {
	val_banana = null;
}


other = $('DTIA36').value();
if (other == 1){
	val_other = 80*7;
} else if (other == 2) {
	val_other = 80*5;
} else if (other == 3) {
	val_other = 80*2.5;
} else if (other == 4) {
	val_other = 80;
} else if (other == 5) {
	val_other = (80*5.5)/7;
} else if (other == 6) {
	val_other = (80*3)/7;
} else if (other == 7) {
	val_other = 80/7;
} else if (other == 8) {
	val_other = (80*2)/30;
} else if (other == 9) {
	val_other = (80*6)/365;
} else {
	val_other = null;
}

if (val_oranges == null && val_apples == null && val_banana == null && val_other == null){
	sum = -1;
} else {
	sum = val_oranges+val_apples+val_banana+val_other;
}

// COV_VEG

beans = $('DTIA15').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

brocolli = $('DTIA16').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);


cabbage = $('DTIA17').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);


carrot = $('DTIA18').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

corn = $('DTIA19').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

spinach = $('DTIA20').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

peas = $('DTIA21').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

squash = $('DTIA22').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

sweetpotato = $('DTIA23').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

bakedbeans = $('DTIA24').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

tomatoes = $('DTIA25').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

beans = parseFloat(beans)
brocolli = parseFloat(brocolli)
cabbage = parseFloat(cabbage)
carrot = parseFloat(carrot)
corn = parseFloat(corn)
spinach = parseFloat(spinach)
peas = parseFloat(peas)
squash = parseFloat(squash)
sweetpotato = parseFloat(sweetpotato)
bakedbeans = parseFloat(bakedbeans)
tomatoes = parseFloat(tomatoes)

if (beans == null && brocolli == null && 
	cabbage == null && carrot == null && 
	corn == null && spinach == null && 
	peas == null && squash == null && sweetpotato == null && 
	bakedbeans == null && tomatoes == null){
		sum = -1;
} else {
	sum = beans+brocolli+cabbage+carrot+corn+
	spinach+peas+squash+sweetpotato+bakedbeans+tomatoes;
}

// COV_FIBER
fib = $('DFIB').value();

// COV_RED_MEAT
redSandwich = $('DTIA32').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

redMain = $('DTIA33').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

redSandwich = parseFloat(redSandwich)
redMain = parseFloat(redMain)

if (redSandwich == null && redMain == null){
		sum = -1;
} else {
	sum = redSandwich+redMain;
}


// COV_PROC_MEAT
hamburger = $('DTIA28').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

hotdog = $('DTIA29').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

procmeat = $('DTIA30').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

bacon = $('DTIA31').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

hamburger = parseFloat(hamburger)
hotdog = parseFloat(hotdog)
procmeat = parseFloat(procmeat)
bacon = parseFloat(bacon)

if (hamburger == null && hotdog == null && procmeat == null && bacon == null){
		sum = -1;
} else {
	sum = hamburger+hotdog+procmeat+bacon;
}


// COV_SUG_BEVS

softdrinks = $('DTIA64').map({
  '1': 1*7, 
  '2': 1*5,
  '3': 1*2.5,
  '4': 1,
  '5': (1*5.5)/7,
  '6': (1*3)/7,
  '7': 1/7,
  '8': (1*2)/30,
  '9': (1*6)/365
  },$('DTIA15'),null);

fruitydrink = $('DTIA65').map({
  '1': 1*7, 
  '2': 1*5,
  '3': 1*2.5,
  '4': 1,
  '5': (1*5.5)/7,
  '6': (1*3)/7,
  '7': 1/7,
  '8': (1*2)/30,
  '9': (1*6)/365
  },$('DTIA15'),null);

softdrinks = parseFloat(softdrinks)
fruitydrink = parseFloat(fruitydrink)

if (softdrinks == null && fruitydrink == null){
		sum = -1;
} else {
	sum = softdrinks+fruitydrink;
}

// MEDS
meds = $('HYPTMDCODE01').map({
	'1':0,
	'2':1
}, $('HYPTMDCODE01'),-1);

// WAIST
waist = $('ANTA07A').value();

// SUPPLEMENTS

// COMORBID
//hypertension
h = $('HOM10A').value();
if (h == null || h == 2){
  hype = -1;
} else if (h == 3) {
  hype = 1;
} else {
	hype = 0;
}

//cancer
cancer = $('HOM10F').value();

if (cancer == null || cancer == 2){
  kanker = -1;
} else if (cancer == 3) {
  kanker = 1;
} else {
	kanker = 0;
}

//mi
m = $('MHXA28').value();
r = $('HOM10C').value();

if (m == null && r == null) {
  miout = -1;
} else if (m == 2 && r == 2) {
  miout = -1;
} else if (m == 3 || r == 3) {
	miout = 1;
} else {
	miout = 0;
}

//stroke
stroke = $('HOM10D').value();
if (stroke == null || stroke == 2){
  stro = -1;
} else if (h == 3) {
  stro = 1;
} else {
	stro = 0;
}

/////////////////////////////////////////////////
/////////////////////////////////////////////////
if (hype == -1 && stro==-1 && kanker == -1 && miout == -1 ) {
  output = -1;
} else if (hype == 1 || kanker == 1 || miout == 1 || stro == 1) {
  output = 1;
} else {
  output = 0;
}

output;

//meat
redSandwich = $('DTIA32').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

redMain = $('DTIA33').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

redSandwich = parseFloat(redSandwich)
redMain = parseFloat(redMain)

if (redSandwich == null && redMain == null){
  redsum = -1;
} else {
	redsum = redSandwich+redMain;
}

hamburger = $('DTIA28').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

hotdog = $('DTIA29').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

procmeat = $('DTIA30').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

bacon = $('DTIA31').map({
  '1': 80*7, 
  '2': 80*5,
  '3': 80*2.5,
  '4': 80,
  '5': (80*5.5)/7,
  '6': (80*3)/7,
  '7': 80/7,
  '8': (80*2)/30,
  '9': (80*6)/365
  },$('DTIA15'),null);

hamburger = parseFloat(hamburger)
hotdog = parseFloat(hotdog)
procmeat = parseFloat(procmeat)
bacon = parseFloat(bacon)

if (hamburger == null && hotdog == null && procmeat == null && bacon == null){
  procsum = -1;
} else {
	procsum = hamburger+hotdog+procmeat+bacon;
}

if (procsum == -1 && redsum == -1){
  sum = -1;
} else if (procsum == -1 && redsum != -1){
  sum = redsum;
} else if (procsum != -1 && redsum == -1) {
  sum = procsum;
}else {
  sum = procsum+redsum;
}