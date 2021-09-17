cd "V:\Studies\InterConnect\Internal\Other data sharing mechanisms\BioLINCC data_ US data\MESA\Primary\stata"

local files mesae1dietdres06192012 mesae1dres06192012 MESAe2DRes06222012 mesae3dres06222012 mesae4dres06222012 mesae5_drepos_20151101 mesaevnoncvddres06192012

local nsets : word count `files'

clear

 forval i = 1/`nsets' {
	local f : word `i' of `files'
	use `f', clear
	sort mesaid
	save `f'_sort, replace
}

clear



*Optional deduping

*check dupes
*quietly by mesaid :  gen dup = cond(_N==1,0,_n)
*tabulate dup
/*
clear

use F60_ITEM_OS_PUB_sort
keep if f60vtyp == 1
save F60_ITEM_OS_PUB_sort, replace

clear

use F60_NUTR_OS_PUB_sort
keep if f60vtyp == 1
save F60_NUTR_OS_PUB_sort, replace

clear

use F80_OS_PUB_sort
keep if f80vtyp == 1
save F80_OS_PUB_sort, replace

clear

use F44_OS_PUB_sort
quietly by id :  gen dup = cond(_N==1,0,_n)
*tabulate dup
keep if f44vtyp == 1
destring tccode, replace
keep if tccode >= 270000
keep if tccode <= 279999
drop if dup>0
save F44_OS_PUB_sort, replace

*/



clear

use mesae1dres06192012_sort

keep mesaid age1c diabet1 dbhxtyp1  insln1c ohga1c diabins1 glucos1c  gender1 bmi1c educ1 cig1c leismt1c htnmed1c waistcm1 cancer1 highbp1

save mesae1dres06192012_sort_trim, replace

clear

use mesae2dres06222012_sort

keep mesaid diabins2 glucos2c ohga2c insln2c age2c

save mesae2dres06222012_sort_trim, replace

clear

use mesae3dres06222012_sort

keep mesaid glucos3c ohga3c insln3c diabins3 age3c

save mesae3dres06222012_sort_trim, replace

clear

use mesae4dres06222012_sort

keep mesaid glucos4c ohga4c insln4c diabins4 age4c

save mesae4dres06222012_sort_trim, replace

clear

use mesae5_drepos_20151101_sort

keep mesaid glucose5 ohga5c insln5c diabins5 age5c

save mesae5_drepos_20151101_sort_trim, replace

clear

use mesae1dietdres06192012_sort

/*keep mesaid enrgyn1c fgfruit1c fgvgreenleafy1c fgvcrucifer1c fgvdyellow1c fgvother1c fgavocado1c fgtomato1c tfibrn1c svdhamburger1c svdham1c fgsoda1c supdurcodoil1 svdsteak1c svdhocks1c svdtuna1c svdfriedfish1c svdboiledfish1c svdshrimp1c fgfish1c alcn1c*/

keep mesaid enrgyn1c fgfruit1c fgvgreenleafy1c fgvcrucifer1c fgvdyellow1c fgvother1c fgavocado1c fgtomato1c tfibrn1c svdhamburger1c svdham1c fgsoda1c supdurcodoil1 svdsteak1c svdhocks1c fglegumes1c frqbean1 svdbean1c frqpeasoup1 svdpeasoup1c frqgreenbean1 svdgreenbean1c frqfriedbeans1 svdfriedbeans1c fgsoy1c frqsoymilk1 svdsoymilk1c frqstirfrdtofu1 svdstirfrdtofu1c frqtofudessert1 svdtofudessert1c frqpeanuts1 svdpeanuts1c frqnuts1 svdnuts1c alcn1c

save mesae1dietdres06192012_sort_trim, replace

clear

use mesaevnoncvddres06192012_sort

keep mesaid diabtt

save mesaevnoncvddres06192012_sort_trim, replace

clear


local files mesae1dietdres06192012 mesae1dres06192012 MESAe2DRes06222012 mesae3dres06222012 mesae4dres06222012 mesae5_drepos_20151101 mesaevnoncvddres06192012

foreach l of local files {
	local files_sort "`files_sort' `l'_sort_trim"
	}

local nsets : word count `files'
local nsets_sort : word count `files_sort'
local first : word 1 of `files_sort'

local rest

 forval i = 2/`nsets' {
	   
      local f : word `i' of `files_sort'
	  local rest `rest' `f' 

 }


disp "`first'"
disp "`rest'"

use `first'

 forval i = 1/`nsets_sort' {
	local f : word `i' of `files_sort'
	merge 1:1 mesaid using `f'
	rename _merge _merge_`i'
	
}


save mesa_datamerged_data_leg, replace


