/*=========================================================================
DO FILE NAME:			11cr_copd_patient_denominators.do

AUTHOR:					Marleen Bokern
VERSION:				v1

DATE VERSION CREATED: 	12/2022
						
DATASETS CREATED:       patient_denominators_copd.dta
						
DESCRIPTION OF FILE:	Get denominators for each month and prescription counts for each month. Uses clean drug issue files for calculation
					
*=========================================================================*/
clear all
capture log close
log using "$Logdir/11cr_copd_patient_denominators.log", replace
ssc install ereplace

cd "$Datadir_copd"

return list
scalar list

putexcel set "denominators", replace
putexcel A1 = "month_str"
putexcel B1 = "month"
putexcel A2 = "Mar2019"
putexcel A3 = "Apr2019"
putexcel A4 = "May2019"
putexcel A5 = "Jun2019"
putexcel A6 = "Jul2019"
putexcel A7 = "Aug2019"
putexcel A8 = "Sep2019"
putexcel A9 = "Oct2019"
putexcel A10 = "Nov2019"
putexcel A11 = "Dec2019"
putexcel A12 = "Jan2020"
putexcel A13 = "Feb2020"
putexcel A14 = "Mar2020"
putexcel A15 = "Apr2020"
putexcel A16 = "May2020"
putexcel A17 = "Jun2020"
putexcel A18 = "Jul2020"
putexcel A19 = "Aug2020"
putexcel A20 = "Sep2020"
putexcel A21 = "Oct2020"
putexcel A22 = "Nov2020"
putexcel A23 = "Dec2020"
putexcel A24 = "Jan2021"
putexcel A25 = "Feb2021"
putexcel A26 = "Mar2021"
putexcel A27 = "Apr2021"

local start = 710
local end = 735

local row = 2
forval i = `start'/`end' {
    putexcel B`row' = "`i'"
    local row = `row' + 1
}

/************************************************************************
DENOMINATOR COUNTS
*************************************************************************/
use "${file_stub}_Patient_included.dta", replace 

merge m:1 patid using "${file_stub}_firstevent", keep(match)

g dm_copd = mofd(copd_date)
format dm_copd %tm

g dm_end = mofd(enddate)
format dm_end %tm

g first_month = dm_copd
replace first_month = first_month + 1  if day(copd_date) >= 15
format first_month %tm

putexcel C1 = "denom"
local loop_count = 1
forvalues dm = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if first_month <= (`dm') & dm_end >= (`dm')
	scalar denom_`dm' = r(unique)
	local cell = `loop_count' + 1
	putexcel C`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

clear all 

/************************************************************************
ICS SINGLE
*************************************************************************/
***ICS patients
clear all
use "${file_stub}_ics_single_clean", replace

putexcel D1 = "ics_single_pat"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel D`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

***individual ICS prescriptions
putexcel E1 = "ics_single_rx"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if dm == `m'
	scalar count_`m' = r(N)
	local cell = `loop_count' + 1 
	putexcel E`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }

/************************************************************************
ICS LABA
*************************************************************************/
****add ICS LABA PATIENTS
clear all
use "${file_stub}_ics_laba_clean", replace
     
putexcel F1 = "ics_laba_pat"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel F`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

***ICS LABA prescriptions
putexcel G1 = "ics_laba_rx"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if dm == `m'
	scalar count_`m' =r(N)
	local cell = `loop_count'+1
	putexcel G`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }
   
/************************************************************************
LABA LAMA
*************************************************************************/
***LABA LAMA patients
clear all
use "${file_stub}_laba_lama_clean", replace

putexcel H1 = "laba_lama_pat"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel H`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

***LABA LAMA prescriptions
putexcel I1 = "laba_lama_rx"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if dm == `m'
	scalar count_`m' = r(N)
	local cell = `loop_count' + 1
	putexcel I`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }
   
/************************************************************************
LABA single
*************************************************************************/
***LABA single patients
clear all
use "${file_stub}_laba_single_clean", replace

putexcel J1 = "laba_single_pat"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel J`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

***LABA single prescriptions
putexcel K1 = "laba_single_rx"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if dm == `m'
	scalar count_`m' = r(N)
	local cell = `loop_count'+ 1
	putexcel K`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }
   
/************************************************************************
LAMA single
*************************************************************************/
***LAMA single patients
clear all
use "${file_stub}_lama_single_clean", replace

putexcel L1 = "lama_single_pat"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel L`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

****add LAMA single prescriptions
putexcel M1 = "lama_single_rx"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if dm == `m'
	scalar count_`m' = r(N)
	local cell = `loop_count' + 1
	putexcel M`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }
      
/************************************************************************
TRIPLE THERAPY
*************************************************************************/
***TRIPLE THERAPY patients
clear all
use "${file_stub}_triple_therapy_clean", replace

putexcel N1 = "triple_therapy_pat"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel N`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

****add triple therapy prescriptions
putexcel O1 = "triple_therapy_rx"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if dm == `m'
	scalar count_`m' = r(N)
	local cell = `loop_count' + 1
	putexcel O`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }
   
/************************************************************************
***generate end/discontinuation dates in different ways
***ICS prevalence with 60d allowable gap
*************************************************************************/
clear all
use "${file_stub}_ics_episodes_60d"

****gen prevalence counts
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	gen count`m' =.
	replace count`m' = 1 if dm <= `m' & dm_disc >= `m'
	replace count`m' = 0 if count`m' ==.
	bysort patid: ereplace count`m' = max(count`m')
   } 

drop date1 date2 regstart dm rx enddate disc_date dm_disc
duplicates drop

****1 row per patient

putexcel P1 = "ics_prev_pat60d"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if count`m' == 1
	scalar count_`m' = r(N)
	local cell = `loop_count' + 1
	putexcel P`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }

save "${file_stub}_patient_prevalence_ics60d", replace
clear all

/************************************************************************
***generate end/discontinuation dates in different ways
***ICS prevalence with 6m post-last prescription gap
*************************************************************************/
use "${file_stub}_ics_episodes_6m" 

****gen prevalence counts
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	gen count`m' =.
	replace count`m'= 1 if mofd(date1) <= `m' & dm_disc >= `m'
	replace count`m' = 0 if count`m' ==.
	bysort patid: ereplace count`m' = max(count`m')
   }
   
putexcel Q1 = "ics_prev_pat6m"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	count if count`m'== 1
	scalar count`m' = r(N)
	local cell = `loop_count' + 1
	putexcel Q`cell' = `r(N)'
	local loop_count = `loop_count' + 1
   }   
   
save "${file_stub}_patient_prevalence_ics6m", replace

***get users of any ics   
clear all
use "copd_all_ics.dta", replace

putexcel R1 = "ics_all_pat"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel R`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }
   
**** get discontinuations = 6m post last prescription
drop substancestrength dosageid
merge m:1 patid using "${file_stub}_Patient.dta", keepusing(regstart enddate) keep(match) nogen
gen disc_date6m = issuedate + 183
format disc_date6m %td
replace disc_date6m =. if enddate < disc_date6m
bysort patid (issuedate): replace disc_date6m =. if issuedate[_n + 1] < disc_date6m
g dm_disc6m = mofd(disc_date6m)
format dm_disc6m %tm

putexcel S1 = "disc6m"
local loop_count = 1
forvalues m = `=tm(2019m3)'/`=tm(2021m4)' {
	di `loop_count'
	unique patid if dm_disc6m == `m'
	scalar count_`m' = r(unique)
	local cell = `loop_count' + 1
	putexcel S`cell' = `r(unique)'
	local loop_count = `loop_count' + 1
   }

*********************************************************************************************
*********************************************************************************************
***get initiations = 1y without prescriptions
use "${file_stub}_ics_episodes_initiation", replace

****assess initation over time 
tabulate dm if init == 1 & date1 >= td(01mar2019) & excl ==., matcell(freq) matrow(names)
matrix list freq
matrix list names

putexcel T1 = "init" T2 = matrix(freq) 
by patid (date1): assert date2 < date1[_n + 1]

clear all
***********************************************************************************************
***get discontinuations = 60 post end date
use "${file_stub}_ics_episodes_60d"

**export only numbers no months
tabulate dm_disc if dm_disc >= tm(2019m3) & dm_disc <= tm(2021m4) , matcell(freq) matrow(names)
matrix list freq
matrix list names

putexcel U1 = "disc60d" U2 = matrix(freq)

save "${file_stub}_ics_episodes_60d", replace
clear all 
**************************************************************************************************

import excel "denominators.xlsx", firstrow

destring month, replace
format month %tm
gen ics_rxprop = (ics_single_rx + ics_laba_rx + triple_therapy_rx) / denom 
gen ics_rx_per100 = (ics_single_rx + ics_laba_rx + triple_therapy_rx) / denom *100
gen ics_all_rx = (ics_single_rx + ics_laba_rx + triple_therapy_rx)
gen ics_pat_prop = ics_all_pat / denom
gen ics_prev_prop60d = ics_prev_pat60d / denom
gen ics_prev_prop6m = ics_prev_pat6m / denom
gen init_prop = init/denom
gen disc60d_prop = disc60d /denom
gen disc6m_prop = disc6m /denom

save "${file_stub}_summary_stats.dta", replace
export excel "${file_stub}_summary_stats.xlsx", firstrow(var) keepcellfmt replace
export delim "${file_stub}_summary_stats.csv", replace

clear all
log close