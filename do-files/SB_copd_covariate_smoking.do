/*=========================================================================
DO FILE NAME:			SB_copd_covariate_smoking.do

AUTHOR:					Marleen Bokern, adpated from Emily Herrett

VERSION:				v1

DATE VERSION CREATED: 	11/2022

DATASETS CREATED:       
						
DESCRIPTION OF FILE:	smoking codelist applied

*=========================================================================*/

/*******************************************************************************
>> HOUSEKEEPING
*******************************************************************************/
clear all

capture log close
log using $Logdir/SB_copd_covariate_smoking.log, replace
cd "$Datadir_copd"

/*******************************************************************************
>> Use file containing smoking observations, created in 02cr_copd_inc_exc_crit.dta
*******************************************************************************/

use "${file_stub}_Observation_smoking.dta"

drop if eventdate > td(31mar2021)

cap drop _merge
duplicates drop patid eventdate smokstatus, force

***get most recent smoking status 
merge m:1 patid using "SB_${file_stub}_Patient_included_all.dta", keepusing(covid_hes_date) keep(match)
drop if eventdate > covid_hes_date

gsort patid -eventdate

***drop values that were ambiguous
drop if smokstatus == 9 | smokstatus == 12 
by patid: gen obs_no = _n

gsort patid -eventdate

keep if obs_no == 1

rename eventdate smokdate
keep patid smokdate smokstatus

compress

***included patients all had record of former smoking. Therefore all patients coded as non-smokers are ex-smokers
***NEED TO FIGURE OUT WHAT TO DO WITH AMBIGUOUS CATEGORIES
replace smokstatus = 2 if smokstatus == 0

compress
save "SB_${file_stub}_covariate_smoking.dta", replace


log close
clear all




