/*=========================================================================
DO FILE NAME:			copd_covariate_smoking.do

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
log using $Logdir/copd_covariate_smoking.log, replace
cd "$Datadir_copd\extracted"

foreach file of numlist 1/$no_Observation {
	noi di "Merging smoking observations, File `file'"
    use "${file_stub}_Extract_Observation_`file'", clear
	drop pracid
	merge m:1 patid using "${file_stub}_Patid_list_included.dta", keep(match)
    merge m:1 medcodeid using "$Codelistsdir/cl_smoking.dta", keep(match) nogen
	if `file' == 1 {
		save "${file_stub}_Observation_smoking.dta", replace
	}
	if `file' > 1 {
		append using "${file_stub}_Observation_smoking.dta"
	}
	compress
	save "${file_stub}_Observation_smoking.dta", replace
}

use "${file_stub}_Observation_smoking.dta"
drop if eventdate > td(30apr2021)
unique patid
cap drop _merge

***get most recent smoking status for wave 1 cohorts
drop if eventdate > td(01mar2020)
gsort patid -eventdate
duplicates drop patid eventdate smokstatus, force
***drop values that were ambiguous
drop if smokstatus == 9 | smokstatus == 12 
by patid: gen obs_no = _n

gsort patid -eventdate


***included patients all had record of former smoking. Therefore all patients coded as non-smokers are ex-smokers
***NEED TO FIGURE OUT WHAT TO DO WITH AMBIGUOUS CATEGORIES
replace smokstatus = 2 if smokstatus == 0
keep if obs_no == 1

rename eventdate smokdate
keep patid smokdate smokstatus
compress
save "${file_stub}_covariate_smoking.dta", replace
