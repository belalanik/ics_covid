/*=========================================================================
DO FILE NAME:		    05cr_copd_outcome.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	03/2022

DATASETS CREATED:       copd_Observation_covid_positive.dta
						
DESCRIPTION OF FILE:	Create patid + eventdate lists for outcome positive COVID test. Uses patient file with inclusion/exclusion criteria applied

*=========================================================================*/

clear all
***run globals
capture log close 
log using $Logdir/05cr_copd_outcome.log, replace

cd "$Datadir_copd"

/***************************************************************************************
COMORBIDITIES THAT USE FIRST EVENT EVER
*****************************************************************************************/
/***************************************************************************************
POSITIVE COVID TEST
*****************************************************************************************/

local outcome pos_covid_test
disp `"`outcome'"'

foreach file of numlist 1/$no_Observation {
	noi di "Merging positive COVID test Observations, File `file'"
    use "$Copd_aurum_extract\\${file_stub}_Extract_Observation_`file'", clear
	drop if eventdate > td(31aug2020)
	drop if eventdate < td(01mar2020)
    merge m:1 medcodeid using "$Codelistsdir\Outcomes\cl_`outcome'.dta", keep(match) nogen
	merge m:1 patid using "${file_stub}_Patid_list_included_all.dta", keep(match) nogen
	keep patid medcodeid term obsdate enterdate eventdate
	if `file' == 1{
		save "${file_stub}_Observation_`outcome'.dta", replace
	}
	if `file' > 1{
		append using "${file_stub}_Observation_`outcome'.dta"
	}
	compress
	save "${file_stub}_Observation_`outcome'.dta", replace
	}
	

sort patid eventdate
bysort patid (eventdate): gen keep = _n
keep if keep == 1
drop enterdate obsdate medcodeid term keep
rename eventdate `outcome'_date 
save "${file_stub}_outcome_`outcome'_w1.dta", replace

log close 
clear all