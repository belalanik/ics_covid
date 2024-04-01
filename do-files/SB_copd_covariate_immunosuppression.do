/*=========================================================================
DO FILE NAME:			copd_covariate_immunosuppression.do

AUTHOR:					Marleen Bokern, 

VERSION:				v1

DATE VERSION CREATED: 	03/2023

DATASETS CREATED:       copd_covariate_immunosuppression.dta
						
DESCRIPTION OF FILE:	immunosuppression algorithm for immunosuppression at baseline

*=========================================================================*/

clear all
capture log close 
log using $Logdir/SB_copd_covariate_immunosuppression.log, replace

cd "$Datadir_copd"

/********************************************************************************************
ORGAN TRANSPLANT - EVER BEFORE INDEX
**********************************************************************************************/
local concept organ_transplant
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate
qui count
if r(N) > 0 {
bysort patid (eventdate): gen first = _n
keep if first == 1

drop if eventdate > covid_hes_date
}

keep patid eventdate
save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
HIV - EVER BEFORE INDEX
**********************************************************************************************/
local concept hiv
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate

qui count
if r(N) > 0 {
bysort patid (eventdate): gen first = _n
keep if first == 1

drop if eventdate > covid_hes_date
}

keep patid eventdate
save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
PERMCMI - EVER BEFORE INDEX
**********************************************************************************************/
local concept permcmi
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate
qui count
if r(N) > 0 {
bysort patid (eventdate): gen first = _n
keep if first == 1

drop if eventdate > covid_hes_date
}

keep patid eventdate
save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
SPLEEN DYSFUNCTION - EVER BEFORE INDEX
**********************************************************************************************/
local concept spleen
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate

qui count
if r(N) > 0 {
bysort patid (eventdate): gen first = _n
keep if first == 1

drop if eventdate > covid_hes_date
}

keep patid eventdate
save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
HAEMATOLOGICAL CANCER - FIRST EVER CODE WITHIN PREVIOUS YEAR
**********************************************************************************************/
local concept haematological_cancer
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate

qui count
if r(N) > 0 {
bysort patid (eventdate): gen first = _n
keep if first == 1

drop if eventdate > covid_hes_date
drop if eventdate < (covid_hes_date - 365.25)
}

keep patid eventdate
save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
APLASTIC ANAEMIA - FIRST EVER CODE WITHIN PREVIOUS YEAR
**********************************************************************************************/
local concept aplastic_anaemia
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate

qui count
if r(N) > 0 {
bysort patid (eventdate): gen first = _n
keep if first == 1

drop if eventdate > covid_hes_date
drop if eventdate < (covid_hes_date - 365.25)
}

keep patid eventdate
save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
BMT - ANY CODE WITHIN PREVIOUS YEAR
**********************************************************************************************/
local concept bmt
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate

drop if eventdate > covid_hes_date
drop if eventdate < (covid_hes_date - 365.25)

qui count
	
if r(N) > 0 {
    bysort patid (eventdate): gen first = _n
    keep if first == 1
}

    keep patid eventdate
    save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
OTHER CMI -  ANY CODE WITHIN PREVIOUS 3 MONTHS
**********************************************************************************************/
local concept othercmi
use "${file_stub}_Observation_`concept'.dta"
merge m:1 patid using "$Datadir_copd\SB_covid_hes_date_w1.dta", keep(match) nogen
drop enterdate obsdate
	
drop if eventdate > covid_hes_date
drop if eventdate < (covid_hes_date - 91.25)

qui count
if r(N) > 0 {
bysort patid (eventdate): gen first = _n
keep if first == 1
}

keep patid eventdate
save "SB_${file_stub}_covariate_`concept'.dta", replace

clear all
/********************************************************************************************
LOOP THROUGH CONCEPTS AND APPEND DATASET
**********************************************************************************************/

local concept organ_transplant hiv permcmi spleen haematological_cancer aplastic_anaemia bmt othercmi
di`"`concept'"'
tokenize `"`concept'"'
local i = 1
foreach j of local concept {
    use "${file_stub}_covariate_`j'.dta", clear
	gen `j' = 9
	if `i' == 1{
		save "SB_${file_stub}_covariate_immunosuppression.dta", replace
	}
	if `i' != 1{
		append using "SB_${file_stub}_covariate_immunosuppression.dta"
	}
	compress
	save "SB_${file_stub}_covariate_immunosuppression.dta", replace
	local i = `i' + 1
}

collapse (max) organ_transplant hiv permcmi spleen haematological_cancer aplastic_anaemia bmt othercmi eventdate, by(patid)
keep patid eventdate
rename eventdate immunosuppression_date

save "SB_${file_stub}_covariate_immunosuppression_patid.dta", replace

log close 
clear all
