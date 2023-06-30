/*=========================================================================
DO FILE NAME:		    04cr_copd_comorbidities.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	03/2022

DATASETS CREATED:       copd_Observation_`comorbidities'.dta
						
DESCRIPTION OF FILE:	Create patid + eventdate lists for each comorbidity. Uses patien file with inclusion/exclusion criteria applied
For comorbidities where first event ever of interest: generates final list of patients. For other comorbidities: generates list of all observations

*=========================================================================*/
clear all
capture log close 
log using $Logdir/04cr_copd_comorbidities.log, replace

cd "$Datadir_copd\extracted"

glob file_stub 			= 	"copd"
glob file_Patient 		= 	"${file_stub}_Extract_Patient_"
glob file_Practice 		=	"${file_stub}_Extract_Practice_"
glob file_Staff			=	"${file_stub}_Extract_Staff_"
glob file_Consultation 	= 	"${file_stub}_Extract_Consultation_"
glob file_Observation	= 	"${file_stub}_Extract_Observation_"
glob file_Referral 		= 	"${file_stub}_Extract_Referral_"
glob file_Problem 		= 	"${file_stub}_Extract_Problem_"
glob file_DrugIssue		= 	"${file_stub}_Extract_DrugIssue_"

// Specify number of different files
glob no_Patient = 1
glob no_Practice = 1
glob no_Staff = 1
glob no_Consultation2019 = 2
glob no_Observation2019 = 6
glob no_Referral2019 = 1
glob no_Problem2019 = 1
glob no_DrugIssue2019 = 7

// Specify number of different files
glob no_Consultation = 12
glob no_Observation = 48
glob no_Referral = 1
glob no_Problem = 1
glob no_DrugIssue = 49

/***************************************************************************************
COMORBIDITIES THAT USE FIRST EVENT EVER
*****************************************************************************************/

local disease diabetes allcancers hypertension ckd cvd bmt 
disp `"`comorbidity'"'

foreach comorbidity of local disease {
foreach file of numlist 1/$no_Observation {
	noi di "Merging `comorbidity' Observations, File `file'"
    use "${file_stub}_Extract_Observation_`file'", clear
	drop if eventdate > td(30apr2021)
    merge m:1 medcodeid using "$Codelistsdir\Comorbidities\cl_`comorbidity'.dta", keep(match) nogen
	merge m:1 patid using "${file_stub}_Patient_included.dta", keep(match) nogen
	keep patid medcodeid term obsdate enterdate eventdate
	if `file' == 1{
		save "${file_stub}_Observation_`comorbidity'.dta", replace
	}
	if `file' > 1{
		append using "${file_stub}_Observation_`comorbidity'.dta"
	}
	compress
	save "${file_stub}_Observation_`comorbidity'.dta", replace
	}
	

sort patid eventdate
drop if eventdate > td(01mar2020)
bysort patid (eventdate): gen keep = _n
keep if keep == 1
drop enterdate obsdate medcodeid term keep
rename eventdate `comorbidity'_date 
save "${file_stub}_covariate_`comorbidity'.dta", replace
}

clear all

/*********************************************************************************************************************
ASTHMA - ONLY CODES MORE THAN 3 YEARS BEFORE INDEX DATE ARE OF INTEREST. 
*************************************************************************************************************************/
clear all

local comorbidity asthma
disp `"`comorbidity'"'


use "${file_stub}_Observation_`comorbidity'.dta"
merge m:1 patid using "${file_stub}_Patient_included.dta", keep(match) nogen // this list only includes people who had no asthma code within 3 years before the index date
	
assert eventdate < td(01mar2017) | eventdate >= td(01mar2020)
sort patid eventdate
bysort patid: gen keep = _n
keep if keep == 1
drop enterdate obsdate medcodeid term keep
rename eventdate `comorbidity'_date 
save "${file_stub}_covariate_`comorbidity'.dta", replace

/**************************************************************************
Immunosuppression codes - need algorithm
***************************************************************************/
clear all

local disease bmt hiv othercmi permcmi aplastic_anaemia organ_transplant spleen haematological_cancer
disp `"`comorbidity'"'

foreach comorbidity of local disease {
foreach file of numlist 1/$no_Observation {
	noi di "Merging `comorbidity' Observations, File `file'"
    use "${file_stub}_Extract_Observation_`file'", clear
	drop if eventdate > td(30apr2021)
    merge m:1 medcodeid using "$Codelistsdir\Comorbidities\cl_`comorbidity'.dta", keep(match) nogen
	merge m:1 patid using "${file_stub}_Patient_included.dta", keep(match) nogen
	keep patid medcodeid term obsdate enterdate eventdate
	if `file' == 1{
		save "${file_stub}_Observation_`comorbidity'.dta", replace
	}
	if `file' > 1{
		append using "${file_stub}_Observation_`comorbidity'.dta"
	}
	compress
	save "${file_stub}_Observation_`comorbidity'.dta", replace
	}

}
	
clear all
log close

