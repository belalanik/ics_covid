/*=========================================================================
DO FILE NAME:		    copd_covariate_vaccines.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	03/2022

DATASETS CREATED:       copd_Observation_`comorbidities'.dta
						
DESCRIPTION OF FILE:	Create patid + eventdate lists for each comorbidity. Uses patient file with inclusion/exclusion criteria applied
For comorbidities where first event ever of interest: generates final list of patients. For other comorbidities: generates list of all observations

*=========================================================================*/
clear all
capture log close 
log using $Logdir/copd_covariate_vaccines.log, replace

cd "$Datadir_copd"

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

glob no_Consultation = 12
glob no_Observation = 48
glob no_Referral = 1
glob no_Problem = 1
glob no_DrugIssue = 49


/**********************************************************************
FLU VACCINE
***********************************************************************/
local vaccine flu_vaccine

use "${file_stub}_Observation_`vaccine'_medcodes.dta"
append using "${file_stub}_DrugIssue_`vaccine'_product.dta"

drop enterdate

drop if (eventdate < td(01mar2019) | eventdate > td(01mar2020)) & eventdate !=. 
drop if (issuedate < td(01mar2019) | issuedate > td(01mar2020)) & issuedate !=. 
replace fluvacprod = 0 if fluvacprod ==.

gen flu_vacc_date = eventdate 
replace flu_vacc_date = issuedate if flu_vacc_date ==.
assert flu_vacc_date !=.
format flu_vacc_date %td

keep patid flu_vacc_date fluvacprod

bysort patid (flu_vacc_date fluvacprod): gen ord = _n
keep if ord == 1

keep patid flu_vacc_date fluvacprod
compress
save "${file_stub}_covariate_`vaccine'.dta", replace

clear all

/************************************************************
PNEUMOCOCCAL VACCINE
*************************************************************/
local vaccine pneumo_vaccine

use "${file_stub}_Observation_`vaccine'_medcodes.dta"
append using "${file_stub}_DrugIssue_`vaccine'_product.dta"

drop enterdate 

drop if eventdate < td(01mar2015) | eventdate > td(01mar2020)
drop if issuedate < td(01mar2015) | eventdate > td(01mar2020)
gen pneumovacprod = 0 if medcodeid != ""
replace pneumovacprod = 1 if prodcodeid != ""
assert pneumovacprod !=.

gen pneumo_vacc_date = eventdate 
replace pneumo_vacc_date = issuedate if pneumo_vacc_date ==.
assert pneumo_vacc_date !=.
format pneumo_vacc_date %td

keep patid pneumo_vacc_date pneumovacprod

bysort patid (pneumo_vacc_date pneumovacprod): gen ord = _n
keep if ord == 1

keep patid pneumo_vacc_date pneumovacprod
compress
save "${file_stub}_covariate_`vaccine'.dta", replace

log close
clear all
