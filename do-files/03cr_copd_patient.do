/*=========================================================================
DO FILE NAME:			03cr_copd_patient.do

AUTHOR:					Marleen Bokern
VERSION:				v1

DATE VERSION CREATED: 	12/2022
						
DATASETS CREATED:       copd_Patient.dta
						copd_Patient_included.dta
						copd_exclusion_asthma.dta
						copd_DrugIssue_ltra.dta
						copd_Observation_smoking.dta
						copd_Patid_list_included.dta
						copd_Patient_smoking_inclusion.dta
						
DESCRIPTION OF FILE:	Check and manage patient file COPD, apply exclusion criteria to patient file, apply smoking inclusion criteria
					
*=========================================================================*/
/***********************************************************************************************
>> HOUSEKEEPING
************************************************************************************************/
clear all

cap log close
log using $Logdir\03cr_copd_patient.log, replace

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

// Specify number of different file
glob no_Patient = 1
glob no_Practice = 1
glob no_Staff = 1
glob no_Consultation = 12
glob no_Observation = 48
glob no_Referral = 1
glob no_Problem = 1
glob no_DrugIssue = 49

/*******************************************************************************
Import patient file
*******************************************************************************/
use "$Copd_aurum_extract\\${file_stub}_Extract_Patient_1.dta"
describe

count if emis_death !=. & missing(deathdate) //***0 --> no cases where emis has death date but cprd does not
drop emis_death patienttypeid

*Generate DOB. Uses 01 July if month and day are missing, or 15th of month if month is not missing
gen day = 15 if mob !=.
replace mob = 7 if mob ==.
replace day = 1 if day ==.
gen dob = mdy(mob, day, yob)
format dob %td
list mob yob dob in 1/10

*generate date of 35th birthday
gen yo35bday = yob + 35
gen do35bday = mdy(mob, day, yo35bday)
format do35bday %td
list yob mob dob do35bday in 1/10

*drop people under 35 on 01mar2020
count if do35bday > td(01mar2020)
drop if do35bday > td(01mar2020)

***get age and gender info
gen age_index = (td(01mar2020) - dob) / 365.25
tabstat age_index, stat(n mean sd median p25 p75 min max)

*check if anyone is born after deathdate
count if deathdate < dob
drop if deathdate < dob

*check if anyone dies before registration
count if deathdate < regstart
drop if deathdate < regstart 

***drop people with missing gender
tab gender 

**drop anyone who starts after 1st march 2019
count if regstart > td(01mar2019)
drop if regstart > td(01mar2019)

**merge with practice file to generate end dates
merge m:1 pracid using "$Copd_aurum_extract\\${file_stub}_Extract_Practice_1", nogen keep(match)
merge 1:1 patid using "$Datadir_copd\death_date_w1.dta", nogen keep(match master)

gen death = death_date_ons
replace death = deathdate if death ==.
format death %td
count if death < td(01mar2020)
drop if death < td(01mar2020)

***Generate end date (last collection or registration end or death)
gen enddate = min(regend,lcd,death)
lab var enddate "End Date"
format enddate %d

assert enddate >= td(01mar2020)

misstable summarize
summarize
compress
save "${file_stub}_Patient", replace 

clear all 
/*******************************************************************************
>> APPLY CODELISTS FOR EXCLUSION AND INCLUSION CRITERIA 
*******************************************************************************/
/********************************************************
COPD 
********************************************************/
local concept copd 

use "${file_stub}_Observation_`concept'.dta", clear
capture drop _merge
merge m:1 patid using "${file_stub}_Patient.dta", keep(match)
keep if _merge == 3
drop _merge

***remove implausible observation dates
replace eventdate  =. if year(eventdate) < yob
replace eventdate  =. if eventdate < td(01jan1910)

***keep only diagnoses before index date
drop if eventdate > td(01mar2020) 
drop if eventdate < dob
drop if missing(eventdate)
rename eventdate `concept'_date

***keep only first events
sort patid `concept'_date
by patid : gen keep = _n

drop if keep != 1 
duplicates report patid //*** duplicates should be 0

count if `concept'_date < regstart

keep patid `concept'_date
compress
save "copd_firstevent", replace

clear all
/*******************************************************************************
>> ASTHMA 
*******************************************************************************/
local concept asthma

use "${file_stub}_Observation_`concept'.dta"

merge m:1 patid using "$Datadir_copd\\${file_stub}_Patient", keep(match)

***flag diagnoses within past 3 years
gen excl = 1 if eventdate >= td(01mar2017) & eventdate <= td(01mar2020)
sort patid eventdate
by patid: ereplace excl = max(excl)

***exclude people with a recent code
keep if excl == 1 
sort patid eventdate
by patid : gen first = _n
keep if first == 1
drop first

rename eventdate `concept'_date
keep patid `concept'_date 

compress
save "${file_stub}_exclusion_`concept'", replace

clear all
/*******************************************************************************
>> LTRA 
*******************************************************************************/
local concept ltra

use "${file_stub}_DrugIssue_`concept'.dta"

merge m:1 patid using "$Datadir_copd\\${file_stub}_Patient", keep(match)
drop if issuedate > td(01mar2020)

***flag people with a prescription within 4 months before index date
gen excl = 1 if issuedate >= td(01nov2019) & issuedate <= td(01mar2020)
sort patid issuedate
by patid: ereplace excl = max(excl)

***exclude people with a recent code
keep if excl == 1 
sort patid issuedate
by patid : gen first = _n
keep if first == 1
drop first

rename issuedate `concept'_date

keep patid `concept'_date 
compress
save "${file_stub}_exclusion_`concept'", replace

clear all
/*******************************************************************************
>> Nebuliser 
*******************************************************************************/
local concept nebuliser 

use "${file_stub}_DrugIssue_`concept'.dta"

merge m:1 patid using "$Datadir_copd\\${file_stub}_Patient", keep(match)
drop if issuedate > td(01mar2020)

***flag people with a code in the last 4 months
gen excl = 1 if issuedate > td(01mar2019) & issuedate <= td(01mar2020)
sort patid issuedate
by patid: ereplace excl = max(excl)

***exclude people with a recent code
keep if excl == 1 
sort patid issuedate
by patid : gen first = _n
keep if first == 1
drop first

rename issuedate `concept'_date

keep patid `concept'_date 
compress
save "${file_stub}_exclusion_`concept'", replace

clear all
/*******************************************************************************
>> SMOKING
*******************************************************************************/
local concept smoking

use "${file_stub}_Observation_`concept'.dta"
capture drop _merge
drop if eventdate > td(01mar2020)
rename eventdate `concept'_date

***include only people with past or current smoking
drop if smokstatus == 0 //***smok status is a byte variable, not string
drop if missing(smokstatus)
merge m:1 patid using "${file_stub}_Patient.dta", keep(match)
sort patid `concept'_date
bysort patid (`concept'_date): gen keep = _n
drop if keep != 1 

duplicates drop

keep patid `concept'_date smokstatus
compress
save "${file_stub}_inclusion_`concept'.dta", replace

clear all
/*******************************************************************************
>> TRIPLE THERAPY
*******************************************************************************/
local concept triple_therapy

use "${file_stub}_DrugIssue_`concept'.dta"

merge m:1 patid using "$Datadir_copd\\${file_stub}_Patient", keep(match)

***exclude people with triple therapy prescription within past 3 months
drop if issuedate < td(01dec2019)
drop if issuedate > td(01mar2020)
rename issuedate `concept'_date

bysort patid (`concept'_date): gen first = _n
keep if first == 1

keep patid `concept'_date 
compress
save "$Datadir_copd\\${file_stub}_exclusion_`concept'", replace

clear all
/*******************************************************************************
>> OTHER RESPIRATORY DISEASE
*******************************************************************************/
local concept other_resp_disease

use "${file_stub}_Observation_`concept'.dta"
capture drop _merge
drop if eventdate > td(01mar2020)
rename eventdate `concept'_date

merge m:1 patid using "${file_stub}_Patient.dta", keep(match)
sort patid `concept'_date

*exclude people with other respiratory disease ever
bysort patid (`concept'_date): gen keep = _n
drop if keep != 1 

duplicates drop

keep patid `concept'_date 
compress
save "${file_stub}_exclusion_`concept'.dta", replace

clear all

/*******************************************************************************
>> COMBINE ASTHMA/LTRA EXCLUSION AND SMOKING INCLUSION CRITERIA TO PATIENT LIST
*******************************************************************************/
clear all 
use "${file_stub}_exclusion_ltra.dta"

append using "${file_stub}_exclusion_asthma"
append using "${file_stub}_exclusion_other_resp_disease.dta"
append using "${file_stub}_exclusion_nebuliser.dta"
unique patid

***combine eventdates and issuedates into one variable
gen ltra = 1 if ltra_date !=.
gen asthma = 1 if asthma_date !=.
gen other_resp_disease = 1 if other_resp_disease_date !=. 

collapse (max) ltra asthma other_resp_disease, by(patid)

****save list of patients to exclude and merge in list of all patients
cap drop _merge
merge 1:1 patid using "${file_stub}_Patient"

****drop those patients who are included in the asthma/ltra list
keep if _merge == 2
drop _merge

****exclude those without a smoking record
merge 1:1 patid using "${file_stub}_inclusion_smoking.dta"
keep if _merge == 3
drop _merge

****exclude people without copd before 01mar 2020
merge 1:1 patid using "copd_firstevent"
keep if _merge == 3
drop if copd_date > td(01mar2020)
drop _merge

compress
save "${file_stub}_Patient_included.dta", replace

hist age_index
tabstat age_index, s(p50 p25 p75)

unique patid
keep patid

compress
save "${file_stub}_Patid_list_included.dta", replace
clear all

/*******************************************************************************
>> Get list of patients for sensitivity analysis including people with asthma, other respiratory disease, or LTRA use
*******************************************************************************/

use "${file_stub}_Patient"
****exclude people without copd before 01mar 2020
merge 1:1 patid using "copd_firstevent"
keep if _merge == 3
drop if copd_date > td(01mar2020)
***drop all new variables
drop emis_ddate cprd_ddate pracid 
drop _merge

****exclude people without copd before 01mar 2020
merge 1:1 patid using "copd_firstevent"
keep if _merge == 3
drop if copd_date > td(01mar2020)
drop _merge

****exclude those without a smoking record
merge 1:1 patid using "${file_stub}_inclusion_smoking.dta"
keep if _merge == 3
drop _merge

save "${file_stub}_Patient_included_all.dta", replace
keep patid
save "${file_stub}_Patid_list_included_all.dta", replace
clear all

/*******************************************************************************
>> GENERATE FILE OF PATIENTS FOR LINKAGE
*******************************************************************************/
/*
import delim "$Denominator\CPRD Linkage Source Data Files\Version22\set_22_Source_Aurum\Aurum_enhanced_eligibility_January_2022.txt", stringcols(_all)
merge 1:1 patid using "copd_Patid_list_included.dta", keep(match using)
drop pracid sgss_e chess_e hes_op_e hes_ae_e hes_did_e cr_e sact_e mhds_e icnarc_e rtds_e _merge linkdate
destring lsoa_e hes_apc_e ons_death_e, replace
drop if hes_apc_e == 0 & ons_death_e == 0 & lsoa_e == 0

export delim "copd_Patid_for_linkage.txt", replace


clear all

import delim "$Denominator\CPRD Linkage Source Data Files\Version22\set_22_Source_Aurum\Aurum_enhanced_eligibility_January_2022.txt", stringcols(_all)
merge 1:1 patid using "$Datadir_copd\copd_Patient", keep(match using)
drop pracid sgss_e chess_e hes_op_e hes_ae_e hes_did_e cr_e sact_e mhds_e icnarc_e rtds_e _merge linkdate
destring lsoa_e hes_apc_e ons_death_e, replace
drop if hes_apc_e == 0 & ons_death_e == 0 & lsoa_e == 0

keep patid hes_apc_e ons_death_e lsoa_e

export delim "copd_Patid_linkage_all.txt", replace
save "$Datadir_copd\copd_Patid_linkage_all.dta", replace

*/
clear all

log close

