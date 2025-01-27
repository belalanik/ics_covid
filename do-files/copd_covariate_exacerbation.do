/*=========================================================================
DO FILE NAME:			copd_covariate_exacerbation.do

AUTHOR:					Marleen Bokern
VERSION:				v1

DATE VERSION CREATED: 	01/2023
						
DESCRIPTION OF FILE:	exacerbation definition: https://github.com/NHLI-Respiratory-Epi/AECOPD/blob/main/README.md

						In summary, an AECOPD can be in found in primary care EHRs by excluding any events on a COPD annual review day and searching for any of the following events:

						- A prescription of antibiotics and oral corticosteroids for 5–14 days*
						- Respiratory symptoms (2+) with a prescription for an antibiotic or oral corticosteroid
						- A lower respiratory tract infection (LRTI) code
						- An AECOPD code
						Any of these events closer together than 14 days are considered part of the same exacerbation event.
*=========================================================================*/

// Specify file names
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
glob no_Consultation = 12
glob no_Observation = 48
glob no_Referral = 1
glob no_Problem = 1
glob no_DrugIssue = 49

/***********************************************************************************************
>> HOUSEKEEPING
************************************************************************************************/

capture log close
clear all
log using "$Logdir/SB_copd_covariate_exacerbation.log", replace
local path_browsers "$Denominator\CPRD Aurum\Code browsers\2022_05"

cd "$Datadir_copd"

/******************************************************************************************************************
USE CODELISTS FOR AECOPD, LRTI, ANNUAL REVIEW, AECOPD SYMPTOMS
MERGE OBSERVATION FILES 
*******************************************************************************************************************/
foreach file of numlist 1/$no_Observation {
	noi di "Merging exacerbation observations, File `file'"
    use "$Copd_aurum_extract\\${file_stub}_Extract_Observation_`file'", clear
	drop pracid value numunitid obstypeid numrangelow numrangehigh
	drop if eventdate > td(30apr2021)
    merge m:1 medcodeid using "$Codelistsdir/COPD/Exacerbation/cl_AECOPD.dta", keep(match master) nogen
	rename term term_ae
	merge m:1 medcodeid using "$Codelistsdir/COPD/Exacerbation/cl_LRTI.dta", keep(match master) nogen
	rename term term_lrti
	merge m:1 medcodeid using "$Codelistsdir/COPD/Exacerbation/cl_copd_annual_review.dta", keep(match master) nogen
	rename term term_review
	merge m:1 medcodeid using "$Codelistsdir/COPD/Exacerbation/cl_AECOPD_symptoms.dta", keep(match master) nogen
	keep if aecopd == "1" | lrti == "1" | annual_review == "1" | breathlessness == "1" | cough == "1" | sputum == "1"
	replace term = term_ae if missing(term)
	replace term = term_lrti if missing(term)
	replace term = term_review if missing(term)
	drop term_ae term_lrti term_review obsdate enterdate
	merge m:1 patid using "${file_stub}_Patid_list_included_all.dta", keep(match) nogen
	if `file' == 1 {
		save "${file_stub}_Observation_exacerbation.dta", replace
	}
	if `file' > 1 {
		append using "${file_stub}_Observation_exacerbation.dta"
	}
	compress
	save "${file_stub}_Observation_exacerbation.dta", replace
}

/******************************************************************************************************************
MERGE DRUG ISSUE FILES  - SEARCH FOR ANTIBIOTIC/OCS ISSUES
*******************************************************************************************************************/
local drugissue : dir "" files "*DrugIssue*.dta", respectcase
disp `"`drugissue'"'

local product "antibiotic_ocs"
disp `"`product'"'

foreach file of numlist 1/$no_DrugIssue {
	noi di "Merging Drug Issue, File `file'"
    use "$Copd_aurum_extract\\${file_stub}_Extract_DrugIssue_`file'", clear
	drop pracid estnhscost enterdate dosageid quantity quantunitid duration
	drop if issuedate > td(30apr2021)
	merge m:1 patid using "${file_stub}_Patid_list_included_all.dta", keep(match) nogen
    merge m:1 prodcodeid using "$Codelistsdir\COPD\Exacerbation\cl_`product'.dta", keep(match) nogen
	if `file' == 1{
		save "${file_stub}_DrugIssue_`product'.dta", replace
	}
	if `file' > 1{
		append using "${file_stub}_DrugIssue_`product'.dta"
	}
	compress
	save "${file_stub}_DrugIssue_`product'.dta", replace
}

rename issuedate eventdate
rename oral_corticosteroid ocs
rename antibiotic abx

//Step 8. Append clinical event data to prescription event date to obtain all events of interest in one file
append using "${file_stub}_Observation_exacerbation.dta"

//Step 9. Sort new combined clinical and prescription event file by date fore each patient so that older events are listed first
gsort patid eventdate

/* AECOPD ALGORITHM (see Rothnie et al., 2016):
*
*	Excluding annual review days:
*		- ABX and OCS for 5–14 days; or
*		- Symptom (2+) definition with prescription of antibiotic or OCS; or
*		- LRTI code; or
*		- AECOPD code
*/

//Step 10. Collapse data by patient and date to get all events on the same day
destring abx ocs aecopd lrti annual_review breathlessness cough sputum, replace
collapse (max) annual_review abx ocs breathlessness cough sputum lrti aecopd, by(patid eventdate)

//Step 11. Remove events on an annual review day
drop if annual_review == 1
drop annual_review

//Step 12. Calculate total number of symptoms on a specific day
egen symptoms = rowtotal(breathlessness cough sputum)
order symptoms, after(sputum)

//Step 13. Only keep days where both antibiotics and oral corticosteroids were prescribed, days where a patient had 2 or more symptoms and an antibiotic or oral corticosteroid prescribed, days where a patient received an AECOPD code, or days where a patient received a LRTI code
keep if (abx == 1 & ocs == 1) ///
	  | (symptoms >= 2 & (abx == 1 | ocs == 1)) ///
	  | aecopd == 1 ///
	  | lrti == 1

***FOR WAVE 1: INDEX DATE: 01 MAR 2020. EXACERBATIONS ASSESS FOR 1 YEAR PRIOR TO THAT
drop if eventdate < td(01mar2019)	  
drop if eventdate > td(01mar2020)	  
//Step 14. Count any day with the events above as an exacerbation, excluding events closer together than 14 days
by patid (eventdate): gen exacerbation = 1 if _n == 1 | eventdate[_n - 1] < eventdate - 14

//Step 15. You now have a list of exacerbations for each patient. If you run the collapse command you can generate the total number of exacerbations for each patient over the given time peroid
collapse (sum) exacerbations = exacerbation, by(patid)

save "${file_stub}_covariate_exacerbation.dta", replace

log close
clear all