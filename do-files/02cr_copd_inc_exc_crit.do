/*=========================================================================
DO FILE NAME:			02cr_copd_inc_exc_crit.do

AUTHOR:					Marleen Bokern
VERSION:				v1

DATE VERSION CREATED: 	12/2022
						
DATASETS CREATED:       
						copd_exclusion_asthma.dta
						copd_DrugIssue_ltra.dta
						copd_Observation_smoking.dta
						copd_Patid_list_included.dta
						copd_Patient_smoking_inclusion.dta
						
DESCRIPTION OF FILE: 	Gets full list of events for exclusion/inclusion criteria
					
*=========================================================================*/
clear all

***run globals
cap log close
log using $Logdir\02cr_copd_inc_exc_crit.log, replace

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

// Specify number of different files CHANGE THIS
glob no_Patient = 1
glob no_Practice = 1
glob no_Staff = 1
glob no_Consultation = 12
glob no_Observation = 48
glob no_Referral = 1
glob no_Problem = 1
glob no_DrugIssue = 49

/*******************************************************************************
>> IDENTIFY ALL COPD OBSERVATIONS
*******************************************************************************/
local disease copd

foreach file of numlist 1/$no_Observation {
	noi di "Merging `disease' observations, File `file'"
    use "$Copd_aurum_extract\\${file_stub}_Extract_Observation_`file'", clear
	drop if eventdate > td(30apr2021)
	keep patid medcodeid eventdate 
    merge m:1 medcodeid using "$Codelistsdir\cl_copd_aurum_052022.dta", keep(match) nogen
	if `file' == 1 {
		save "${file_stub}_Observation_`disease'.dta", replace
	}
	if `file' > 1 {
		append using "${file_stub}_Observation_`disease'.dta"
	}
	compress
	save "${file_stub}_Observation_`disease'.dta", replace
}

/*******************************************************************************
>> ASTHMA
*******************************************************************************/
local disease asthma

foreach file of numlist 1/$no_Observation {
	noi di "Merging `disease' observations, File `file'"
	use "$Copd_aurum_extract\\${file_stub}_Extract_Observation_`file'", clear
	drop if eventdate > td(30apr2021)
	keep patid medcodeid eventdate
	merge m:1 medcodeid using "$Codelistsdir\cl_asthma_aurum_052022.dta", keep(match) nogen keepusing(term)
	capture confirm string variable medcodeid
	drop if missing(medcodeid)
	if `file' == 1 {
			save "${file_stub}_Observation_`disease'.dta", replace
		}
	if `file' > 1 {
			append using "${file_stub}_Observation_`disease'.dta"
		}
	compress
	save "${file_stub}_Observation_`disease'.dta", replace
}

clear all 

/*******************************************************************************
>> IDENTIFY PEOPLE WITH SMOKING RECORD
*******************************************************************************/
local disease smoking

foreach file of numlist 1/$no_Observation {
	noi di "Merging `disease' observations, File `file'"
    use "$Copd_aurum_extract\\${file_stub}_Extract_Observation_`file'", clear
	drop pracid
	drop if eventdate > td(30apr2021)
    merge m:1 medcodeid using "$Codelistsdir/cl_`disease'.dta", keep(match) nogen
	capture confirm string variable medcodeid
	drop if missing(medcodeid)
	if `file' == 1 {
		save "${file_stub}_Observation_`disease'.dta", replace
	}
	if `file' > 1 {
		append using "${file_stub}_Observation_`disease'.dta"
	}
	compress
	save "${file_stub}_Observation_`disease'.dta", replace
}

clear all

/*******************************************************************************
>> IDENTIFY PEOPLE WITH OTHER CHRONIC RESPIRATORY DISEASE
*******************************************************************************/
local disease other_resp_disease

foreach file of numlist 1/$no_Observation {
	noi di "Merging `disease' observations, File `file'"
    use "$Copd_aurum_extract\\${file_stub}_Extract_Observation_`file'", clear
	keep patid medcodeid eventdate
	drop if eventdate > td(30apr2021)
    merge m:1 medcodeid using "$Codelistsdir/Exclusion/cl_`disease'.dta", keep(match)
	capture confirm string variable medcodeid
	drop if missing(medcodeid)
	if `file' == 1 {
		save "${file_stub}_Observation_`disease'.dta", replace
	}
	if `file' > 1 {
		append using "${file_stub}_Observation_`disease'.dta"
	}
	compress
	save "${file_stub}_Observation_`disease'.dta", replace
}

/*******************************************************************************
>> LTRA, TRIPLE THERAPY
*******************************************************************************/

local drug ltra triple_therapy nebuliser
disp `"`drug'"'

foreach product of local drug {
	foreach file of numlist 1/$no_DrugIssue {
		noi di "Merging `product', File `file'"
		use "$Copd_aurum_extract\\${file_stub}_Extract_DrugIssue_`file'", clear
		drop if issuedate > td(30apr2021)
		merge m:1 prodcodeid using "$Codelistsdir\Product_codelists\cl_`product'.dta", keep(match) nogen
		capture confirm string variable prodcodeid
		drop if missing(prodcodeid)
		if `file' == 1{
			save "${file_stub}_DrugIssue_`product'.dta", replace
		}
		if `file' > 1{
			append using "${file_stub}_DrugIssue_`product'.dta"
		}
		compress
		save "${file_stub}_DrugIssue_`product'.dta", replace
	}
}

log close
clear all