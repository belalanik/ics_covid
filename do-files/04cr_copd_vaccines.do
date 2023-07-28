/*=========================================================================
DO FILE NAME:		    04cr_copd_vaccines.do

AUTHOR:					Marleen Bokern, adapted from Jeremy Brown, Helena Carreira 

VERSION:				v1

DATE VERSION CREATED: 	07/2023

DATASETS CREATED:     
						
DESCRIPTION OF FILE:	

*=========================================================================*/
clear all

cap log close
log using $Logdir\04cr_copd_vaccines, replace
cd $Codelistsdir\Vaccines


import delim "pneumo_terms", clear stringcols(_all)
duplicates drop
save "$Codelistsdir\Vaccines\\cl_pneumo_vaccine_medcodes.dta", replace

import delim "pneumo_products", clear stringcols(_all)
duplicates drop
save "$Codelistsdir\Vaccines\\cl_pneumo_vaccine_product.dta", replace

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

// Specify number of different files
glob no_Consultation = 12
glob no_Observation = 48
glob no_Referral = 1
glob no_Problem = 1
glob no_DrugIssue = 49

/***************************************************************************************
VACCINE MEDCODES
*****************************************************************************************/

local vaccine pneumo_vaccine flu_vaccine
disp `"`vaccine'"'

foreach disease of local vaccine {
	foreach file of numlist 1/$no_Observation {
		noi di "Merging `disease' Observations, File `file'"
		use "$Copd_aurum_extract/${file_stub}_Extract_Observation_`file'", clear
		drop if eventdate > td(30apr2021)
		merge m:1 medcodeid using "$Codelistsdir\Vaccines\cl_`disease'_medcodes.dta", keep(match) nogen
		merge m:1 patid using "${file_stub}_Patient_included.dta", keep(match) nogen
		keep patid medcodeid term obsdate enterdate eventdate
		if `file' == 1{
			save "${file_stub}_Observation_`disease'_medcodes.dta", replace
			}
		if `file' > 1{
			append using "${file_stub}_Observation_`disease'_medcodes.dta"
		}
		compress
		save "${file_stub}_Observation_`disease'_medcodes.dta", replace
		}
		
	}
	
/***************************************************************************************
VACCINE PRODCODES
*****************************************************************************************/
	
local vaccine pneumo_vaccine flu_vaccine
disp `"`vaccine'"'

foreach disease of local vaccine {
	foreach file of numlist 1/$no_DrugIssue {
		noi di "Merging `disease' product, File `file'"
		use "$Copd_aurum_extract\\${file_stub}_Extract_DrugIssue_`file'", clear
		drop if issuedate > td(30apr2021)
		drop pracid estnhscost
		merge m:1 prodcodeid using "$Codelistsdir\Vaccines\cl_`disease'_product.dta", keep(match) nogen
		cap drop drugissues
		if `file' == 1{
			save "${file_stub}_DrugIssue_`disease'_product.dta", replace
		}
		if `file' > 1{
			append using "${file_stub}_DrugIssue_`disease'_product.dta"
		}
		compress
		save "${file_stub}_DrugIssue_`disease'_product.dta", replace
	}
}

log close
clear all