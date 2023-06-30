/*=========================================================================
DO FILE NAME:		    cr05_copd_product_2019.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	05/2023

DATASETS CREATED:       copd_DrugIssue_ics_single_2019
						copd_DrugIssue_ics_laba_2019
						copd_DrugIssue_laba_lama_2019
						copd_DrugIssue_laba_single_2019
						copd_DrugIssue_lama_single_2019
						copd_DrugIssue_triple_therapy_2019
						
DESCRIPTION OF FILE:	Generate list of prescriptions for each drug type. uses patient list with excl/incl criteria applied. Only for people in 2019 cohort

*=========================================================================*/
/**************************************************************************
HOUSEKEEPING
***************************************************************************/

clear all

*run globals
capture log close 
log using $Logdir\05cr_copd_product_2019.log, replace

cd "$Datadir_copd2019/extracted"
****get dosages lookup file as dta

import delimited "$Denominator\CPRD Aurum\Lookups\2022_05\common_dosages.txt", clear
save "$Datadir_copd2019\common_dosages.dta", replace

glob common_dosages "$Datadir_copd2019\common_dosages.dta"

/*******************************************************************************************************************
merge each drug type observation file with included patient list and dosage lookup file
*******************************************************************************************************************/

local drugissue ics_single ics_laba laba_lama laba_single lama_single triple_therapy
disp `"`drugissue'"'

foreach drug in `drugissue' {
	noi di "File `drug'"
    use "${file_stub}_DrugIssue_`drug'_precovid.dta", clear
	merge m:1 patid using "$Datadir_copd2019/extracted/${file_stub}_Patient_included_precovid.dta", keep(match) nogen
	merge m:1 dosageid using "$common_dosages", keep(match master) nogen
	drop dose_duration dose_max_average change_dose dose_unit dose_interval dose_frequency choice_of_dose dose_number enterdate
	cap drop drugissues
	compress
	save "${file_stub}_`drug'_precovid.dta", replace
}


log close
clear all