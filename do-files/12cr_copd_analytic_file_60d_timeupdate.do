/*=========================================================================
DO FILE NAME:		    10cr_copd_analytic_file_60d_timeupdate.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	03/2022

DATASETS CREATED:       copd_analytic_file
						
DESCRIPTION OF FILE:	

*=========================================================================*/
clear all
***run globals
capture log close 
log using $Logdir/10cr_copd_analytic_file_6m.log, replace

cd "$Datadir_copd"

use "copd_Patient_included.dta"
cap drop _merge

****MERGE IN COVARIATES. ALL COVARIATES ARE CODED AS DATES
***BMI 
merge 1:1 patid using "copd_covariate_bmi.dta", nogen

***ETHNICITY
merge 1:1 patid using "copd_Patient_ethnicity", nogen
drop eth16

**DIABETES
merge 1:1 patid using "copd_covariate_diabetes.dta", nogen

**HYPERTENSION
merge 1:1 patid using "copd_covariate_hypertension.dta", nogen

**CVD
merge 1:1 patid using "copd_covariate_cvd.dta", nogen

**CKD
merge 1:1 patid using "${file_stub}_covariate_kidney", nogen

**CANCER
merge 1:1 patid using "copd_covariate_allcancers.dta", nogen

**ASTHMA
merge 1:1 patid using "copd_covariate_asthma.dta", nogen

***COPD EXACERBATIONS IN PAST YEAR 
merge 1:1 patid using "${file_stub}_covariate_exacerbation.dta", nogen

***SMOKING
merge 1:1 patid using "${file_stub}_covariate_smoking.dta", nogen

***immunosuppression
merge 1:1 patid using "${file_stub}_covariate_immunosuppression_patid.dta", nogen
*** vaccines


***OUTCOMES
***positive covid test
merge 1:1 patid using "copd_outcome_pos_covid_test_w1.dta", nogen

***EXPOSURES
merge 1:m patid using "${file_stub}_treatment_eps_w1_60d", nogen
drop on_ics_single on_ics_laba on_laba_single on_lama_single on_laba_lama off_ics_single off_ics_laba off_laba_single off_lama_single off_laba_lama
duplicates drop
export delim "copd_analytic_file_w1_60d_timeupdate.csv", replace


