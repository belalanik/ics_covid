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
log using $Logdir/12cr_copd_analytic_file_60d.log, replace

cd "$Datadir_copd"

use "${file_stub}_Patient_included.dta"
cap drop _merge

****MERGE IN COVARIATES. ALL COVARIATES ARE CODED AS DATES
***BMI 
merge 1:1 patid using "${file_stub}_covariate_bmi.dta", nogen keep(match master)

***ETHNICITY
merge 1:1 patid using "${file_stub}_Patient_ethnicity", nogen keep(match master)
drop eth16

**DIABETES
merge 1:1 patid using "${file_stub}_covariate_diabetes.dta", nogen keep(match master)

**HYPERTENSION
merge 1:1 patid using "${file_stub}_covariate_hypertension.dta", nogen keep(match master)

**CVD
merge 1:1 patid using "${file_stub}_covariate_cvd.dta", nogen keep(match master)

**CKD
merge 1:1 patid using "${file_stub}_covariate_kidney", nogen keep(match master)

**CANCER
merge 1:1 patid using "${file_stub}_covariate_allcancers.dta", nogen keep(match master)

**ASTHMA
merge 1:1 patid using "${file_stub}_covariate_asthma.dta", nogen keep(match master)

***COPD EXACERBATIONS IN PAST YEAR 
merge 1:1 patid using "${file_stub}_covariate_exacerbation.dta", nogen keep(match master)

***SMOKING
merge 1:1 patid using "${file_stub}_covariate_smoking.dta", nogen keep(match master)

***immunosuppression
merge 1:1 patid using "${file_stub}_covariate_immunosuppression_patid.dta", nogen keep(match master)

*** flu vaccines
merge 1:1 patid using "${file_stub}_covariate_flu_vaccine.dta", nogen keepusing(flu_vacc_date) keep(match master)

*** pneumo vaccines
merge 1:1 patid using "${file_stub}_covariate_pneumo_vaccine.dta", nogen keepusing(pneumo_vacc_date) keep(match master)

***IMD 
merge 1:1 patid using "${file_stub}_covariate_imd.dta", nogen keep(match master)

***OUTCOMES
***positive covid test
merge 1:1 patid using "${file_stub}_outcome_pos_covid_test_w1.dta", nogen keep(match master)

***covid hospitalisation
merge 1:1 patid using "${file_stub}_outcome_covid_hes_date_w1.dta", nogen keep(match master)

***covid hospitalisation
merge 1:1 patid using "${file_stub}_outcome_covid_death_date_w1.dta", nogen keep(match master) 

save "${file_stub}_analytic_file_w1_no_exposures.dta", replace

***EXPOSURES
merge 1:m patid using "${file_stub}_treatment_eps_60d", nogen keep(match master)
drop on_ics_single on_ics_laba on_laba_single on_lama_single on_laba_lama on_triple_inhaler off_ics_single off_ics_laba off_laba_single off_lama_single off_laba_lama off_triple_inhaler date ics_single ics_laba laba_single lama_single laba_lama triple_inhaler triple ics_only laba_only lama_only ics_lama no_med _merge ics_group control_group ics_group

duplicates drop
export delim "${file_stub}_analytic_file_w1_60d.csv", replace

clear all


