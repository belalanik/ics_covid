/*=========================================================================
DO FILE NAME:		    10cr_copd_analytic_file_6m.do

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

*** flu vaccines
merge 1:1 patid using "${file_stub}_covariate_flu_vaccine.dta", nogen keepusing(flu_vacc_date)

*** pneumo vaccines
merge 1:1 patid using "${file_stub}_covariate_pneumo_vaccine.dta", nogen keepusing(pneumo_vacc_date)

***IMD 
merge 1:1 patid using "${file_stub}_covariate_imd.dta", nogen

***OUTCOMES
***positive covid test
merge 1:1 patid using "copd_outcome_pos_covid_test_w1.dta", nogen

***covid hospitalisation
merge 1:1 patid using "copd_outcome_covid_hes_date_w1.dta", nogen

***covid hospitalisation
merge 1:1 patid using "copd_outcome_covid_death_date_w1.dta", nogen

***EXPOSURES
merge 1:1 patid using "${file_stub}_treatment_baseline_6m", nogen

merge 1:m patid using "${file_stub}_treatment_eps_full6m", nogen keepusing(ics_ever control_ever)
duplicates drop


export delim "copd_analytic_file_6m.csv", replace


