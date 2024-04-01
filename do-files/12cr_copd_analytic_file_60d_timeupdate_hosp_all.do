/*=========================================================================
DO FILE NAME:		    12cr_copd_analytic_file_60d_timeupdate_hosp.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	03/2022

DATASETS CREATED:       copd_analytic_file
						
DESCRIPTION OF FILE:	

*=========================================================================*/
clear all
***run globals
capture log close 
log using $Logdir/12cr_copd_analytic_file_60d_hosp_all.log, replace

cd "$Datadir_copd"

import delim "$Denominator\CPRD Linkage Source Data Files\Version22\set_22_Source_Aurum\Aurum_enhanced_eligibility_January_2022.txt", stringcols(_all)

merge 1:1 patid using "${file_stub}_Patient_included_all.dta", keep(match)
cap drop _merge
drop hes_apc_e ons_death_e sgss_e lsoa_e chess_e hes_op_e hes_ae_e hes_did_e cr_e icnarc_e mhds_e rtds_e sact_e smokstatus smoking_date

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
merge 1:1 patid using "${file_stub}_covariate_asthma_past.dta", nogen keep(match master)

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
merge 1:1 patid using "$Datadir_copd\covariate_imd.dta", nogen keep(match master)

***OUTCOMES
***positive covid test
merge 1:1 patid using "${file_stub}_outcome_pos_covid_test_w1.dta", nogen keep(match master)

***primary covid hospitalisation
merge 1:1 patid using "$Datadir_copd\outcome_covid_hes_date_w1.dta", nogen keep(match master)

***any covid hospitalisation
merge 1:1 patid using "$Datadir_copd\outcome_covid_hes_date_w1_any.dta", nogen keep(match master)

***any hospitalisation
merge 1:m patid using "$Datadir_copd\hes_epi_date_w1.dta", nogen keep(match master)

***covid death
merge m:1 patid using "$Datadir_copd\outcome_covid_death_date_w1.dta", nogen keep(match master) 

***all death dates
merge m:1 patid using "$Datadir_copd\death_date_w1.dta", nogen keep(match master) 

save "${file_stub}_analytic_file_hosp_w1_no_exposures_all.dta", replace

***EXPOSURES: this is not possible as there are multiple rows per patient in both the master and using data. 
*merge 1:m patid using "${file_stub}_treatment_eps_60d", nogen keep(match master)
*rop on_ics_single on_ics_laba on_laba_single on_lama_single on_laba_lama on_triple_inhaler off_ics_single off_ics_laba off_laba_single off_lama_single off_laba_lama off_triple_inhaler date ics_single ics_laba laba_single lama_single laba_lama triple_inhaler triple ics_only laba_only lama_only ics_lama no_med _merge ics_group control_group ics_group

duplicates drop
export delim "${file_stub}_analytic_file_hosp_w1_60d_all.csv", replace

clear all


