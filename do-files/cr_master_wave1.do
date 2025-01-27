/*=========================================================================
DO FILE NAME:			cr_master_wave1.do

AUTHOR:					Marleen Bokern
VERSION:				v1

DATE VERSION CREATED: 	05/2022
						
DESCRIPTION OF FILE:	Runs data management for wave 1 copd cohort
						
*=========================================================================*/
clear all

cd $Dodir
do "01cr_copd_file_import.do"

cd $Dodir
do "02cr_copd_inc_exc_crit.do"

cd $Dodir
do "03cr_copd_patient.do"

cd $Dodir
do "04cr_copd_comorbidities.do"

cd $Dodir
do "copd_covariate_bmi.do"

cd $Dodir
do "copd_covariate_immunosuppression.do"

cd $Dodir
do "copd_covariate_ethnicity.do"

cd $Dodir
do "copd_covariate_smoking.do"

cd $Dodir
do "copd_covariate_exacerbation.do"

cd $Dodir
do "copd_covariate_creatinine.do"

cd $Dodir
do "05cr_copd_outcome.do"

cd $Dodir
do "06cr_copd_product_patids"

cd $Dodir
do "07cr_copd_product"

cd $Dodir
do "08cr_copd_med_check"

cd $Dodir
do "09cr_copd_treatment_ep"

cd $Dodir
do "10cr_copd_all_drugs"

cd $Dodir
do "10cr_copd_all_ics"

cd $Dodir
do "11cr_copd_treatment_combinations6m"

cd $Dodir
do "11cr_copd_treatment_combinations60d"

cd $Dodir
do "cr_copd_linkage_data"

cd $Dodir
do "12cr_copd_analytic_file_60d"

cd $Dodir
do "12cr_copd_analytic_file_60d_hosp"

cd $Dodir
do "12cr_copd_analytic_file_60d_all"

cd $Dodir
do "12cr_copd_analytic_file_6m"




cd $Dodir2019
do "01cr_copd_file_import_2019.do"

cd $Dodir2019
do "02cr_copd_inc_exc_crit_2019.do"

cd $Dodir2019
do "03cr_copd_patient_2019.do"

cd $Dodir2019
do "04cr_copd_comorbidities_2019.do"

cd $Dodir2019
do "copd_covariate_bmi_precovid.do"

cd $Dodir2019
do "copd_covariate_immunosuppression_precovid.do"

cd $Dodir2019
do "copd_covariate_ethnicity_precovid.do"

cd $Dodir2019
do "copd_covariate_smoking_precovid.do"

cd $Dodir2019
do "copd_covariate_exacerbation_precovid.do"

cd $Dodir2019
do "copd_covariate_creatinine_precovid.do"

cd $Dodir2019
do "05cr_copd_product_patids_precovid.do"

cd $Dodir2019
do "06cr_copd_product_precovid.do"

cd $Dodir2019
do "07cr_copd_med_check_precovid.do"

cd $Dodir2019 
do "08cr_copd_treatment_ep_precovid.do"

cd $Dodir2019 
do "09cr_copd_all_drugs_precovid.do"

cd $Dodir2019 
do "09cr_copd_all_ics_precovid.do"

cd $Dodir2019 
do "10cr_copd_patient_denominators_precovid.do"
