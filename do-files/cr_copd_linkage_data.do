/*=========================================================================
DO FILE NAME:		    cr_copd_linkage_data.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	05/2022

DATASETS CREATED:       copd_linkage_data
						
DESCRIPTION OF FILE:	Imports ONS and HES linkage files and saves as dta, renames

*=========================================================================*/

clear all
***run globals
capture log close 
log using $Logdir/cr_copd_linkage_data.log, replace

local sourcedir "$Mainfolder\COPD_2020\Results\Aurum_linked\Final restored as 12 May 23"
cd "`sourcedir'"
local linkagefiles : dir "`sourcedir'" files "*_22_001876*"

di `"`linkagefiles'"'

foreach x of local linkagefiles {
	di `"`x'"'
	import delimited "`x'", clear stringcols(_all)
	local oldname "`x'"
	di `"`oldname'"'
	local newname : subinstr local oldname "_22_001876.txt" ""
	compress
    save "`newname'.dta", replace
}

/**************************************************
GET COVID HES DIAGNOSES
***************************************************/
use "hes_diagnosis_epi.dta"

merge m:1 icd using "$Codelistsdir\Outcomes\cl_covid_hes.dta", keep(match)
g epistart1 = date(epistart, "DMY")
format epistart1 %td
drop epistart
rename epistart1 epistart

hist epistart

drop if epistart > td(31aug2020) | epistart < td(01mar2020)

rename epistart covid_hes_date
keep patid covid_hes_date

duplicates drop

sort patid covid_hes_date
by patid (covid_hes_date): gen keep = _n
keep if keep == 1
drop keep

compress
save "$Datadir_copd\copd_outcome_covid_hes_date_w1.dta", replace

clear all
/**************************************************
GET COVID DEATHS
***************************************************/
use "death_patient.dta"

drop cause_neonatal1 cause_neonatal2 cause_neonatal3 cause_neonatal4 cause_neonatal5 cause_neonatal6 cause_neonatal7 cause_neonatal8

gen covid = 0 
forval i = 1/15 {
    * Check if any of the variables contain the strings "U07.1" or "U07.2"
    replace covid = 1 if strpos("U07.1 U07.2", cause`i') > 0
}

keep if covid == 1

g dod1 = date(dod, "DMY")
format dod1 %td
drop dod
rename dod1 dod

hist dod

drop if dod > td(31aug2020) | dod < td(01mar2020)

rename dod covid_death_date
keep patid covid_death_date

duplicates drop

sort patid covid_death_date
by patid (covid_death_date): gen keep = _n
keep if keep == 1
drop keep

compress
save "$Datadir_copd\copd_outcome_covid_death_date_w1.dta", replace

clear all
/**************************************************
GET IMD
***************************************************/
use "patient_2019_imd.dta"

drop pracid
rename e2019_imd_5 imd

duplicates drop

compress
save "$Datadir_copd\copd_covariate_imd.dta", replace

clear all
log close

