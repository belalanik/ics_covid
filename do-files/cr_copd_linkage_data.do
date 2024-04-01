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

local sourcedir "$Mainfolder\Linkage\Aurum_linked"
cd "`sourcedir'"
local linkagefiles : dir "`sourcedir'" files "*_22_001876_DM*"

di `"`linkagefiles'"'

foreach x of local linkagefiles {
	di `"`x'"'
	import delimited "`x'", clear stringcols(_all)
	local oldname "`x'"
	di `"`oldname'"'
	local newname : subinstr local oldname "_22_001876_dm.txt" ""
	compress
    save "`newname'.dta", replace
}

/**************************************************
GET PRIMARY COVID HES DIAGNOSES
***************************************************/
use "hes_primary_diag_hosp.dta"

rename icd_primary icd
merge m:1 icd using "$Codelistsdir\Outcomes\cl_covid_hes.dta", keep(match)

g admidate1 = date(admidate, "DMY")
format admidate1 %td
drop admidate
rename admidate1 admidate

hist admidate

drop if admidate < td(01mar2020) | admidate > td(31aug2020)

rename admidate covid_hes_date
keep patid covid_hes_date

duplicates drop

sort patid covid_hes_date
by patid (covid_hes_date): gen keep = _n
keep if keep == 1
drop keep

compress
save "$Datadir_copd\outcome_covid_hes_date_w1.dta", replace

clear all
/**************************************************
GET ANY COVID HES DIAGNOSES
***************************************************/
use "hes_diagnosis_epi.dta"

merge m:1 icd using "$Codelistsdir\Outcomes\cl_covid_hes.dta", keep(match)

g epistart1 = date(epistart, "DMY")
format epistart1 %td
drop epistart
rename epistart1 epistart

hist epistart

drop if epistart < td(01mar2020) | epistart > td(31aug2020)

rename epistart any_covid_hes_date
keep patid any_covid_hes_date

duplicates drop

sort patid any_covid_hes_date
by patid (any_covid_hes_date): gen keep = _n
keep if keep == 1
drop keep

compress
save "$Datadir_copd\outcome_covid_hes_date_w1_any.dta", replace

clear all


/**************************************************
GET ALL HOSPITALISATIONS
***************************************************/
use "hes_hospital.dta"
merge 1:m spno using "hes_primary_diag_hosp.dta", nogen
rename icd_primary icd
merge m:1 icd using "$Codelistsdir\Outcomes\cl_covid_hes.dta"

gen covid_hosp = 1 if _merge == 3

drop discharged spno icd icdx alt_code description _merge

g hosp_date = date(admidate, "DMY")
format hosp_date %td
drop admidate

drop if hosp_date < td(01mar2020) | hosp_date > td(31aug2020)
hist hosp_date

bysort patid: egen covid_hes_any = max(covid_hosp)
replace covid_hes_any = 0 if missing(covid_hes_any)

keep patid hosp_date covid_hosp covid_hes_any

duplicates drop

sort patid hosp_date
by patid (hosp_date): gen hosp_num = _n

rename hosp_date hes_date

compress
save "$Datadir_copd\hes_epi_date_w1.dta", replace
export delim "$Datadir_copd\hes_epi_date_w1.csv", replace

clear all

/**************************************************
GET COVID DEATHS
***************************************************/
use "death_patient.dta"

gen covid = 0 
gen covid1 = 0
gen covid2 = 0

forval i = 1/15 {
    * Check if any of the variables contain the strings "U07.1" or "U07.2"
    replace covid = 1 if strpos(cause`i', "U07.1") > 0 | strpos(cause`i', "U07.2") > 0
	replace covid1 = 1 if strpos(cause`i', "U07.1") > 0 
	replace covid2 = 1 if  strpos(cause`i', "U07.2") > 0
}

keep if covid == 1

gen prim_cause_covid = 1 if cause == "U07.1" | cause == "U07.2" 

g dod1 = date(dod, "DMY")
format dod1 %td
drop dod
rename dod1 dod

hist dod

drop if dod < td(01mar2020) | dod > td(31aug2020)

rename dod covid_death_date
keep patid covid_death_date prim_cause_covid

duplicates drop

sort patid covid_death_date
by patid (covid_death_date): gen keep = _n
keep if keep == 1
drop keep

compress
save "$Datadir_copd\outcome_covid_death_date_w1.dta", replace

clear all

/**************************************************
GET ALL DEATHS
***************************************************/
use "death_patient.dta"

g dod1 = date(dod, "DMY")
format dod1 %td
drop dod
rename dod1 death_date_ons

keep patid death_date_ons

duplicates drop

sort patid death_date_ons
by patid (death_date_ons): gen keep = _n
keep if keep == 1
drop keep

compress
save "$Datadir_copd\death_date_w1.dta", replace

clear all

/**************************************************
GET IMD
***************************************************/
import delimited "patient_2019_imd_22_001876.txt", clear stringcols(_all)

drop pracid
rename e2019_imd_5 imd

duplicates drop

compress
save "$Datadir_copd\covariate_imd.dta", replace

clear all
log close

