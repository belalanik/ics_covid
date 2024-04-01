/*=========================================================================
DO FILE NAME:		    SB_cr_copd_linkage_data.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	05/2022

DATASETS CREATED:       copd_linkage_data
						
DESCRIPTION OF FILE:	Imports ONS and HES linkage files and saves as dta, renames

*=========================================================================*/

clear all
***run globals
capture log close 
log using $Logdir/SB_cr_copd_linkage_data.log, replace

local sourcedir "$Mainfolder\Linkage\Aurum_linked"
cd "`sourcedir'"
/*
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
*/
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

drop if admidate < td(01mar2020) | admidate > td(31mar2021)

rename admidate covid_hes_date
keep patid covid_hes_date

duplicates drop

sort patid covid_hes_date
by patid (covid_hes_date): gen keep = _n
keep if keep == 1
drop keep

compress
save "$Datadir_copd\SB_covid_hes_date_w1.dta", replace

clear all






