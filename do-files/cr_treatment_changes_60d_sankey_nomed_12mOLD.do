/*=========================================================================
DO FILE NAME:		    cr_treatment_changes_60d_sankey_nomed_12m.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	04/2023

DATASETS CREATED:       treatment_changes_w1_60d_sankey_nomed_12m.xlsx
						
DESCRIPTION OF FILE:	format treatment episode data so it can be used to create a sankey plot in networkD3 in R

*=========================================================================*/
clear all
cap log close 
log using "$Logdir\cr_treatment_changes_60d_sankey_nomed_12m.log", replace

use "$Datadir_copd\\${file_stub}_treatment_eps_full60d"
cap drop _merge
merge m:1 patid using "$Datadir_copd\\${file_stub}_Patient_included.dta", keepusing(enddate)

***generate indicators to show when change happened (before baseline, before 01 jun 2020, before 31 aug 2020)
gen time1 = 1 if date <= td(01mar2020)
gen time2 = 1 if date > td(01mar2020) & date <= td(01jun2020)
gen time3 = 1 if date > td(01jun2020) & date <= td(01sep2020)
gen time4 = 1 if date > td(01sep2020) & date <= td(01dec2020)
gen time5 = 1 if date > td(01dec2020) & date <= td(01mar2021)

assert time1 == 1 | time2 == 1 | time3 == 1 | time4 == 1 | time5 == 1 | _merge == 2

bysort patid time1 time2 time3 time4 time5 (date): gen keep = _n
bysort patid time1 time2 time3 time4 time5 (date): gen keep1 = _N
keep if keep == keep1
drop keep keep1

drop on_ics_single on_ics_laba on_laba_single on_lama_single on_laba_lama off_ics_single off_ics_laba off_laba_single off_lama_single off_laba_lama

gen other_med = 1 if ics_group != 1 & control_group != 1 & no_med != 1

gen ics1 = 1 if time1 == 1 & ics_group == 1
gen ics2 = 1 if time2 == 1 & ics_group == 1
gen ics3 = 1 if time3 == 1 & ics_group == 1
gen ics4 = 1 if time4 == 1 & ics_group == 1
gen ics5 = 1 if time5 == 1 & ics_group == 1

gen control1 = 1 if time1 == 1 & control_group == 1
gen control2 = 1 if time2 == 1 & control_group == 1
gen control3 = 1 if time3 == 1 & control_group == 1
gen control4 = 1 if time4 == 1 & control_group == 1
gen control5 = 1 if time5 == 1 & control_group == 1

gen other1 = 1 if time1 == 1 & other_med == 1
gen other2 = 1 if time2 == 1 & other_med == 1
gen other3 = 1 if time3 == 1 & other_med == 1
gen other4 = 1 if time4 == 1 & other_med == 1
gen other5 = 1 if time5 == 1 & other_med == 1

gen nomed1 = 1 if time1 == 1 & no_med == 1 
gen nomed2 = 1 if time2 == 1 & no_med == 1 
gen nomed3 = 1 if time3 == 1 & no_med == 1
gen nomed4 = 1 if time4 == 1 & no_med == 1 
gen nomed5 = 1 if time5 == 1 & no_med == 1

bysort patid (date): gen keep = _n

tab keep
assert keep <= 5

putexcel set "$Datadir_copd\treatment_changes_w1_60d_sankey_nomed_12mOLD", replace
putexcel A1 = "source"

// Fill the source and target cells with the desired pattern using a loop
local row 2
forvalues i = 0/15 {
    forvalues j = 1/4 {
        putexcel A`row' = "`i'"
        local row = `row' + 1
    }
}

putexcel B1 = "target"

local row 2
local startnum = 4
local endnum = `startnum' + 3

forvalues m = 1/4 {
	forvalues j = 1/4{
		forvalues i = `startnum'/`endnum' {
				putexcel B`row' = "`i'"
				local row = `row' + 1
			}
		}
		local startnum = `startnum' + 4
		local endnum = `startnum' + 3
}

*****values for each change in treatment 

putexcel C1 = "value"
local row = 2
***time: 01 mar 2020 --> 01 jun 2020
local start 01mar2020
local end 01jun2020
di `"`end'"'
sort patid date

local target ics
local source ics
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n + 1] | `source'1 == 1 & date[_n + 1] > td(`end') ) & enddate >= td(`end')
gen tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n + 1] | `source'1 == 1 & date[_n + 1] > td(`end') ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1 

local target control
local source ics
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target other
local source ics
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target nomed
local source ics
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target ics
local source control
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target control
local source control
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n + 1] | `source'1 == 1 & date[_n+1] > td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n + 1] | `source'1 == 1 & date[_n + 1] > td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target other
local source control
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target nomed
local source control
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target ics
local source other
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target control
local source other
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target other
local source other
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n+1]  | `source'1 == 1 & date[_n+1] > td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n+1]  | `source'1 == 1 & date[_n+1] > td(`end') | date < td(`end') & other1 == 1 & patid != patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target nomed
local source other
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target ics
local source nomed
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target control
local source nomed
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target other
local source nomed
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target nomed
local source nomed
count if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n + 1]  | `source'1 == 1 & date[_n + 1] > td(`end') | _merge == 2 | (date > td(`end') & patid != patid[_n - 1])) & enddate >= td(`end')
replace tag = 1 if (`target'2 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | `source'1 == 1 & patid != patid[_n + 1]  | `source'1 == 1 & date[_n + 1] > td(`end') | _merge == 2| (date > td(`end') & patid != patid[_n - 1])) & enddate >= td(`end') 
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

count if enddate < td(`end')

by patid : egen tag2 = max(tag)


***01 jun 2020 --> 01 sep 2020
local start 01jun2020
local end 01sep2020

local target ics
local source ics
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n + 1] | `source'1 == 1 & patid != patid[_n + 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n + 1] | `source'1 == 1 & patid != patid[_n + 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target control
local source ics
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target other
local source ics
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target nomed
local source ics
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target ics
local source control
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target control
local source control
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n+1] | `source'1 == 1 & patid != patid[_n+1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n+1] | `source'1 == 1 & patid != patid[_n+1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target other
local source control
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target nomed
local source control
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target ics
local source other
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target control
local source other
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target other
local source other
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n + 1] | `source'1 == 1 & patid != patid[_n + 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n + 1] | `source'1 == 1 & patid != patid[_n + 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target nomed
local source other
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target ics
local source nomed
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local target control
local source nomed
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1


local target other 
local source nomed
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1


local target nomed
local source nomed
					
count if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n + 1] | `source'1 == 1 & patid != patid[_n + 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | _merge == 2) & enddate >= td(`end')
replace tag = 1 if (`target'3 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] | `source'2 == 1 & patid != patid[_n + 1] | `source'1 == 1 & patid != patid[_n + 1] | `target'3 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] | _merge == 2) & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

count if enddate < td(`end') & enddate > td(`start')

***01 sep 2020 --> 01 dec 2020
local start 01sep2020
local end 01dec2020

local target ics
local source ics

gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						((`source'3 == 1 |`source'2 == 1 |`source'1 == 1 )& patid != patid[_n + 1]) 
					
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source ics
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 

count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other
local source ics
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source ics
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target ics
local source control
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source control
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						((`source'3 == 1 |`source'2 == 1 |`source'1 == 1 )& patid != patid[_n + 1]) 
					
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other
local source control
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source control
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target ics
local source other
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source other
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other
local source other
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						((`source'3 == 1 |`source'2 == 1 |`source'1 == 1 )& patid != patid[_n + 1]) 
					
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source other
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target ics
local source nomed
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source nomed
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other 
local source nomed
gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source nomed

gen condition`row' = 1 if (`target'4 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
							(`target'4 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] ) | ///
							(`target'4 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) | ///
							(`source'3 == 1 & patid != patid[_n + 1]) | ///
							(`source'2 == 1 & patid != patid[_n + 1]) | ///
							(`source'1 == 1 & patid != patid[_n + 1]) | ///
							(_merge == 2)
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

count if enddate < td(`end') & enddate > td(`start')

***01 dec 2020 --> 01 mar 2021
local start 01dec2020
local end 01mar2021

local target ics
local source ics

gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						((`source'4 == 1 |`source'3 == 1 |`source'2 == 1 |`source'1 == 1 )& patid != patid[_n + 1]) 
					
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source ics
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other
local source ics
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source ics
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target ics
local source control
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source control
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						((`source'4 == 1 |`source'3 == 1 |`source'2 == 1 |`source'1 == 1 )& patid != patid[_n + 1]) 				
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other
local source control
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source control
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target ics
local source other
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source other
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other
local source other
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						((`source'4 == 1 |`source'3 == 1 |`source'2 == 1 |`source'1 == 1 )& patid != patid[_n + 1]) 	
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source other
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target ics
local source nomed
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target control
local source nomed
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target other 
local source nomed
gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
						(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) | ///
						(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] & date > td(`start') & date <= td(`end')) 
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

local target nomed
local source nomed

gen condition`row' = 1 if (`target'5 == 1 & `source'4[_n - 1] == 1 & patid == patid[_n - 1]) | ///
							(`target'5 == 1 & `source'3[_n - 1] == 1 & patid == patid[_n - 1]) | ///
							(`target'5 == 1 & `source'2[_n - 1] == 1 & patid == patid[_n - 1] ) | ///
							(`target'5 == 1 & `source'1[_n - 1] == 1 & patid == patid[_n - 1] ) | ///
							(`source'4 == 1 & patid != patid[_n + 1]) | ///
							(`source'3 == 1 & patid != patid[_n + 1]) | ///
							(`source'2 == 1 & patid != patid[_n + 1]) | ///
							(`source'1 == 1 & patid != patid[_n + 1]) | ///
							(_merge == 2)
count if condition`row' == 1 & enddate >= td(`end')					
replace tag = 1 if condition`row' == 1 & enddate >= td(`end')
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
drop condition`row'
local row = `row' + 1

count if enddate < td(`end') & enddate > td(`start')

tab tag
gen flag = 1 if keep!=1 & missing(tag)
assert enddate < td(01mar2021) if flag == 1