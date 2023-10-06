/*=========================================================================
DO FILE NAME:		    cr_treatment_changes_6m_sankey_nomed_12m.do

AUTHOR:					Marleen Bokern

VERSION:				v1

DATE VERSION CREATED: 	04/2023

DATASETS CREATED:       treatment_changes_w1_6m_sankey_nomed_12m.xlsx
						
DESCRIPTION OF FILE:	format treatment episode data so it can be used to create a sankey plot in networkD3 in R

*=========================================================================*/
clear all
cap log close 
log using "$Logdir\cr_treatment_changes_6m_sankey_nomed_12m.log", replace

use "$Datadir_copd\\${file_stub}_treatment_eps_full6m"
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

putexcel set "$Datadir_copd\treatment_changes_6m_sankey_nomed_12m_w1", replace
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

***time: 01 mar 2020 --> 01 jun 2020
local start 01mar2020
local end 01jun2020
di `"`end'"'
sort patid date

local target ics
local source ics

* ics = 1 , control = 2, other = 3, no med = 4
gen tp1 = 1 if ics_group == 1 & date <= td(`start')
replace tp1 = 2 if control_group == 1 & date <= td(`start')
replace tp1 = 3 if (lama_only == 1 |ics_lama == 1 | ics_only == 1 | laba_only == 1) & date <= td(`start')
replace tp1 = 4 if (no_med == 1 & date <= td(`start')) | _merge == 2 | keep == 1 & date > td(`start')

bysort patid (date): egen check = total(tp1 != .)
assert check == 1
drop check

by patid : egen treat_tp1 = max(tp1)
replace treat_tp1 = 99 if enddate < td(`start') 

drop tp1

*tp2 = 01 jun
local start 01jun2020

gen tp2 = 1 if ics2 == 1
replace tp2 = 2 if control2 == 1
replace tp2 = 3 if other2 == 1
replace tp2 = 4 if nomed2 == 1

assert tp2 !=. if date > td(01mar2020) & date < td(`start')
bysort patid (date): egen check = total(tp2 != .) 
assert check <= 1
drop check
by patid : egen treat_tp2 = max(tp2)
replace treat_tp2 = treat_tp1 if treat_tp2 ==.
replace treat_tp2 = 99 if enddate < td(`start') 

drop tp2 

***time: 01 Sep 2020
local start 01sep2020
local end 01dec2020

gen tp3 = 1 if ics3 == 1
replace tp3 = 2 if control3 == 1
replace tp3 = 3 if other3 == 1
replace tp3 = 4 if nomed3 == 1

assert tp3 != . if date > td(01jun2020) & date < td(`start')
bysort patid (date): egen check = total(tp3 != .)
assert check <= 1
drop check

by patid: egen treat_tp3 = max(tp3)
replace treat_tp3 = treat_tp2 if treat_tp3 == .
replace treat_tp3 = 99 if enddate < td(`start') 

drop tp3

***time: 01 Dec 2020
local start 01dec2020

gen tp4 = 1 if ics4 == 1
replace tp4 = 2 if control4 == 1
replace tp4 = 3 if other4 == 1
replace tp4 = 4 if nomed4 == 1

assert tp4 != . if date > td(01sep2020) & date < td(`start')
bysort patid (date): egen check = total(tp4 != .)
assert check <= 1
drop check

by patid: egen treat_tp4 = max(tp4)
replace treat_tp4 = treat_tp3 if treat_tp4 == .
replace treat_tp4 = 99 if enddate < td(`start') 

drop tp4

***time: 01 Mar 2021
local start 01mar2021

gen tp5 = 1 if ics5 == 1
replace tp5 = 2 if control5 == 1
replace tp5 = 3 if other5 == 1
replace tp5 = 4 if nomed5 == 1

assert tp5 != . if date > td(01dec2020) & date < td(`start')
bysort patid (date): egen check = total(tp5 != .)
assert check <= 1
drop check

by patid: egen treat_tp5 = max(tp5)
replace treat_tp5 = treat_tp4 if treat_tp5 == .
replace treat_tp5 = 99 if enddate < td(`start') 

drop tp5

keep patid treat_tp1 treat_tp2 treat_tp3 treat_tp4 treat_tp5
duplicates drop

gen tag2 =.
gen tag3 =.
gen tag4 =. 
gen tag5 =.

putexcel C1 = "value"
local row = 2


foreach time of numlist 1/4 {
	local start treat_tp`time'
	di `"`start'"'
	local time2 = `time'+1
	local end treat_tp`time2'
	di `"`end'"'

	foreach source of numlist 1/4 {
		foreach target of numlist 1/4 {
			count if `start' == `source' & `end' == `target'
			replace tag`time2' = 1 if `start' == `source' & `end' == `target'
			return list
			matrix stats = r(N)
			putexcel C`row' = matrix(stats)
			local row = `row' + 1
	} 
}
}

tab tag2, missing
tab tag3, missing
tab tag4, missing
tab tag5, missing

tab treat_tp1, missing
tab treat_tp2, missing
tab treat_tp3, missing
tab treat_tp4, missing
tab treat_tp5, missing



log close 
clear all





/****** this code does the same thing, but without the loop

*first transition march - jun
local i = 2 
local start treat_tp1
local end treat_tp2

local source 1 //*ics -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1


*second transition jun-sep
local i = 3 
local start treat_tp2
local end treat_tp3

local source 1 //*ics -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

*third transition sep - dec
local i = 4 
local start treat_tp3
local end treat_tp4

local source 1 //*ics -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

*fourth transition dec - mar
local i = 5
local start treat_tp4
local end treat_tp5

local source 1 //*ics -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 1 //*ics -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 2 //*control -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 3 //*other -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> ics
local target 1
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> control
local target 2
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> other
local target 3
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1

local source 4 //*nomed -> nomed
local target 4
count if `start' == `source' & `end' == `target'
replace tag`i' = 1 if `start' == `source' & `end' == `target'
return list
matrix stats = r(N)
putexcel C`row' = matrix(stats)
local row = `row' + 1




***/