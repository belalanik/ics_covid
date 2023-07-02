/*=========================================================================
DO FILE NAME:	copd_covariate_creatinine

AUTHOR:					Marleen Bokern, adapted from Angel Wong (adapted from Kate Mansfield and Helen McDonald's work)

VERSION:				v1.0
DATE VERSION CREATED:	v1 01/2023				

DESCRIPTION OF FILE: 
	Extracts serum creatinine test results.
	Calculates eGFR
	Optional code to use ethnicity data to calculate eGFR if available
	
	Arguements (options) required:
		* obsfile						// path and name of file containing test result 
										// extract files (exclude the underscore and the number of the file)
		* obsfilesnum 					// number of test files to loop through
		* serum_creatinine_codelist		// list of medcodes that are likely to be used for serum creatinine test results
		* savefile						// string containing name of file to save
		* patientfile					// string containing name of file containing patient details - gender and realyob
		* ethnicityfile  				// optional string with filename of file containing 
										// ethnicity data, ethnicity should be recorded in a var called ethdm
*=========================================================================*/

clear all

capture log close
log using $Logdir/covariate_creatinine_copd.log, replace

cd "$Projectdir"
cd "$Datadir_copd\extracted"

****get NumUnit lookup file as dta

import delimited "J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Lookups\2022_05\NumUnit.txt", clear
save "$Datadir_copd\NumUnit", replace

glob NumUnit "$Datadir_copd\NumUnit.dta"

****creatinine codelist was originally from Angel, adapted using code from Emily Herrett
****loop thorugh all observation files to generate dataset of all creatinine codes

foreach file of numlist 1/$no_Observation {
	noi di "Merging creatinine observations, File `file'"
    use "${file_stub}_Extract_Observation_`file'", clear
	merge m:1 patid using "${file_stub}_Patid_list_included.dta", keep(match) nogen
    merge m:1 medcodeid using "$Codelistsdir/cl_creatinine.dta", keep(match) nogen
	if `file' == 1 {
		save "${file_stub}_Observation_creatinine.dta", replace
	}
	if `file' > 1 {
		append using "${file_stub}_Observation_creatinine.dta"
	}
	compress
	save "${file_stub}_Observation_creatinine.dta", replace
}

use "${file_stub}_Observation_creatinine.dta"
unique patid

drop if eventdate > td(30apr2021)

duplicates drop //**0

destring numunitid, replace	
	/*******************************************************************************
	#A3. Drop unnecessary vars and label variables.
	*******************************************************************************/	
	merge m:1 numunitid using "$Datadir_copd\NumUnit", keep(master match) nogen
	
	destring value, gen(SCr)
	*rename value SCr
	 
	*rename variables and add labels
	rename numunitid    unit     	// unit of measure
	rename numrangelow  rangeFrom 	//"normal range from"
	rename numrangehigh rangeTo		//"normal range to"
	
	*label variable SCr "SCr: SCr result"
	label variable unit "unit of measure"	
	label variable rangeFrom "rangeFrom: normal range from"
	label variable rangeTo "rangeTo: normal range to"	
	
	/*******************************************************************************
	#A4. Drop any duplicate records
		Drop records with missing dates or SCr results
	*******************************************************************************/	
	duplicates drop

	* drop if eventdate missing 
	* but check if sysdate available and replace missing eventdate with sysdate if available
	replace obsdate = enterdate if (obsdate ==. & enterdate !=.)
	drop if obsdate ==.
	
	* drop if creatinine value is missing or zero
	drop if SCr == 0 
	drop if SCr ==.
	
	/*******************************************************************************
	#A6. Drop records with SCr values that are very low or very high
	*******************************************************************************/
	* drop improbable values for SCr i.e. <20 or >3000
	gen improbable = 0
	recode improbable 0 = 1 if SCr < 20 | SCr > 3000 //*593 values

	drop if improbable == 1
	drop improbable	

	/*******************************************************************************
	#A7. Add notes and labels to the data
	*******************************************************************************/

	save "copd_creatinine_values", replace
	
/*******************************************************************************
================================================================================
#B. CALCULATE eGFR
================================================================================
*******************************************************************************/
	/**************************************************************************
	#B1. Open patient details file, sort and save relevant details ready to merge
		with test results file
	**************************************************************************/
	use "copd_Patient_included.dta", clear
	keep patid gender yob
	
	destring gender, replace
	destring yob, replace
	
	sort patid
	
	merge 1:m patid using "copd_creatinine_values.dta", nogen keep(match) force // only keep patients with test results available

	/**************************************************************************
	#B2. Calculate age at event
	**************************************************************************/	
	generate eventyr = year(obsdate)
	count if eventyr == yob /*22*/
	drop  if eventyr == yob // drop if test result is in the same year as patient born

	* make an age at event
	gen ageAtEvent = 0
	replace ageAtEvent = eventyr - yob - 1 if obsdate < mdy(07,01,eventyr) // round down if eventdate in first half of year
	replace ageAtEvent = eventyr - yob if obsdate >= mdy(07,01,eventyr)	

	/**************************************************************************
	#B3. Deal with duplicate records
	**************************************************************************/
	*drop enterdate and medcodeid so the only same day duplicates are those with different values for data2
	drop enterdate medcodeid 
	duplicates drop
	
	/**************************************************************************
	#B4. Calculate eGFR
	**************************************************************************/
	* calculate egfr using ckd-epi
	* first multiply by 0.95 (for assay - fudge factor) and divide by 88.4 (to convert umol/l to mg/dl)
	* DN "fudge factor"
	gen SCr_adj = (SCr * 0.95) / 88.4

	gen min=.
	replace min = SCr_adj / 0.7 if gender == 2
	replace min = SCr_adj / 0.9 if gender == 1
	replace min = min^-0.329 if gender == 2
	replace min = min^-0.411 if gender == 1
	replace min = 1 if min < 1

	gen max=.
	replace max = SCr_adj / 0.7 if gender == 2
	replace max = SCr_adj / 0.9 if gender == 1
	replace max = max^-1.209
	replace max = 1 if max > 1

	gen egfr = min * max * 141
	replace egfr = egfr * (0.993^ageAtEvent)
	replace egfr = egfr * 1.018 if gender == 2
	label var egfr "egfr calculated using CKD-EPI formula with no eth + fudge"
	
	* categorise into ckd stages
	egen egfr_cat = cut(egfr), at(0, 15, 30, 45, 60, 5000)
	label define EGFR 0"stage 5" 15"stage 4" 30"stage 3b" 45"stage 3a" 60"no CKD"
	label values egfr_cat EGFR
	label var egfr_cat "eGFR category calc without eth + DN fudge factor"
	
	* * recode with appropriate category as reference
	recode egfr_cat 0 = 5 15 = 4 30 = 3 45 = 2 60 = 0, generate(ckd)
	label define ckd 0"no CKD" 2"stage 3a" 3"stage 3b" 4"stage 4" 5"stage 5"
	label values ckd ckd
	label var ckd "CKD stage calc without eth + DN fudge factor"
	
	/*
	CKD STAGES
		stage 2 and below: eGFR >=60 / missing
		stage 3a: eGFR 45-59
		stage 3b: eGFR 30-44
		stage 4: eGFR 15-29
		stage 5: eGFR <15)

	Low eGRF = bad
	High eGFR = good
	*/
	
/*come back if necessary, check if ethnicity necessary 	
	if "copd_Patient_ethnicity.dta"!="" {
		* use ethnicity to calculate eGFR if the ethnicityfile option is
		* specified
		merge m:1 patid using "copd_Patient_ethnicity.dta", keep(match master) nogen
		generate egfr_eth=egfr*1.159 if eth5==2	// recalculate egfr for those with black ethnicity
		replace egfr_eth=egfr if eth5!=2 & eth5!=. & eth5!=5	// set this variable to previous value if ethnicity other than black and ethnicity not either missing or not stated
		label var egfr_eth "egfr calculated using CKD-EPI formula with ethnicity"
	
		* categorise into ckd stages
		egen egfr_cat_eth= cut(egfr_eth), at(0, 15, 30, 45, 60, 5000)
		label values egfr_cat_eth EGFR
		label var egfr_cat_eth "eGFR category calc with ethnicity"
		
		* recode with appropriate category as reference
		recode egfr_cat_eth 0=5 15=4 30=3 45=2 60=0, generate(ckd_eth)
		label values ckd_eth ckd
		label var ckd_eth "CKD stage calculated with ethnicity"
	}
*/

	* save	
	label data "serum creatinine records and eGFR results from CPRD"
	notes: prog_getScr.do / TS
	compress
	save "copd_Observations_egfr", replace

drop if eventdate > td(01mar2020)
drop pracid term obsdate rangeTo rangeFrom obstypeid unit description max min SCr_adj ageAtEvent ckd
sort patid eventdate
bysort patid (eventdate): gen n = _N
gen kidney_impair = 1 if egfr < 60
replace kidney_impair = 0 if kidney_impair ==.
by patid : egen kidney = max(kidney_impair)

drop if kidney == 0
bysort patid (eventdate): gen first = _n
keep if first == 1
keep eventdate patid
rename eventdate kidney_date

save "${file_stub}_covariate_kidney", replace

cap log close
clear all

