/*=========================================================================
DO FILE NAME:		    01cr_copd_file_import.do

AUTHOR:					Marleen Bokern, adapted from Jeremy Brown, Helena Carreira 

VERSION:				v1

DATE VERSION CREATED: 	10/2022

DATASETS CREATED:       copd_Extract_Observation_xx
						copd_Extract_DrugIssue_xx
						copd_Extract_Patient_1
						copd_Extract_Practice_1
						
DESCRIPTION OF FILE:	checks all files are unzipped, imports txt file, formats some variables and saves as dta

*=========================================================================*/

/***********************************************************************************************
>> HOUSEKEEPING
************************************************************************************************/
clear all

capture log close
log using $Logdir/01cr_copd_file_import.log, replace

macro list _all

** Instructions: to use specify directory containing unzipped aurum files, file name stubs, and number of files expected
// Specify file names
glob file_stub 			= 	"copd"
glob file_Patient 		= 	"${file_stub}_Extract_Patient_"
glob file_Practice 		=	"${file_stub}_Extract_Practice_"
glob file_Staff			=	"${file_stub}_Extract_Staff_"
glob file_Consultation 	= 	"${file_stub}_Extract_Consultation_"
glob file_Observation	= 	"${file_stub}_Extract_Observation_"
glob file_Referral 		= 	"${file_stub}_Extract_Referral_"
glob file_Problem 		= 	"${file_stub}_Extract_Problem_"
glob file_DrugIssue		= 	"${file_stub}_Extract_DrugIssue_"

// Specify number of different files
glob no_Patient = 1
glob no_Practice = 1
glob no_Staff = 1
glob no_Consultation = 12
glob no_Observation = 48
glob no_Referral = 1
glob no_Problem = 1
glob no_DrugIssue = 49

***zip files were extracted manually.
/*******************************************************************************
>> check all files are there
*******************************************************************************/

// Specify directory containing file names
cd $Copd_aurum_extract

// Check presence of files
loc valid_check = 1
foreach table_name in "Patient" "Practice" "Staff" "Consultation" "Observation" "Referral" "Problem" "DrugIssue" {
    forvalues i = 1/${no_`table_name'} {
	    loc file_name: di "marleen_${file_`table_name'}" %03.0f (`i') ".txt"
	    capture confirm file "`file_name'"
		if _rc != 0  {
		    di "`file_name' not found"
			local valid_check = 0
		}
	}
} 

// Provide feedback on check success or failure
qui {
    if `valid_check' == 1 {
     noi di "All files found"
	} 
	else {
	 noi di "Not all files found"
	}
}

***all files found
***********************************************************************************************
/******************************************************************************************************
>> Convert all necessary files to dta, rename, reformat, and drop variables where necessary
*******************************************************************************************************/

cd $Copd_aurum_extract

/******************************************************************************************************
>> CONVERT OBSERVATION FILES
****import text files
****drop staffid, consid, obsid, parentobsid, probobsid
****reformat date variables
****drop observations after the end of follow-up
****summarize missing values
****compress and save as dta file
*******************************************************************************************************/
foreach file of numlist 1/9 {
	noi di "Converting Observation, File `file'"
    import delimited using "marleen_${file_stub}_Extract_Observation_00`file'.txt", clear stringcols(_all)
	drop staffid consid obsid parentobsid probobsid
	g obsdate1 = date(obsdate, "DMY")
	g enterdate1 = date(enterdate,"DMY")
	gen eventdate = obsdate1
	replace eventdate = enterdate1 if eventdate ==.
	format (obsdate1 enterdate1 eventdate) %td
	drop obsdate enterdate 
	rename (enterdate1 obsdate1) (enterdate obsdate)
	drop if obsdate >= td(01may2021) & obsdate !=.
    misstable summarize patid pracid obsdate enterdate eventdate medcodeid
	compress
	save "${file_stub}_Extract_Observation_`file'.dta", replace
}

foreach file of numlist 10/$no_Observation {
	noi di "Converting Observation, File `file'"
    import delimited using "marleen_${file_stub}_Extract_Observation_0`file'.txt", clear stringcols(_all)
	drop staffid consid obsid parentobsid probobsid
	g obsdate1 = date(obsdate, "DMY")
	g enterdate1 = date(enterdate,"DMY")
	gen eventdate = obsdate1
	replace eventdate = enterdate1 if eventdate ==.
	format (obsdate1 enterdate1 eventdate) %td
	drop obsdate enterdate 
	rename (enterdate1 obsdate1) (enterdate obsdate)
	drop if obsdate >=td(01may2021) & obsdate !=.
    misstable summarize patid pracid obsdate enterdate eventdate medcodeid
	compress
	save "${file_stub}_Extract_Observation_`file'.dta", replace
}

/******************************************************************************************************
>> CONVERT DRUG ISSUE FILES
****import text files
****drop staffid, probobsid, issueid, drugrecid
****reformat date variables
****destring numerical variables
****drop observations after the end of follow-up
****summarize missing values
****compress and save as dta file
*******************************************************************************************************/
foreach file of numlist 1/9 {
	noi di "Converting Drug Issue, File `file'"
    import delimited using "marleen_${file_stub}_Extract_DrugIssue_00`file'.txt", stringcols(_all) clear
	drop issueid probobsid drugrecid staffid
	g issuedate1 = date(issuedate, "DMY")
	g enterdate1 = date(enterdate,"DMY")
	format (issuedate1 enterdate1) %td
	drop issuedate enterdate 
	rename (enterdate1 issuedate1) (enterdate issuedate)
	destring quantity duration, replace
	drop if issuedate >= td(01may2021) & issuedate !=.
    misstable summarize patid pracid issuedate enterdate prodcodeid dosageid quantity quantunitid duration
	compress
	save "${file_stub}_Extract_DrugIssue_`file'.dta", replace
}

foreach file of numlist 10/$no_DrugIssue {
	noi di "Converting Drug Issue, File `file'"
    import delimited using "marleen_${file_stub}_Extract_DrugIssue_0`file'.txt", stringcols(_all) clear
	drop issueid probobsid drugrecid staffid
	g issuedate1 = date(issuedate, "DMY")
	g enterdate1 = date(enterdate,"DMY")
	format (issuedate1 enterdate1) %td
	drop issuedate enterdate 
	rename (enterdate1 issuedate1) (enterdate issuedate)
	destring quantity duration, replace
	drop if issuedate >= td(01may2021) & issuedate !=.
    misstable summarize patid pracid issuedate enterdate prodcodeid dosageid quantity quantunitid duration
	compress
	save "${file_stub}_Extract_DrugIssue_`file'.dta", replace
}

/******************************************************************************************************
>> CONVERT PATIENT FILE
****import text file
****drop usualgpstaffid, acceptable
****reformat date variables
****summarize missing values
****compress and save as dta file
*******************************************************************************************************/
noi di "Converting Patient"
import delimited using "marleen_${file_stub}_Extract_Patient_001.txt", clear stringcols(_all)
drop usualgpstaffid acceptable
destring yob mob gender, replace
g regstart = date(regstartdate, "DMY")
g emis_death = date(emis_ddate,"DMY")
g deathdate = date(cprd_ddate,"DMY")
g regend = date(regenddate,"DMY")
format (regstart emis_death deathdate regend) %td
drop regenddate regstartdate
misstable summarize 
compress
save "${file_stub}_Extract_Patient_1.dta", replace

/******************************************************************************************************
>> CONVERT PRACTICE FILE
****import text file
****drop uts, region
****reformat date variables
****compress and save as dta file
*******************************************************************************************************/
noi di "Converting Practice"
import delimited using "marleen_${file_stub}_Extract_Practice_001.txt", clear stringcols(_all)
drop uts region
g lcd1 = date(lcd, "DMY")
format lcd1 %td
drop lcd
rename lcd1 lcd
compress
save "${file_stub}_Extract_Practice_1", replace


clear all

log close