cd "/Users/fredsolt/Documents/Projects/swiid/vignette"

// Get GDP per capita data from the Penn World Tables, Version 9.1 (Feenstra et al. 2015)
// download from https://www.rug.nl/ggdc/docs/pwt91.dta
// create gdppc and save as .dta

use pwt91.dta, clear
gen gdppc = rgdpe/pop/1000
drop if gdppc==.
keep country year gdppc
save pwt91_gdppc.dta, replace


// Get World Values Survey 7-wave data 
// from http://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp
// generate variables of interest, merge in the PWT data, and save

use WVS_Longitudinal_1981_2016_stata_v20180912.dta, clear
kountry S003, from(iso3n)
rename NAMES_STD country
gen year = S020
gen country_year = S025 
gen religiosity = F063 if F063>0
gen age = X003 if X003>0
gen educ = X025 if X025>0
gen male = (X001 == 1) if X001>0
keep country year country_year religiosity male educ age
merge m:1 country year using pwt91_gdppc.dta
drop if _merge!=3
drop _merge
save wvs_pwt.dta, replace

// Now merge these data *into* the SWIID
use "../data/swiid9_0_vig.dta", clear

merge 1:m country year using wvs_pwt.dta
drop if _merge!=3
drop _merge

mi estimate: mixed religiosity gini_disp gdppc age educ male || country: || country_year:

//import delimited "../data/swiid_summary.csv", clear
insheet using "/Users/fredsolt/Documents/Projects/swiid/data/swiid_summary.csv", comma clear
 
// Calculate the bounds of the 95% uncertainty intervals
gen gini_disp_95ub = gini_disp + 1.96*gini_disp_se
gen gini_disp_95lb = gini_disp - 1.96*gini_disp_se

// A silly example
gen name_length = length(country)
gen first_letter = substr(country, 1, 1)
keep if year==2010 & first_letter=="S" /*2010 for Senegal, Serbia, . . .*/

// A scatterplot with 95% uncertainty intervals
twoway rspike gini_disp_95ub gini_disp_95lb name_length, lstyle(ci) || ///
    scatter gini_disp name_length, msize(small) ///
    legend(order(2 "SWIID Disposable-Income Inequality")) 

g graph export "/Users/fredsolt/Documents/Projects/swiid/vignette/stata_scatter.png", as(png) name("Graph") replace

