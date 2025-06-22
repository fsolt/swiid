set more off
clear

cd "~/Documents/Projects/swiid/"
use "data/for_stata.dta", clear

gen gini_disp=.
gen gini_mkt=.
replace redist=0 if redist==.

order country year gini_disp gini_disp* gini_mkt gini_mkt*

mi import wide, imputed(gini_disp=gini_disp_1-gini_disp_100 gini_mkt=gini_mkt_1-gini_mkt_100) clear
mi xtset, clear
drop gini_disp_1-gini_disp_100 gini_mkt_1-gini_mkt_100 
sort country year

mi passive: gen rel_red=(gini_mkt-gini_disp)*100/gini_mkt if redist 
mi passive: gen abs_red=gini_mkt-gini_disp if redist 
drop redist

order country year gini_disp _*gini_disp gini_mkt _*gini_mkt abs_red _*abs_red rel_red _*rel_red 
sort country year
label data "SWIID v9.9, June 2025. Refer to the stata_swiid.pdf file for usage instructions." 
saveold "data/swiid9_9.dta", replace

set more on
