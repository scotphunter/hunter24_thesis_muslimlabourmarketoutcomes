** ************************************************************************** **
	* CHAPTER 5 ANALYSIS 1 - EXPLORING INTERSECTIONAL CONTEXTS 
	* Part of PhD research on assessing impact of contemporary migration 
	* patterns on the Muslim community in the UK. 
	* Last updated 2 Apr 2024
	* Scot Hunter 
** ************************************************************************** **


*** This is a replication of Evans et al. 2024 analysis using UKHLS data
* https://www.sciencedirect.com/science/article/pii/S235282732400065X?via%3Dihub

* DOI: https://doi.org/10.1016/j.ssmph.2024.101664

********************************************************************************
**# General Setup
********************************************************************************


* Load in UKHLS pooled data:  

use $ptemp/file8.dta, clear

use pidp fem ff_religion dvage_ind ethn_dv employment  ///
bornuk_dv gor_dv mar hiqual_dv educscore engprof pubsector parttime ///
jbstat fimngrs_dv jbhrs jshrs health cardum child ties liberal religiosity using $ptemp/file8.dta, clear

* Limit the sample to one observation per person per job 65 under
keep if dvage_ind < 65
egen personjob = tag(pidp)
keep if personjob
drop personjob 

rename religiosity sity 

* Limit the sample to employees/self-employed 

keep if employment == 1

tab liberal 
recode liberal . = 0


* Prepare dependent variable: average log hourly earnings 

* Combine employee and self-employed weekly work hours 
replace jbhrs = jshrs if jbhrs == .
* Generate hourly pay 
capture drop hrpay 
gen hrpay = fimngrs_dv / jbhrs 
* Generate log version 
gen temp = 1 + hrpay
gen temp2= log(temp)
gen ln_hrpay  = temp2 + 1
drop temp temp2 jshrs jbhrs  fimngrs_dv
lab var ln_hrpay "Average Log Hourly Earnings"


* Prepare strata variables 
tab ethn_dv
clonevar eth2 = ethn_dv 
recode eth2 1/3 = 1 4 = 7 9 = 2 10 = 3 11 = 4 14 15 16 = 5 5/8 12 13 17 97 = 6 
label define a_ethn_dv  2 "Indian" 3 "Pakistani" 4 "Bangladeshi" 5 "Black" 6 "Other eth. " 1 "White British" 7 "White Other", modify
drop ethn_dv 
lab var eth2 "Ethnic Group"

tab ff_religion eth2 , m
replace ff_religion = 0 if ff_religion == . 
capture drop reli
gen reli = ff_religion 
recode reli 0=1 2=2 12=3 13=4 14 15 16=97 
recode reli 97 = 5
lab var reli "Religion"
label define reli 1 "No religion" 2 "Christian" 3 "Muslim" 4 "Hindu" 5 "Oth. religion"
label values reli reli 
drop ff_religion

tab reli eth2, m 
replace reli = 3 if eth2 == 3 & reli == 5
replace reli = 3 if eth2 == 3 & reli == 2 
replace reli = 3 if eth2 == 4 & reli == 2


tab bornuk_dv 
capture drop ukborn 
gen ukborn =(bornuk_dv == 1)
recode ukborn 0 = 2 
label define ukborn 1 "Born in the UK" 2 "Not born in the UK"
drop bornuk_dv 
lab var ukborn "Country of Birth"
label values ukborn ukborn 

capture drop agecats 
gen agecats = dvage_ind 
recode agecats 16/27 = 1 28/39 = 2 40/51 = 3 52/64 = 4
label define age 1 "16 - 27" 2 "28 - 39" 3 "40 - 51" 4 "52 - 64"
label values agecats age
lab var agecats "Age categories"

* Education 
codebook hiqual_dv
label define a_hiqual_dv 6 "no quals.", modify
tab hiqual_dv 
capture drop quals 
gen quals = hiqual_dv 
recode quals 1 2 = 1 3 4 = 2 5 = 3 6= 4 
label define quals 1 "Degree or other higher" 2 "A level/GCSE" 3 "Other quals" 4 "No quals"
label values quals quals
lab var quals "Highest qual held"
capture drop degree 
gen degree = hiqual_dv
recode degree 1 2 = 1 3 4 5 6 = 2
tab degree hiqual_dv
label define degree 1 "Degree" 2 "Below degree"
label values degree degree

* Prepare controls 
tab1 fem pubsector parttime gor_dv
gen lonse = (gor_dv == 7 | gor_dv == 8)
lab var lonse "Region" 
label define lo 1 "London/SE Eng." 0 "Rest of UK"
label values lonse lo
drop gor_dv

su dvage_ind educscore 
lab var dvage_ind "Age"
recode fem 1 = 2 0 =1
label define fem 2 "Female" 1 "Male"
label values fem fem
lab var fem "Sex"

lab var mar "Marital Status"
lab define mar 1 "Married or Cohabitating" 0 "Single"
label values mar mar 

lab var educscore "Education Score"

lab var pubsector "Occupation Sector"
label define pubsector 1 "Public" 0 "Private"
label val pubsector pubsector

lab var parttime "Work Orientation"
label define par 1 "Works part-time" 0 "Works Full-time"
label val parttime par 

* Generate missing values variable 
capture drop miss 
egen miss = rmiss(ln_hrpay eth2 reli ukborn fem dvage_ind educscore pubsector parttime lonse mar)
codebook ln_hrpay eth2 reli ukborn fem dvage_ind educscore pubsector parttime lonse mar if miss == 0, compact 

* keep observations with valid information across all variables 
keep if miss == 0 
drop if ln_hrpay < 0
drop miss 

capture drop health2 
gen health2=(health==1)
capture drop health
rename health2  health 
 
********************************************************************************
**# Generate Stratum 
********************************************************************************

* Generate the stratum ID first just interacting ethnicity, religion and cob.
capture drop stratum
generate stratum = 1000*fem + 100*eth2 + 10*reli + 1*ukborn 

* Move stratum to the beginning of the dataset
order stratum

* Tabulate stratum
tab stratum

* Generate a new variabe which records stratum size
capture drop n
bysort stratum: generate n = _N

* Generate a new variabe which tags one record for each startum
capture drop s_tag
egen s_tag = tag(stratum)

* List stratums 
list fem eth2 reli ukborn stratum n if s_tag

sort n 
list fem eth2 reli ukborn stratum n if s_tag


********************************************************************************
**# Estimate Linear Model 1A - Null 
********************************************************************************

* Fit the two-level linear regression with no covariates
mixed ln_hrpay || stratum:

* Save the model results
estimates save "m1A_v2.ster", replace

* Store the between-stratum variance
scalar m1Asigma2 = exp(_b[lns1_1_1:_cons])^2
// The mixed command estimates and returns the between-stratum variance as the 
// log of the SD of the stratum random effects. We must therefore exponentiate 
// and square this quantity to recover the between-stratum variance.

* Predict the mean outcome
predict m1Am, fitted
// Calculated as the fixed-portion linear prediction plus contributions based on 
// predicted random effects.

********************************************************************************
**# Estimate Linear Model 8B - Just Strata 
********************************************************************************

* Fit the two-level linear regression with covariates
mixed ln_hrpay  i.fem i.eth2 i.reli i.ukborn || stratum:, baselevels

* Save the model results
estimates save "m1B_v2.ster", replace

* Store the between-stratum variance
scalar m1Bsigma2 = exp(_b[lns1_1_1:_cons])^2
// The mixed command estimates and returns the between-stratum variance as the 
// log of the SD of the stratum random effects. We must therefore exponentiate 
// and square this quantity to recover the between-stratum variance.

* Predict the mean outcome
predict m1Bm, fitted
// Calculated as the fixed-portion linear prediction plus contributions based on 
// predicted random effects.

* Predict the stratum random effect and its SE
predict m1Bu, reffect reses(m1Buse)

* Predict the standard error of the fixed-portion linear prediction
predict m1Bxbse, stdp

********************************************************************************
**# Estimate Linear Model 8B - Just Strata & controls
********************************************************************************

* Fit the two-level linear regression with covariates
mixed ln_hrpay  i.fem i.eth2 i.reli i.ukborn  ///
dvage_ind educscore engprof lonse mar child health cardum ///
ties liberal sity parttime pubsector || stratum:, baselevels

* Save the model results
estimates save "m1C_v2.ster", replace

* Store the between-stratum variance
scalar m1Csigma2 = exp(_b[lns1_1_1:_cons])^2
// The mixed command estimates and returns the between-stratum variance as the 
// log of the SD of the stratum random effects. We must therefore exponentiate 
// and square this quantity to recover the between-stratum variance.

* Predict the mean outcome
predict m1Cm, fitted
// Calculated as the fixed-portion linear prediction plus contributions based on 
// predicted random effects.

* Predict the stratum random effect and its SE
predict m1Cu, reffect reses(m1Cuse)

* Predict the standard error of the fixed-portion linear prediction
predict m1Cxbse, stdp


compress

* Save the data
save "individual_v2.dta", replace

*-------------------------------------------------------------------------------
**## Save the data at the stratum-level
*-------------------------------------------------------------------------------

collapse (count) n = ln_hrpay (mean) ln_hrpay , ///
by(stratum fem eth2 reli ukborn lonse dvage_ind educscore engprof ///
  mar child health cardum parttime pubsector ties liberal sity ///
   m1Am m1Bm m1Cm m1Bxbse m1Cxbse m1Bu m1Cu m1Buse m1Cuse) 

capture drop N 
bysort stratum: generate N = _N

* Move the list of variables to the beginning of the dataset
order stratum N fem  eth2 reli ukborn lonse dvage_ind educscore ///
 mar parttime pubsector ln_hrpay
  
 capture drop s_tag 
 egen s_tag = tag(stratum)
 keep if s_tag 

* Set the display format to 2dp
format %9.2f ln_hrpay m1Am m1Bm m1Cm m1Bxbse m1Cxbse m1Bu m1Cu m1Buse m1Cuse

* Compress the data 
compress 

* Save the data
save "stratum_v2.dta", replace

********************************************************************************
**# Table 1 + Table 2
********************************************************************************

*-------------------------------------------------------------------------------
**## Calculate individual-level descriptive statistics
*-------------------------------------------------------------------------------

* Load the individual-level data
use "individual_v2.dta", clear

* Total sample size
codebook stratum, compact

* Summarize the individual outcome
tabstat ln_hrpay, statistics( mean p50 sd min max ) format(%3.2f)
//[mean of the individual outcomes = "sample mean"]


* Table 1 - Descriptive stats. 
table (var), ///
stat(fvfrequency i.fem) stat(fvpercent i.fem) ///
stat(fvfrequency i.eth2) stat(fvpercent i.eth2) ///
stat(fvfrequency i.reli) stat(fvpercent i.reli) ///
stat(fvfrequency i.ukborn) stat(fvpercent i.ukborn) ///
stat(fvfrequency i.mar) stat(fvpercent i.mar) ///
stat(fvfrequency i.lonse) stat(fvpercent i.lonse) ///
stat(fvfrequency i.parttime) stat(fvpercent i.parttime) ///
stat(fvfrequency i.pubsector) stat(fvpercent i.pubsector) ///
stat(count dvage_ind) stat(mean dvage_ind) stat(median dvage_ind)  stat(sd dvage_ind) ///
stat(min dvage_ind) stat(max dvage_ind)  ///
stat(count educscore) stat(mean educscore) stat(median educscore) stat(sd educscore) ///
stat(min educscore) stat(max educscore) ///
stat(count ln_hrpay) stat(mean ln_hrpay) stat(median ln_hrpay) stat(sd ln_hrpay) stat(min ln_hrpay) stat(max ln_hrpay) nformat(%9.0fc fvfrequency) nformat(%3.1f fvpercent) nformat(%3.2f mean sd median min max) sformat("%s%%" fvpercent) sformat("(%s)" sd)
collect recode result fvfrequency = column1 fvpercent = column3 count = column1 mean = column2 median = column3 sd = column4 min = column5 max = column6
collect layout (var) (result[column1 column2 column3 column4 column5 column6 column7])
collect style cell var#result[column3], nformat(%3.1f)
collect style cell var[ln_hrpay]#result[column2], nformat(%3.2f) 
collect style cell var[ln_hrpay]#result[column3], nformat(%3.2f)
collect style cell var[ln_hrpay]#result[column4], nformat(%3.2f)
collect style cell var[ln_hrpay]#result[column5], nformat(%3.2f)
collect style cell var[ln_hrpay]#result[column6], nformat(%3.2f)
collect style cell var[educscore]#result[column2], nformat(%3.2f) 
collect style cell var[educscore]#result[column3], nformat(%3.2f)
collect style cell var[educscore]#result[column4], nformat(%3.2f)
collect style cell var[educscore]#result[column5], nformat(%3.2f)
collect style cell var[educscore]#result[column6], nformat(%3.2f)
collect style cell var[dvage_ind]#result[column2], nformat(%3.2f) 
collect style cell var[dvage_ind]#result[column3], nformat(%9.0fc)
collect style cell var[dvage_ind]#result[column4], nformat(%9.0fc)
collect style cell var[dvage_ind]#result[column5], nformat(%9.0fc)
collect style cell var[dvage_ind]#result[column6], nformat(%9.0fc)
collect style header result, level(hide)
collect preview
collect style row stack, nobinder spacer
collect style cell border_block, border(right, pattern(nil))
collect preview
collect export table1.docx, replace 

sum engprof child health cardum

*-------------------------------------------------------------------------------
**## Calculate stratum-level descriptive statistics
*-------------------------------------------------------------------------------

* Load the stratum-level data
use "stratum_v2.dta", clear

su N, detail
* Generate binary indicators for whether each stratum has more than X 
* individuals
gen n100plus =    (N >= 100)
gen n50plus  =    (N >= 50)
gen n30plus  =    (N >= 30)
gen n20plus  =    (N >= 20)
gen n10plus  =    (N >= 10)
gen nlessthan10  = (N < 10)

tabulate n100plus
tabulate n50plus
tabulate n30plus
tabulate n20plus
tabulate n10plus
tabulate nlessthan10



table var, ///
stat(fvfrequency i.n100plus) stat(fvpercent i.n100plus)  ///
stat(fvfrequency i.n50plus) stat(fvpercent i.n50plus)  ///
stat(fvfrequency i.n30plus) stat(fvpercent i.n30plus) ///
stat(fvfrequency i.n20plus) stat(fvpercent i.n20plus) ///
stat(fvfrequency i.n10plus) stat(fvpercent i.n10plus) ///
stat(fvfrequency i.nlessthan10) stat(fvpercent i.nlessthan10)  

collect recode result fvfrequency = column1 fvpercent = column2 
collect layout (var) (result[column1 column2])
collect style cell var#result[column2], nformat(%3.1f)  sformat("%s%%")
collect style header result, level(hide)
collect preview

collect export table2.docx, replace 


* Summarize the observed stratum means
tabstat ln_hrpay, statistics( mean p50 sd min max ) format(%3.2f)
//[mean of the observed stratum means = "grand mean"]

* Summarize the predicted stratum means
tabstat m1Am, statistics( mean p50 sd min max ) format(%3.2f)
//[mean of the predicted stratum means = "precision weighted grand mean"]



********************************************************************************
**# Model Results
********************************************************************************

* Load the individual-level data
use "individual_v2.dta", clear


*-------------------------------------------------------------------------------
**## Model 1A
*-------------------------------------------------------------------------------

* Load the estimation results and make them the current (active) results
estimates use "m1A_v2.ster"

* Replay the estimation results
estimates replay, baselevels
est store m1a

* Calculate the variance partition coefficient (VPC)
estat icc
estadd scalar icc_ = r(icc2) 




*-------------------------------------------------------------------------------
**## Model 1B
*-------------------------------------------------------------------------------

* Load the estimation results and make them the current (active) results
estimates use "m1B_v2.ster"

* Replay the estimation results
estimates replay, baselevels
est store m1b

* Calculate the variance partition coefficient (VPC)
estat icc
estadd scalar icc_ = r(icc2) 




* Calculate the Proportional Change in Variance (PCV) (as a percentage)
display %3.1f 100*(m1Asigma2 - m1Bsigma2) / m1Asigma2

scalar m1Bpcv = 100 * (m1Asigma2 - m1Bsigma2) / m1Asigma2

*-------------------------------------------------------------------------------
**## Model 1C
*-------------------------------------------------------------------------------

* Load the estimation results and make them the current (active) results
estimates use "m1C_v2.ster"

* Replay the estimation results
estimates replay, baselevels
est store m1c

* Calculate the variance partition coefficient (VPC)
estat icc
estadd scalar icc_ = r(icc2) 

* Calculate the Proportional Change in Variance (PCV) (as a percentage)
display %3.1f 100*(m1Asigma2 - m1Csigma2) / m1Asigma2

scalar m1Bpcv = 100 * (m1Asigma2 - m1Csigma2) / m1Asigma2



* Estimates table 

esttab m1a m1b m1c using $poutputs/modelsm8.csv, ///
stat(N icc_, label("Obs." "VPC") fmt(%8.0f %8.2f)) ///
 collabels(Estimates [95%CI] )  b(%8.2f) ///
 baselevels ci(%8.2f) star(* 0.05) label wide replace 
 
 esttab m1a m1b m1c, refcat(1.fem {bf:Sex} 1.eth2 {bf:Ethnicity} ///
	1.ff_religion {bf:Religion} 1.ukborn {bf:Country} ///
	1.degree {bf:Education} 1.agecats {bf:Age} ///
	0.parttime {bf:Parttime}, nolabel ) ///
stat(N icc_, label("Obs." "VPC") fmt(%8.0f %8.2f)) ///
 collabels(Estimates [95%CI] )  b(%8.2f) ///
 baselevels ci(%8.2f) star(* 0.05) label wide replace 



//  ///
// 	refcat(1.fem {bf:Sex} 1.eth2 {bf:Ethnicity} ///
// 	1.ff_religion {bf:Religion} 1.ukborn {bf:Country} ///
// 	1.degree {bf:Education} 1.agecats {bf:Age} ///
// 	0.parttime {bf:Parttime}, nolabel ) label ///
// 	order(_cons)  b(%8.2f) collabels(Estimates [95%CI] ) ///
// 	stat(N icc_, label("Obs." "VPC") fmt(%8.0f %8.2f)) star(* 0.05) ///
// 	baselevels ci(%8.2f)  mtitles wide replace 

	
********************************************************************************
**# Figure 1 - Means 
********************************************************************************	

* G1

* Load the individual-level data
use "individual_v2.dta", clear

* Plot the histogram of the individual outcome
histogram ln_hrpay
* Summarize HbA1c
summarize ln_hrpay

* Store the sample mean of HbA1c
scalar mean = round(r(mean), 0.01)

* Store the sample minimum
scalar min = round(r(min), 0.01)

* Store the sample maximum
scalar max = round(r(max), 0.01)

* Re-plot the histogram of the individual outcome with options
histogram ln_hrpay, ///
	width(0.2) start(0.26) percent ///
	addplot((pci 0 `=mean' 17 `=mean', lcolor(lime) lwidth(thick))) ///
	text(18  `=mean'  "Sample Mean = `=mean'", ///
	placement(0) ) ///
	text(6 `=min' "Min =") ///
	text(4 `=min' "`=min'", ) ///
	text(6 `=max' "Max = " ) ///
	text(4 `=max' "`=max'", ) /// 
	ytitle("Percent of Individuals",) ///
	xtitle("Average Log Hourly Earnings") ///
	xlabel(0(1)12) ///
	ylabel(0(5)20) ///
	legend(off) ///
	scheme(s1mono) ///
	name(Figure1A, replace) ///
	xsize(5) title("{bf: (a) Sample Distribution (Individual Observations of Average Log Hourly Earnings)}" , size(3))
	
graph save $pgraphs/G1_v2.gph, replace 

* G2

* Load the stratum-level data
use "stratum_v2.dta", clear

* Plot the histogram of the observed stratum means
histogram ln_hrpay

* Summarize the observed stratum means
summarize ln_hrpay

* Store the grand mean (the mean of the observed stratum means)
scalar mean = round(r(mean), 0.01)

* Store the minimum observed stratum mean
scalar min = round(r(min), 0.01)

* Store the maximum observed stratum mean
scalar max = round(r(max), 0.01)

* Re-plot the histogram of the observed stratum means with options
histogram ln_hrpay, ///
  percent ///
	addplot((pci 0 `=mean' 35 `=mean', lcolor(lime) lwidth(thick))) ///
	text(36  `=mean' "Grand Mean = `=mean'", ///
		placement(0)) ///
	text(6 `=min' "Min =") ///
	text(3.5 `=min' "`=min'", ) ///
	text(6 `=max' "Max = " ) ///
	text(3.5 `=max' "`=max'", ) /// 
	ytitle("Percent of Strata", ) ///
	legend(off) xtitle("Log Average Hourly Earnings") ///
	scheme(s1mono) xlabel(2.50(1)6.50) ylabel(0(5)40) ///
	name(Figure1B, replace) ///
	xsize(5) title("{bf: (b) Distribution of Strata Mean Values (observed)}", size(3))

* Export the graph as a portable network graphics (PNG) file
graph save $pgraphs/G2_v2.gph, replace 

* G3

* Load the stratum-level data
use "stratum_v2.dta", clear

* Plot the histogram of the predicted stratum means
histogram m1Am

* Summarize the predicted stratum means
summarize m1Am

* Store the precision weighted grand mean (the mean of the predicted stratum 
* means)
scalar mean = round(r(mean), 0.01)

* Store the minimum predicted stratum mean
scalar min = round(r(min), 0.01)

* Store the maximum predicted stratum mean
scalar max = round(r(max), 0.01)

* Re-plot the histogram of the predicted stratum means with options
histogram m1Am, ///
	percent ///
	addplot((pci 0 `=mean' 37 `=mean', lcolor(lime) lwidth(thick))) ///
	text(39 `=mean' "Percission Weighted Grand Mean = `=mean'", ///
		placement(0)) ///
	text(10 `=min' " Min =") ///
	text(7  `=min' " `=min'") ///
	text(15 5.24 " Max = ") ///
	text(12  5.24 " 5.10") ///
	ytitle("Percent of Strata", ) xlabel(2.50(1)6.50) ylabel(0(5)40) ///
	 legend(off) xtitle("Log Average Hourly Earnings") ///
	scheme(s1mono)  ///
	name(Figure1C, replace) ///
	xsize(5) title("{bf: (c) Distribution of Strata Mean Values (predicted from Model 1A)}", size(3))
	
graph save $pgraphs/G3_v2.gph, replace 

graph combine $pgraphs/G1_v2.gph $pgraphs/G2_v2.gph $pgraphs/G3_v2.gph, row(3) iscale(*0.75) ysize(10)
graph save $pgraphs/distributions_v2.gph, replace 

*-------------------------------------------------------------------------------
**## Figure 2 Panel A & Table 
*-------------------------------------------------------------------------------

* Load the stratum-level data
use "stratum_v2.dta", clear

* Rank the predicted stratum means
egen m1Bmrank = rank(m1Bm)

* Generate the lower and upper limits of the approximate 95% confidence 
* intervals for the predicted stratum means
generate m1Bmlo = m1Bm - 1.96 * sqrt(m1Cxbse^2 + m1Cuse^2)
generate m1Bmhi = m1Bm + 1.96 * sqrt(m1Cxbse^2 + m1Cuse^2)
// Approximate as the model assumes no sampling covariability between the 
// regression coefficients and the stratum random effect

* Plot the caterpillar plot of the predicted stratum means

capture drop h_mus
egen h_mus = max(reli == 3), by(stratum)

twoway ///
	(rspike m1Bmhi m1Bmlo m1Bmrank, lcolor(gs4)) /// 
	(rspike m1Bmhi m1Bmlo m1Bmrank if h_mus == 1, lcolor(lime) lwidth(thick) ) ///
	(scatter m1Bm m1Bmrank, mcolor(black) msymbol(smcircle) msize(tiny)) ///
	(scatter m1Bm m1Bmrank if h_mus == 1, mcolor(lime) msymbol(smcircle) msize(tiny)) ///
	, ///
	ytitle("{bf:(A) Predicted Average Log Hourly}" "{bf: Earnings Model 1B}", size(*1.2)) xtitle("Stratum rank") ///
	xlabel(0 50 100 115) ///
	legend(off) ylabel(4(0.5)6) ///
	scheme(s1mono) ///
	name(Figure2AZ, replace) ///
	xsize(5) 
	
* Export the graph as a portable network graphics (PNG) file
graph save "Figure 0B1_v2.gph", replace 
	
* Generate list of 10 highest/lowest predicted stratum means (for Table 4)
sort m1Bmrank
list stratum fem eth2 reli ukborn n N m1Bmrank m1Bm m1Bmlo m1Bmhi in f/10 
list stratum fem eth2 reli ukborn  n N m1Bmrank m1Bm m1Bmlo m1Bmhi in -10/-1  
list stratum fem eth2 reli ukborn   N m1Bmrank m1Bm m1Bmlo m1Bmhi   

*-------------------------------------------------------------------------------
**##  
*-------------------------------------------------------------------------------

* Load the stratum-level data
use "stratum_v2.dta", clear

capture drop h_mus
egen h_mus = max(reli == 3), by(stratum)
* Rank the predicted stratum means
egen m1Cmrank = rank(m1Cm)

* Generate the lower and upper limits of the approximate 95% confidence 
* intervals for the predicted stratum means
generate m1Cmlo = m1Cm - 1.96 * sqrt(m1Bxbse^2 + m1Buse^2)
generate m1Cmhi = m1Cm + 1.96 * sqrt(m1Bxbse^2 + m1Buse^2)
// Approximate as the model assumes no sampling covariability between the 
// regression coefficients and the stratum random effect

* Plot the caterpillar plot of the predicted stratum means

twoway ///
	(rspike m1Cmhi m1Cmlo m1Cmrank, lcolor(gs4)) /// 
	(rspike m1Cmhi m1Cmlo m1Cmrank if h_mus == 1, lcolor(lime) lwidth(thick) ) ///
	(scatter m1Cm m1Cmrank, mcolor(black) msymbol(smcircle) msize(tiny)) ///
	(scatter m1Cm m1Cmrank if h_mus == 1, mcolor(lime) msymbol(smcircle) msize(tiny)) ///
	, ///
	ytitle("{bf:(B) Predicted Average Log Hourly}" "{bf:Earnings Model 1C}", size(*0.7)) ///
	xtitle("Stratum rank") ///
	xlabel(0 50 100 115) ///
	legend(off) ylabel(4(0.5)6) ///
	scheme(s1mono) ///
	name(Figure2A, replace) ///
	xsize(5)
	
* Export the graph as a portable network graphics (PNG) file
graph save "Figure 0C1_v2.gph", replace 
	
* Generate list of 10 highest/lowest predicted stratum means (for Table 4)
sort m1Cmrank
list stratum fem eth2 reli ukborn N m1Cmrank m1Cm m1Cmlo m1Cmhi in f/10 
list stratum fem eth2 reli ukborn  N m1Cmrank m1Cm m1Cmlo m1Cmhi in -10/-1 
sort m1Cmrank
list stratum fem eth2 reli ukborn N m1Cmrank m1Cm m1Cmlo m1Cmhi  

graph combine "Figure 0B1_v2.gph" "Figure 0C1_v2.gph", row(2) col(1) ysize(10) iscale(*0.95)
graph save "Combo0_v2.gph", replace 
********************************************************************************
**# Figure 3 - Catterpillar plots. 
********************************************************************************

* 1B1

* Load the stratum-level data
use "stratum_v2.dta", clear

* Rank the predicted stratum random effects
egen m1Burank = rank(m1Bu)

* Generate the lower and upper 95% confidence interval limits for the predicted 
* stratum random effect
generate m1Bulo = m1Bu - 1.96 * m1Buse
generate m1Buhi = m1Bu + 1.96 * m1Buse

* Plot the caterpillar plot of the predicted stratum random effects
twoway (rspike m1Buhi m1Bulo m1Burank) (scatter m1Bu m1Burank)

* Re-plot the caterpillar plot of the predicted stratum random effects with 
* options
twoway ///
	(rspike m1Buhi m1Bulo m1Burank, lcolor(gs4)) ///
	(scatter m1Bu m1Burank, mcolor(black) msymbol(smcircle)) ///
	, ///
	ytitle("{bf: (A) Predicted Stratum Random Effect}" ///
	 "{bf:Average Log Hourly Earnings Model 1B}", size(*1)) ///
	yline(0) ///
	xtitle("Stratum rank") ///
	xlabel(0 50 100 115) ///
	legend(off) ///
	scheme(s1mono) ///
	name(Figure3A, replace) ///
	xsize(5)

* Export the graph as a portable network graphics (PNG) file
graph save "Figure 1B1_v2.gph", replace 


* Load the stratum-level data
use "stratum_v2.dta", clear

* Rank the predicted stratum random effects
egen m1Curank = rank(m1Cu)

* Generate the lower and upper 95% confidence interval limits for the predicted 
* stratum random effect
generate m1Culo = m1Cu - 1.96 * m1Cuse
generate m1Cuhi = m1Cu + 1.96 * m1Cuse

* Plot the caterpillar plot of the predicted stratum random effects
twoway (rspike m1Cuhi m1Culo m1Curank) (scatter m1Cu m1Curank)

* Re-plot the caterpillar plot of the predicted stratum random effects with 
* options
twoway ///
	(rspike m1Cuhi m1Culo m1Curank, lcolor(gs4)) ///
	(scatter m1Cu m1Curank, mcolor(black) msymbol(smcircle)) ///
	, ///
	ytitle("{bf:(B) Predicted Stratum Random Effect}" ///
	"{bf:Average Log Hourly Earnings Model 1C}", size(*1)) ///
	yline(0) ///
	xtitle("Stratum rank") ///
	xlabel(0 50 100 115) ///
	legend(off) ///
	scheme(s1mono) ///
	name(Figure3A, replace) ///
	xsize(5)

* Export the graph as a portable network graphics (PNG) file
graph save "Figure 1C1_v2.gph", replace 


* Load the stratum-level data
use "stratum_v2.dta", clear

* Generate the lower and upper 95% confidence interval limits for the predicted 
* stratum random effect
generate m1Bulo = m1Bu - 1.96 * m1Buse
generate m1Buhi = m1Bu + 1.96 * m1Buse

* Generate a binary indicator for whether the predicted stratum random effect is 
* significant
generate m1Busig = (m1Buhi < 0 | m1Bulo > 0)

* Rank the significant predicted stratum random effects
egen m1Busigrank = rank(m1Bu) if m1Busig == 1

* Plot the caterpillar plot of the significant predicted stratum random effects
twoway (rspike m1Buhi m1Bulo m1Busigrank) (scatter m1Bu m1Busigrank) 
	
* Re-plot the caterpillar plot of the significant predicted stratum random 
* effects with options
capture drop h_mus
egen h_mus = max(reli == 3), by(stratum)
list stratum reli h_mus if h_mus
twoway ///
	(rspike m1Buhi m1Bulo m1Busigrank, lcolor(gs4)) ///
	(scatter m1Bu m1Busigrank, mcolor(black) msymbol(smcircle)) ///
	
twoway ///	
	(rspike m1Buhi m1Bulo m1Busigrank, lcolor(gs4)) ///
	(scatter m1Bu m1Busigrank, mcolor(black) msymbol(smcircle)) ///
	(scatter m1Bulo m1Busigrank if m1Buhi < 0, ///
		msymbol(none) mlabel(stratum) mlabposition(7) mlabangle(ninety) ///
		mlabsize(*0.8) mlabgap(0)) ///
	(scatter m1Buhi m1Busigrank if m1Bulo > 0, ///
		msymbol(none) mlabel(stratum) mlabposition(1) mlabangle(ninety) ///
		mlabsize(*0.8) mlabgap(0)) ///
		(scatter m1Bu m1Busigrank if h_mus == 1, mcolor(lime) ms(O)) ///
	, ///
	ytitle("{bf:(C) Predicted Stratum Random Effect}" ///
		 "{bf: in Average Log Hourly Earnings Model 1B}", size(*1)) ///
	yline(0) ///
	xtitle("Stratum rank") ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ylabel(-0.25(0.2)0.25) ///
	name(Figure3B, replace) ///
	xsize(5) 
	
* Export the graph as a portable network graphics (PNG) file
graph save "Figure 1B2_v2.gph", replace 



* Load the stratum-level data
use "stratum_v2.dta", clear

* Generate the lower and upper 95% confidence interval limits for the predicted 
* stratum random effect
generate m1Culo = m1Cu - 1.96 * m1Cuse
generate m1Cuhi = m1Cu + 1.96 * m1Cuse

* Generate a binary indicator for whether the predicted stratum random effect is 
* significant
generate m1Cusig = (m1Cuhi < 0 | m1Culo > 0)

* Rank the significant predicted stratum random effects
egen m1Cusigrank = rank(m1Cu) if m1Cusig == 1

* Plot the caterpillar plot of the significant predicted stratum random effects
twoway (rspike m1Cuhi m1Culo m1Cusigrank) (scatter m1Cu m1Cusigrank) 
	
* Re-plot the caterpillar plot of the significant predicted stratum random 
* effects with options
capture drop h_mus
egen h_mus = max(reli == 3), by(stratum)
list stratum reli h_mus if h_mus
twoway ///
	(rspike m1Cuhi m1Culo m1Cusigrank, lcolor(gs4)) ///
	(scatter m1Cu m1Cusigrank, mcolor(black) msymbol(smcircle)) ///
	(scatter m1Culo m1Cusigrank if m1Cuhi < 0, ///
		msymbol(none) mlabel(stratum) mlabposition(7) mlabangle(ninety) ///
		mlabsize(*0.8) mlabgap(0)) ///
	(scatter m1Cuhi m1Cusigrank if m1Culo > 0, ///
		msymbol(none) mlabel(stratum) mlabposition(1) mlabangle(ninety) ///
		mlabsize(*0.8) mlabgap(0)) ///
		(scatter m1Cu m1Cusigrank if h_mus == 1, mcolor(lime) ms(O)) ///
	, ///
	ytitle("{bf: (D) Predicted Stratum Random Effect}" ///
		"{bf:in Average Log Hourly Earnings 1C}", size(*1)) ///
	yline(0) ///
	xtitle(" " "Stratum rank") ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ylabel(-0.25(0.2)0.25) ///
	name(Figure3B, replace) ///
	xsize(5) 
	
* Export the graph as a portable network graphics (PNG) file
graph save "Figure 1C2_v2.gph", replace 

graph combine "Figure 1B2_v2.gph" "Figure 1B1_v2.gph" "Figure 1C2_v2.gph" "Figure 1C1_v2.gph" , row(2) col(2) iscale(*0.75)
graph save "Combo1_v2.gph", replace 

graph combine "Combo0_v2.gph" "Combo1_v2.gph", row(1)


