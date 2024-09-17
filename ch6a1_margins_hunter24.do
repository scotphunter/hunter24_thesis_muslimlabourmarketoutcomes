** ************************************************************************** **
	* CHAPTER 6 ANALYSIS 1 - CONTEXUAL MODELS AND MARGINS PLOTS
	* Part of PhD research on assessing impact of contemporary migration 
	* patterns on the Muslim community in the UK. 
	* Last updated 19 JULY 2024
	* Scot Hunter 
** ************************************************************************** **

********************************************************************************
**# General Setup
********************************************************************************

* Set command interpreter to version 18
version 18.0

* Load in UKHLS pooled data. 
use $ptemp/zfile_emppar.dta, clear

* Log analysis and save log
capture log close
log using "$poutputs/lsoa_msoa_comp" , replace



********************************************************************************
**# Data management 
********************************************************************************

* sort out dummy for the urban control. 
recode urban 2 = 0 
* set global for list of covariates to be included in models below. 
global parvar "dvage age2 c.educscore ib7.gor_dv urban wave"

*//*

Information is avaiable at different levels of geography, including: 
	- LSOA (400/1200 HHs and 1000/3000 people) 
	- MSOA level (2000/600 HHs and 5000/15000 people). 
	
	Inclined to go with lower level of geography (LSOA) as is done in 
	the literature. Might be worth exploring patterns at different levels to 
	see if there are any major differences. This means generating neighbourhood 
	indicators at both levels (lsoa and msoa). 
	
	There also might be some worth in generating measures that take information 
	from both levels - I'll try this by taking making a composite measures that
	comprises 0.50 % of LSOA indicator and the same from MSOA indicator. 
	
	Each indicator will be divided into quintile versions of the variables. 
	This is so they can easily be interacted and skewness is addressed. 

*/




* LSOA measures 
	* resources: 
su raw_lsoa_top2 raw_lsoa_educ16plus4 raw_lsoa_emprate
	* gen composite measure: 
capture drop lsoa_scale 
gen lsoa_scale = raw_lsoa_top2 + raw_lsoa_educ16plus4 + raw_lsoa_emprate
	* gen quintile version. 
capture drop dep_qt 
xtile dep_qt = lsoa_scale, nq(5)
	* gen quintiles of EU & Muslim at LSOA. 
capture drop eu_qt
xtile eu_qt = log_lsoa_eu , nq(5)
capture drop mus_qt
xtile mus_qt = raw_lsoa_muslim, nq(5)



* MSOA measures 
capture drop msoa_scale 
gen msoa_scale = raw_msoa_top2 + raw_msoa_educ16plus4 + raw_msoa_emprate
	* quintile version 
capture drop m_dep_qt
xtile m_dep_qt = msoa_scale, nq(5)
capture drop m_mus_qt 
xtile m_mus_qt = raw_msoa_muslim , nq(5)
capture drop m_eu_qt
xtile m_eu_qt  = raw_msoa_eu , nq(5)



* LSOA & MSOA measure 
capture drop c_dep_qt 
gen c_dep_qt = 0.50 * dep_qt + 0.50 * m_dep_qt
capture drop c_mus_qt 
gen c_mus_qt = 0.50 * mus_qt + 0.50 * m_mus_qt
capture drop c_eu_qt 
gen c_eu_qt = 0.50 * eu_qt + 0.50 * m_eu_qt


* correlation between outcome & macro measures. 
	* (compare the patterns of different measures)
bysort muslim : pwcorr outcome6 outcome7 ///
dep_qt mus_qt eu_qt m_dep_qt m_mus_qt m_eu_qt c_dep_qt c_mus_qt c_eu_qt ///
if fem == 0 [aw=nwei], star(0.05) 

bysort muslim : pwcorr outcome6 outcome7 ///
dep_qt mus_qt eu_qt m_dep_qt m_mus_qt m_eu_qt c_dep_qt c_mus_qt c_eu_qt ///
if fem == 1 [aw=nwei], star(0.05) 


*//*

	Looking at the correlations (focusing on Muslim respondents)
	I see marginal differences between the three different measures. Overall, 
	I don't think there is much evidence to suggest one measure is better than 
	the other.

*/


********************************************************************************
**# Preliminary analysis  
********************************************************************************

*//*
	Going to first generate box plots which show the distribution of the raw 
	measures and how they're categorised into quintiles. Also going to do some 
	bivariate analysis betweent participationa and employment by Muslim group - 
	using confidence interval plots. 
*/

********************************************************************************
**# Data management 
********************************************************************************

* Setting global for look of confidence intervals for graphs below. 

global cits "ciopts(type(rcap) lcolor(black) lwidth(med))"


* Boxplot showing the distribution of raw measures by quintile. 
su lsoa_scale raw_lsoa_muslim raw_lsoa_eu dep_qt mus_qt eu_qt
graph box lsoa_scale, over(dep_qt) ///
ytitle("Comoposite resources scale (employed/graduates/top occupations)")
graph save $pgraphs/box1.gph, replace 
graph box raw_lsoa_muslim, over(mus_qt) ytitle("Raw % of Muslim in LSOA")
graph save $pgraphs/box2.gph, replace 
graph box raw_lsoa_eu, over(eu_qt) ytitle("Raw % of post-04 EU migrants in LSOA")
graph save $pgraphs/box3.gph, replace 


* Loop for generating the mean estimates for participation 
* and employment across ethno-religious groups. 
foreach num of numlist 6 7 { 
	mean outcome`num' if fem == 0, over(mcats)
	est store mens`num's
	mean outcome`num' if fem == 1, over(mcats)
    est store fems`num's
}


* Plotting mean estimates (using recast - bar to make bar graphs with 95% CI). 
coefplot (mens6s, bcolor(black%25)) (fems6s, bcolor(black%65)), ///
drop(_cons) recast(bar) $cits citop ///
barwidth(0.60) legend(pos(6) row(1)label(1 "Men") label(3 "Women"))  ///
coeflabels(c.outcome6@1.mcats ="Christian White British" ///
c.outcome6@2.mcats ="Indian (fgn) " ///
c.outcome6@3.mcats ="Pakistani (fgn)" ///
c.outcome6@4.mcats ="Bangladeshi (fgn)"  ///
c.outcome6@5.mcats ="Black (fgn/UK)"  ///
c.outcome6@6.mcats ="Other Muslim (fgn/UK)" ///
c.outcome6@7.mcats ="Indian (UK)"  ///
c.outcome6@8.mcats ="Pakistani (UK)"  ///
c.outcome6@9.mcats ="Bangladeshi (UK)", wrap(11) labsize(small)) ///
title("{bf:(a) Mean proportion}" "{bf:(participation == 1)}", size(small)) 
graph save $pgraphs/ci6.gph, replace


coefplot (mens7s, bcolor(black%25)) (fems7s, bcolor(black%65)), ///
drop(_cons) recast(bar) $cits citop ///
barwidth(0.60) legend(pos(6) row(1)label(1 "Men") label(3 "Women"))  ///
coeflabels(c.outcome7@1.mcats ="Christian White British" ///
c.outcome7@2.mcats ="Indian (fgn) " ///
c.outcome7@3.mcats ="Pakistani (fgn)" ///
c.outcome7@4.mcats ="Bangladeshi (fgn)"  ///
c.outcome7@5.mcats ="Black (fgn/UK)"  ///
c.outcome7@6.mcats ="Other Muslim (fgn/UK)" ///
c.outcome7@7.mcats ="Indian (UK)"  ///
c.outcome7@8.mcats ="Pakistani (UK)"  ///
c.outcome7@9.mcats ="Bangladeshi (UK)", wrap(11) labsize(small)) ///
title("{bf:(b) Mean proportion}" "{bf:(employment == 1)}", size(small)) 
graph save $pgraphs/ci7.gph, replace


graph combine $pgraphs/ci6.gph $pgraphs/ci7.gph, col(2)


rename religiosity sity 
tab liberal 
recode liberal . = 0
capture drop lonse
gen lonse = (gor_dv == 7 | gor_dv == 8)


*gen lonse 

su outcome6 mcats fem dvage_ind educscore engprof lonse  mar child health cardum ties liberal sity

capture drop test 
gen test = mcats 

capture drop resm
gen resm = mcats 
recode resm 1 = 1 2 = 2 3 = 3 4 = 4 7 = 5 8 = 6 9 = 7 5 = 8 6 = 9
tab resm mcats 


* Missing data - keep only cases with data on all variables. 
global vars1 "outcome6 mcats fem dvage_ind educscore engprof lonse mar child health cardum ties liberal sity "
capture drop miss1 
egen miss1 = rmiss($vars1)
codebook $vars1 if miss1 == 0, compact

global parvar "dvage_ind educscore engprof lonse mar child health cardum ties liberal sity"

* Set global for generic regression options 
global miss "& miss == 0 [pw=nwei] , robust cluster(pidp)"

* LSOA
	* Pariticpation logistic regresison loops (for men 0 and women 1) 
foreach num of numlist 0 1 { 
	* base model 
 logit outcome6 i.wave i.resm if fem == `num' $miss1 
	est store b`num'_6 
	* adding in individual level controls.
  logit outcome6 i.wave i.resm $parvar if fem == `num' $miss1 
	est store i`num'_6 
	* adding in neighbourhood charactersitics. 
  logit outcome6 i.wave i.resm $parvar dep_qt  mus_qt eu_qt ///
		if fem == `num' $miss1
	est store all`num'_6 
	* adding interactions between mcats and neighbourhood chars. 
  logit outcome6 i.wave i.resm $parvar ///
		i.resm##c.dep_qt i.resm##c.mus_qt i.resm##c.eu_qt /// 		 
		if fem == `num' $miss1
	est store int`num'_6 
}

esttab int0_6 int1_6, ///
keep(*resm dep_qt mus_qt eu_qt _cons) b(%8.2f) not stat(N r2 r2_p)

esttab int0_6 int1_6, ///
b(%8.2f) not stat(N r2 r2_p)

* dep 
est restore int0_6
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm #c.dep_qt]
    scalar d_mpvalue_`cat' = r(p)
    display "P-value for mcats category `cat': " d_mpvalue_`cat'
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.mus_qt]
    scalar m_mpvalue_`cat' = r(p)
    display "P-value for mcats category `cat': " m_mpvalue_`cat'
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm #c.eu_qt]
    scalar eu_mpvalue_`cat' = r(p)
    display "P-value for mcats category `cat': " eu_mpvalue_`cat'
}

est restore int1_6
* Test the significance of each interaction term
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm #c.dep_qt]
    scalar d_fpvalue_`cat' = r(p)
    display "P-value for mcats category `cat': " d_fpvalue_`cat'
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm #c.mus_qt]
    scalar m_fpvalue_`cat' = r(p)
    display "P-value for mcats category `cat': " m_fpvalue_`cat'
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm #c.eu_qt]
    scalar eu_fpvalue_`cat' = r(p)
    display "P-value for mcats category `cat': " eu_fpvalue_`cat'
}


*dep 
local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
foreach cat of numlist 2/9 { 
	est restore int0_6
	quietly margins, at(resm  = (`cat') dep_qt=(0(1)5)) ///
	level(95) saving(d_0_6_`cat', replace) 
	
	local mcolor "black"  
    if d_mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
    }
	est restore int1_6
	quietly margins, at(resm  = (`cat') dep_qt=(0(1)5)) ///
	level(95) saving(d_1_6_`cat', replace) 
	
	local lcolor "black"  
    if d_fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
	local current_title "`title`cat''"
	combomarginsplot d_0_6_`cat' d_1_6_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(d_cg_6_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
	
}


*mus 
local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
foreach cat of numlist 2/9 {
	est restore int0_6
	quietly margins, at(resm  = (`cat') mus_qt=(0(1)5)) ///
	level(95) saving(m_0_6_`cat', replace) 
		local mcolor "black"  
    if m_mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
	}
	est restore int1_6
	quietly margins, at(resm  = (`cat') mus_qt=(0(1)5)) ///
	level(95) saving(m_1_6_`cat', replace) 
		 local lcolor "black"  
    if m_fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
		local current_title "`title`cat''"

	combomarginsplot m_0_6_`cat' m_1_6_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(m_cg_6_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw

	 
}
	
*eu
local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
foreach cat of numlist 2/9 {
	est restore int0_6
	quietly margins, at(resm  = (`cat') eu_qt=(0(1)5)) ///
	level(95) saving(eu_0_6_`cat', replace) 
	local mcolor "black"  
    if eu_mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
    }
	est restore int1_6
	quietly margins, at(resm  = (`cat') eu_qt=(0(1)5)) ///
	level(95) saving(eu_1_6_`cat', replace) 
	 local lcolor "black"  
    if eu_fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
		local current_title "`title`cat''"

    	combomarginsplot eu_0_6_`cat' eu_1_6_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(eu_cg_6_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
	
	}
	
graph combine ///
d_cg_6_2 d_cg_6_3 d_cg_6_4 d_cg_6_5 d_cg_6_6 d_cg_6_7 d_cg_6_8 d_cg_6_9, ///
ycommon iscale(*0.95) title("{bf: (A) Participation & Deprivation}")
	graph save "$pgraphs/d_cg_6.gph" , replace 
graph combine ///
m_cg_6_2 m_cg_6_3 m_cg_6_4 m_cg_6_5 m_cg_6_6 m_cg_6_7 m_cg_6_8 m_cg_6_9, ///
ycommon iscale(*0.95)  title("{bf: (B) Participation & Muslim Share}")
	graph save "$pgraphs/m_cg_6.gph" , replace 
graph combine ///
eu_cg_6_2 eu_cg_6_3 eu_cg_6_4 eu_cg_6_5 eu_cg_6_6 eu_cg_6_7 eu_cg_6_8 eu_cg_6_9, ///
ycommon iscale(*0.95)   title("{bf: (C) Participation & Post-04 EU Share }")
		graph save "$pgraphs/eu_cg_6.gph" , replace 
graph combine  "$pgraphs/d_cg_6.gph"  "$pgraphs/m_cg_6.gph"  "$pgraphs/eu_cg_6.gph" , row(2) col(2) ysize(16) xsize(16) iscale(*0.75)
 
graph save $pgraphs/par_fin.gph, replace 

//	
// foreach cat of numlist 2/9 {
//     * Plot margins for each category
// 	est restore int0_6
// 	quietly margins, at(resm  = (`cat') dep_qt=(0(1)5)) ///
// 	level(95) saving(d_0_6_`cat', replace) 
// 	quietly margins, at(resm  = (`cat') mus_qt=(0(1)5)) ///
// 	level(95) saving(m_0_6_`cat', replace) 
// 	quietly margins, at(resm  = (`cat') eu_qt=(0(1)5)) ///
// 	level(95) saving(eu_0_6_`cat', replace) 
//    
//     * Check if the interaction is significant and set color accordingly
//     local mcolor "black"  
//     if d_mpvalue_`cat' < 0.05 {
//         local mcolor "lime"  
//     }
// 	local mcolor "black"  
//     if m_mpvalue_`cat' < 0.05 {
//         local mcolor "lime"  
//     }
// local mcolor "black"  
//     if eu_mpvalue_`cat' < 0.05 {
//         local mcolor "lime"  
//     }
//
//
//    
// 	est restore int1_6
// 	quietly margins, at(resm  = (`cat') dep_qt=(0(1)5)) ///
// 	level(95) saving(d_1_6_`cat', replace) 
// 	quietly margins, at(resm  = (`cat') mus_qt=(0(1)5)) ///
// 	level(95) saving(m_1_6_`cat', replace) 
// 	quietly margins, at(resm  = (`cat') eu_qt=(0(1)5)) ///
// 	level(95) saving(eu_1_6_`cat', replace) 
//    
//     local lcolor "black"  
//     if d_fpvalue_`cat' < 0.05 {
//         local lcolor "pink"  
//     }
// 	 local lcolor "black"  
//     if m_fpvalue_`cat' < 0.05 {
//         local lcolor "pink"  
//     }
// 	 local lcolor "black"  
//     if eu_fpvalue_`cat' < 0.05 {
//         local lcolor "pink"  
//     }
//	
//	
// 	local current_title "`title`cat''"
//	
// 	combomarginsplot d_0_6_`cat' d_1_6_`cat', ///
// 	noci xlabel(0 "Least" 5 "Most") recast(line) ///
// 	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
// 	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
// 	name(d_cg_6_`cat', replace) ///
// 	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
//	
// 	combomarginsplot m_0_6_`cat' m_1_6_`cat', ///
// 	noci xlabel(0 "Least" 5 "Most") recast(line) ///
// 	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
// 	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
// 	name(m_cg_6_`cat', replace) ///
// 	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
//	
// 	combomarginsplot eu_0_6_`cat' eu_1_6_`cat', ///
// 	noci xlabel(0 "Least" 5 "Most") recast(line) ///
// 	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
// 	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
// 	name(eu_cg_6_`cat', replace) ///
// 	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
//	
// }





*missing data - keep only cases with data on all variables. 
global vars2 "outcome7 resm fem dvage_ind educscore engprof lonse mar child health cardum ties liberal sity dep_qt mus_qt eu_qt"
capture drop miss2 
egen miss2 = rmiss($vars2)
codebook $vars2 if miss2 == 0, compact
global miss "& miss2 == 0 [pw=nwei] , robust cluster(pidp)"
** pariticpation logistic regresison loops (for men 0 and women 1) 
foreach num of numlist 0 1 { 
	* base model 
 logit outcome7 i.wave i.resm if fem == `num' $miss
	est store b`num'_7
	* adding in individual level controls.
  logit outcome7 i.wave i.resm $parvar if fem == `num' $miss
	est store i`num'_7
	* adding in neighbourhood charactersitics. 
  logit outcome7 i.wave i.resm $parvar dep_qt mus_qt eu_qt ///
		if fem == `num' $miss
	est store all`num'_7
	* adding interactions between resm and neighbourhood chars. 
  logit outcome7 i.wave i.resm $parvar ///
		i.resm##c.dep_qt i.resm##c.mus_qt i.resm##c.eu_qt /// 		 
		if fem == `num' $miss
	est store int`num'_7
}



* dep 
est restore int0_7
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.dep_qt]
    scalar d_mpvalue_`cat'_p = r(p)
    display "P-value for resm category `cat': " d_mpvalue_`cat'_p
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.mus_qt]
    scalar m_mpvalue_`cat'_p = r(p)
    display "P-value for resm category `cat': " m_mpvalue_`cat'_p
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.eu_qt]
    scalar eu_mpvalue_`cat'_p = r(p)
    display "P-value for resm category `cat': " eu_mpvalue_`cat'_p
}

est restore int1_7
* Test the significance of each interaction term
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.dep_qt]
    scalar d_fpvalue_`cat'_p = r(p)
    display "P-value for resm category `cat': " d_fpvalue_`cat'_p
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.mus_qt]
    scalar m_fpvalue_`cat'_p = r(p)
    display "P-value for resm category `cat': " m_fpvalue_`cat'_p
}

foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.eu_qt]
    scalar eu_fpvalue_`cat'_p = r(p)
    display "P-value for resm category `cat': " eu_fpvalue_`cat'_p
}


*d
local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
foreach cat of numlist 2/9 {
est restore int0_7
	quietly margins, at(resm = (`cat') dep_qt=(0(1)5)) ///
	level(95) saving(d_0_7_`cat', replace) 
	local mcolor "black"  
    if d_mpvalue_`cat'_p < 0.05 {
        local mcolor "lime"  
    }
	est restore int1_7
	quietly margins, at(resm = (`cat') dep_qt=(0(1)5)) ///
	level(95) saving(d_1_7_`cat', replace) 
	 local lcolor "black"  
    if d_fpvalue_`cat'_p < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot d_0_7_`cat' d_1_7_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(d_cg_7_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
	
}

*mus
local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
foreach cat of numlist 2/9 {
	est restore int0_7
	quietly margins, at(resm = (`cat') mus_qt=(0(1)5)) ///
	level(95) saving(m_0_7_`cat', replace) 
		 local mcolor "black"  
    if m_mpvalue_`cat'_p < 0.05 {
        local mcolor "lime"  
    }
	est restore int1_7
	quietly margins, at(resm = (`cat') mus_qt=(0(1)5)) ///
	level(95) saving(m_1_7_`cat', replace) 
		local lcolor "black"  
    if m_fpvalue_`cat'_p < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot m_0_7_`cat' m_1_7_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(m_cg_7_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
	
}

*eu 
local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
foreach cat of numlist 2/9 {
    * Plot margins for each category
	est restore int0_7
	quietly margins, at(resm = (`cat') eu_qt=(0(1)5)) ///
	level(95) saving(eu_0_7_`cat', replace) 
		 local mcolor "black"  
    if eu_mpvalue_`cat'_p < 0.05 {
        local mcolor "lime"  
    }
	est restore int1_7
	quietly margins, at(resm = (`cat') eu_qt=(0(1)5)) ///
	level(95) saving(eu_1_7_`cat', replace) 
    	local lcolor "black"  
    if eu_fpvalue_`cat'_p < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot eu_0_7_`cat' eu_1_7_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(eu_cg_7_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
	
	
}

graph combine ///
d_cg_7_2 d_cg_7_3 d_cg_7_4 d_cg_7_5 d_cg_7_6 d_cg_7_7 d_cg_7_8 d_cg_7_9, ///
ycommon iscale(*0.95) title("{bf: (A) Employment & Deprivation}")
	graph save "$pgraphs/d_cg_7.gph" , replace 
graph combine ///
m_cg_7_2 m_cg_7_3 m_cg_7_4 m_cg_7_5 m_cg_7_6 m_cg_7_7 m_cg_7_8 m_cg_7_9, ///
ycommon iscale(*0.95)  title("{bf: (B) Employment & Muslim Share}")
	graph save "$pgraphs/m_cg_7.gph" , replace 
graph combine ///
eu_cg_7_2 eu_cg_7_3 eu_cg_7_4 eu_cg_7_5 eu_cg_7_6 eu_cg_7_7 eu_cg_7_8 eu_cg_7_9, ///
ycommon iscale(*0.95)   title("{bf: (C) Employment & Post-04 EU Share }")
		graph save "$pgraphs/eu_cg_7.gph" , replace 
graph combine  "$pgraphs/d_cg_7.gph"  "$pgraphs/m_cg_7.gph"  "$pgraphs/eu_cg_7.gph" , row(2) col(2) ysize(16) xsize(16) iscale(*0.65)
graph save $pgraphs/emp_fin.gph, replace 

// foreach cat of numlist 2/9 {
//     * Plot margins for each category
// 	est restore int0_7
// 	quietly margins, at(resm = (`cat') dep_qt=(0(1)5)) ///
// 	level(95) saving(d_0_7_`cat', replace) 
// 	quietly margins, at(resm = (`cat') mus_qt=(0(1)5)) ///
// 	level(95) saving(m_0_7_`cat', replace) 
// 	quietly margins, at(resm = (`cat') eu_qt=(0(1)5)) ///
// 	level(95) saving(eu_0_7_`cat', replace) 
//	
//
//
//    
//     * Check if the interaction is significant and set color accordingly
//     local mcolor "black"  
//     if d_mpvalue_`cat'_p < 0.1 {
//         local mcolor "lime"  
//     }
//	
// 	 local mcolor "black"  
//     if m_mpvalue_`cat'_p < 0.1 {
//         local mcolor "lime"  
//     }
//	
// 	 local mcolor "black"  
//     if eu_mpvalue_`cat'_p < 0.1 {
//         local mcolor "lime"  
//     }
//    
// 	est restore int1_7
// 	quietly margins, at(resm = (`cat') dep_qt=(0(1)5)) ///
// 	level(95) saving(d_1_7_`cat', replace) 
// 	quietly margins, at(resm = (`cat') mus_qt=(0(1)5)) ///
// 	level(95) saving(m_1_7_`cat', replace) 
// 	quietly margins, at(resm = (`cat') eu_qt=(0(1)5)) ///
// 	level(95) saving(eu_1_7_`cat', replace) 
//    
//     local lcolor "black"  
//     if d_fpvalue_`cat'_p < 0.051 {
//         local lcolor "pink"  
//     }
//	
// 	local lcolor "black"  
//     if m_fpvalue_`cat'_p < 0.051 {
//         local lcolor "pink"  
//     }
//	
// 	local lcolor "black"  
//     if eu_fpvalue_`cat'_p < 0.051 {
//         local lcolor "pink"  
//     }
//	
// 	local current_title "`title`cat''"
//	
// 	combomarginsplot d_0_7_`cat' d_1_7_`cat', ///
// 	noci xlabel(0 "Least" 5 "Most") recast(line) ///
// 	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
// 	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
// 	name(d_cg_7_`cat', replace) ///
// 	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
//	
// 	combomarginsplot m_0_7_`cat' m_1_7_`cat', ///
// 	noci xlabel(0 "Least" 5 "Most") recast(line) ///
// 	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
// 	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
// 	name(m_cg_7_`cat', replace) ///
// 	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
//	
// 	combomarginsplot eu_0_7_`cat' eu_1_7_`cat', ///
// 	noci xlabel(0 "Least" 5 "Most") recast(line) ///
// 	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
// 	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
// 	name(eu_cg_7_`cat', replace) ///
// 	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
//	
//	
// }

graph combine ///
du_cg_7_2 d_cg_7_3 d_cg_7_4 d_cg_7_5 d_cg_7_6 d_cg_7_7 d_cg_7_8 d_cg_7_9, ///
ycommon iscale(*0.95) title("{bf: (A) Employment & Deprivation}")
	graph save "$pgraphs/d_cg_7.gph" , replace 
graph combine ///
m_cg_7_2 m_cg_7_3 m_cg_7_4 m_cg_7_5 m_cg_7_6 m_cg_7_7 m_cg_7_8 m_cg_7_9, ///
ycommon iscale(*0.95)  title("{bf: (B) Employment & Muslim Share}")
	graph save "$pgraphs/m_cg_7.gph" , replace 
graph combine ///
eu_cg_7_2 eu_cg_7_3 eu_cg_7_4 eu_cg_7_5 eu_cg_7_6 eu_cg_7_7 eu_cg_7_8 eu_cg_7_9, ///
ycommon iscale(*0.95)   title("{bf: (C) Employment & Post-04 EU Share }")
		graph save "$pgraphs/eu_cg_7.gph" , replace 
graph combine  "$pgraphs/d_cg_7.gph"  "$pgraphs/m_cg_7.gph"  "$pgraphs/eu_cg_7.gph" , row(3) col(1) ysize(32) xsize(16) iscale(*0.65)
graph save $pgraphs/emp_fin.gph, replace 


esttab int0_6 int1_6 int0_7 int1_7, ///
keep(*resm dep_qt mus_qt eu_qt _cons) b(%8.2f) not stat(N r2 r2_p)

 esttab int0_6 int1_6 int0_7 int1_7, ///
 b(%8.2f) not stat(N r2 r2_p) label



**** Employment related outcomes: 

use $pnew/emp_modelsrun.dta, clear 




rename religiosity sity 
tab liberal 
recode liberal . = 0
capture drop lonse
gen lonse = (gor_dv == 7 | gor_dv == 8)
capture drop test 
gen test = mcats 
capture drop resm
gen resm = mcats 
recode resm 1 = 1 2 = 2 3 = 3 4 = 4 7 = 5 8 = 6 9 = 7 5 = 8 6 = 9
tab resm mcats 


label define resm 1 "White British" 2 "Indian (fgn)" 3 "Pakistani (fgn)" 4 "Bangladeshi (fgn)" 5 "Indian (UK)" 6 "Pakistani (UK)" 7 "Bangladeshi (UK)" 8 "Black (fgn/UK)" 9 "Other Mus. (fgn/UK)"
label values resm resm

global parvar_ca "dvage_ind educscore engprof lonse mar child health cardum ties liberal sity"


estimates clear
* CAMSIS 
global vars3 "outcome1 resm fem dvage_ind educscore engprof lonse mar child health cardum ties liberal sity"
capture drop miss3
egen miss3 = rmiss($vars3)
codebook $vars3 if miss3 == 0, compact
global miss "& miss3 == 0 [pw=nwei] , robust cluster(pidp)"
foreach num of numlist 0 1 { 
	regress outcome1 i.wave i.resm if fem == `num' $miss3 
	est store b`num'_1
	*sd model 
	regress outcome1 i.wave i.resm $parvar_ca if fem == `num' $miss3 
	est store i`num'_1
	*local model 
	regress outcome1 i.wave i.resm $parvar_ca ///
	dep_qt mus_qt eu_qt if fem == `num' $miss3 
	est store loc`num'_1
	* occupation model 
	regress outcome1 i.wave i.resm $parvar_ca ///
	dep_qt mus_qt eu_qt ///
	lfs4eu_qt ///
	if fem == `num' $miss3 
	est store occ`num'_1
	*interaction model 
	regress outcome1 i.wave i.resm $parvar_ca ///
	dep_qt mus_qt eu_qt ///
	i.resm##c.lfs4eu_qt ///
	if fem == `num' $miss3 
	est store int`num'_1
}


* Test the significance of each interaction term
est restore int0_1
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar mpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " mpvalue_`cat'
}

est restore int1_1
* Test the significance of each interaction term
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar fpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " fpvalue_`cat'
}


* Derive marings and make plots 
local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"

foreach cat of numlist 2/9 {
    * Plot margins for each category
	est restore int0_1
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g0_1_`cat', replace) 
    
    * Check if the interaction is significant and set color accordingly
    local mcolor "black"  
    if mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
    }
    
	est restore int1_1
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g1_1_`cat', replace)
    
    local lcolor "black"  
    if fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot g0_1_`cat' g1_1_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(cg_1_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off)  nodraw
}


graph combine cg_1_2 cg_1_3 cg_1_4 cg_1_5 cg_1_6 cg_1_7 cg_1_8 cg_1_9, ///
	ycommon iscale(*0.95)  title("{bf: (A) CAMSIS}")
	graph save $pgraphs/cg_1.gph, replace 

	
global parvar_xt "dvage_ind  educscore engprof lonse mar child health cardum ties liberal sity"

* Income 
capture drop lfs4eu_qt
xtile lfs4eu_qt = raw_lfs4eu, nq(5)
capture drop miss4
egen miss4 = rmiss(outcome2 resm fem dvage_ind  educscore engprof ///
lonse mar child health cardum ties liberal sity parttime major pubsector lfs4eu_qt)

global miss4 " & miss4 == 0 [pw=nwei], robust cluster(pidp)"
foreach num of numlist 0 1 { 
	regress outcome2 i.wave i.resm if fem == `num' $miss4
	est store b`num'_2
	*sd model 
	regress outcome2 i.wave i.resm $parvar_xt if fem == `num' $miss4
	est store i`num'_2
	*local model 
	regress outcome2 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt if fem == `num' $miss4
	est store loc`num'_2
	* occupation model 
	regress outcome2 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	lfs4eu_qt pubsector parttime ib1.major  ///
	if fem == `num' $miss4
	est store occ`num'_2
	*interaction model 
	regress outcome2 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	pubsector parttime ib1.major i.resm##c.lfs4eu_qt ///
	if fem == `num' $miss4
	est store int`num'_2
}

* Test the significance of each interaction term
est restore int0_2
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar mpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " mpvalue_`cat'
}

est restore int1_2
* Test the significance of each interaction term
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar fpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " fpvalue_`cat'
}

local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"

foreach cat of numlist 2/9 {
    * Plot margins for each category
	est restore int0_2
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g0_2_`cat', replace) 
    
    * Check if the interaction is significant and set color accordingly
    local mcolor "black"  
    if mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
    }
    
	est restore int1_2
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g1_2_`cat', replace)
    
    local lcolor "black"  
    if fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot g0_2_`cat' g1_2_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(cg_2_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
}

graph combine cg_2_2 cg_2_3 cg_2_4 cg_2_5 cg_2_6 cg_2_7 cg_2_8 cg_2_9, ///
	ycommon iscale(*0.95) title("{bf: (B) Log Hourly Earnings}") 
		graph save "$pgraphs/cg_2.gph" , replace 


* Anxiety 
	
capture drop miss5
egen miss5 = rmiss(outcome3 resm fem dvage_ind educscore engprof ///
lonse mar child health cardum ties liberal sity parttime major pubsector lfs4eu_qt)
global miss5 " & miss5 == 0 [pw=nwei], robust cluster(pidp)"
foreach num of numlist 0 1 { 
	regress outcome3 i.wave i.resm if fem == `num' $miss5
	est store b`num'_3
	*sd model 
	regress outcome3 i.wave i.resm $parvar_xt if fem == `num' $miss5
	est store i`num'_3
	*local model 
	regress outcome3 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt if fem == `num' $miss5
	est store loc`num'_3
	* occupation model 
	regress outcome3  i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	lfs4eu_qt pubsector parttime ib1.major  ///
	if fem == `num' $miss5
	est store occ`num'_3
	*interaction model 
	regress outcome3 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	pubsector parttime ib1.major i.resm##c.lfs4eu_qt ///
	if fem == `num' $miss5
	est store int`num'_3
}

* Test the significance of each interaction term
est restore int0_3
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar mpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " mpvalue_`cat'
}

est restore int1_3
* Test the significance of each interaction term
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar fpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " fpvalue_`cat'
}

local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
* order needs to be 2 3 4 7 8 9 5 6

foreach cat of numlist 2/9 {
    * Plot margins for each category
	est restore int0_3
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g0_3_`cat', replace) 
    
    * Check if the interaction is significant and set color accordingly
    local mcolor "black"  
    if mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
    }
    
	est restore int1_3
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g1_3_`cat', replace)
    
    local lcolor "black"  
    if fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot g0_3_`cat' g1_3_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(cg_3_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
}

graph combine cg_3_2 cg_3_3 cg_3_4 cg_3_5 cg_3_6 cg_3_7 cg_3_8 cg_3_9, ///
	ycommon iscale(*0.95) title("{bf: (C) Financial Anxiety}") 
	graph save "$pgraphs/cg_3.gph" , replace 


* Job related satisfaction 

capture drop miss6
egen miss6 = rmiss(outcome4 resm fem dvage_ind educscore engprof ///
lonse mar child health cardum ties liberal sity parttime major pubsector lfs4eu_qt)

global miss6 " & miss6 == 0 [pw=nwei], robust cluster(pidp)"
foreach num of numlist 0 1 { 
	regress outcome4 i.wave i.resm if fem == `num' $miss6
	est store b`num'_4
	*sd model 
	regress outcome4 i.wave i.resm $parvar_xt if fem == `num' $miss6
	est store i`num'_4
	*local model 
	regress outcome4 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt if fem == `num' $miss6
	est store loc`num'_4
	* occupation model 
	regress outcome4 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	lfs4eu_qt pubsector parttime ib1.major  ///
	if fem == `num' $miss6
	est store occ`num'_4
	*interaction model 
	regress outcome4 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	pubsector parttime ib1.major i.resm##c.lfs4eu_qt ///
	if fem == `num' $miss6
	est store int`num'_4
}

* Test the significance of each interaction term
est restore int0_4
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar mpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " mpvalue_`cat'
}

est restore int1_4
* Test the significance of each interaction term
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar fpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " fpvalue_`cat'
}

local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"
foreach cat of numlist 2/9 {
    * Plot margins for each category
	est restore int0_4
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g0_4_`cat', replace) 
    
    * Check if the interaction is significant and set color accordingly
    local mcolor "black"  
    if mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
    }
    
	est restore int1_4
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g1_4_`cat', replace)
    
    local lcolor "black"  
    if fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot g0_4_`cat' g1_4_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(cg_4_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
}


graph combine cg_4_2 cg_4_3 cg_4_4 cg_4_5 cg_4_6 cg_4_7 cg_4_8 cg_4_9, ///
	ycommon iscale(*0.95) title("{bf: (D) Job Related Satisfaction}")
		graph save "$pgraphs/cg_4.gph" , replace 


* Job Quality 

capture drop miss7
egen miss7 = rmiss(outcome5 resm fem dvage_ind educscore engprof ///
lonse mar child health cardum ties liberal sity parttime major pubsector lfs4eu_qt)

global miss7 " & miss7 == 0 [pw=nwei], robust cluster(pidp)"
foreach num of numlist 0 1 { 
	logit outcome5 i.wave i.resm if fem == `num' $miss7
	est store b`num'_5
	*sd model 
	logit outcome5 i.wave i.resm $parvar_xt if fem == `num' $miss7
	est store i`num'_5
	*local model 
	logit outcome5 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt if fem == `num' $miss7
	est store loc`num'_5
	* occupation model 
	logit outcome5 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	lfs4eu_qt pubsector parttime ib1.major  ///
	if fem == `num' $miss7
	est store occ`num'_5
	*interaction model 
	logit outcome5 i.wave i.resm $parvar_xt ///
	dep_qt mus_qt eu_qt ///
	pubsector parttime ib1.major i.resm##c.lfs4eu_qt ///
	if fem == `num' $miss7
	est store int`num'_5
}


* Test the significance of each interaction term
est restore int0_5
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar mpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " mpvalue_`cat'
}

est restore int1_5
* Test the significance of each interaction term
foreach cat of numlist  2 3 4 5 6 7 8 9 {
    lincom _b[`cat'.resm#c.lfs4eu_qt]
    scalar fpvalue_`cat' = r(p)
    display "P-value for resm category `cat': " fpvalue_`cat'
}

local title2 "Indian (fgn)"
local title3 "Pakistani (fgn)"
local title4 "Bangladeshi (fgn)"
local title5 "Indian (UK)"
local title6 "Pakistani (UK)" 
local title7 "Bangladeshi (UK)"
local title8 "Black (fgn/UK)"
local title9 "Other Mus. (fgn/UK)"

foreach cat of numlist 2/9 {
    * Plot margins for each category
	est restore int0_5
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g0_5_`cat', replace) 
    
    * Check if the interaction is significant and set color accordingly
    local mcolor "black"  
    if mpvalue_`cat' < 0.05 {
        local mcolor "lime"  
    }
    
	est restore int1_5
	quietly margins, at(resm = (`cat') lfs4eu_qt=(0(1)5)) ///
	level(95) saving(g1_5_`cat', replace)
    
    local lcolor "black"  
    if fpvalue_`cat' < 0.05 {
        local lcolor "pink"  
    }
	
	local current_title "`title`cat''"
	
	combomarginsplot g0_5_`cat' g1_5_`cat', ///
	noci xlabel(0 "Least" 5 "Most") recast(line) ///
	plot1opts(lwidth(thick) lcolor(`mcolor')) ///
	plot2opts(lwidth(thick) lp(shortdash) lcolor(`lcolor')) ///
	name(cg_5_`cat', replace) ///
	title("`current_title'") xtitle("") ytitle("") legend(off) nodraw
}



graph combine cg_5_2 cg_5_3 cg_5_4 cg_5_5 cg_5_6 cg_5_7 cg_5_8 cg_5_9, ///
	ycommon iscale(*0.95) title("{bf: (E) Job Quality}")
		graph save "$pgraphs/cg_5.gph" , replace 

graph combine $pgraphs/cg_1.gph $pgraphs/cg_2.gph  $pgraphs/cg_3.gph $pgraphs/cg_4.gph $pgraphs/cg_5.gph, row(2) col(3) ysize(16) xsize(24) iscale(*0.75)
graph save $pgraphs/cg_fin.gph, replace 


***** for regression tables 

esttab int0_1 int1_1 int0_2 int1_2 int0_3 int1_3 int0_4 int1_4 int0_5 int1_5 , ///
 b(%8.2f) not stat(N r2 r2_p) label

