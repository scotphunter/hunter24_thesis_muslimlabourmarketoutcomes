** ************************************************************************** **
	* CHAPTER 6 ANALYSIS 2 - MIXED MODELS ANALYSIS 
	* Part of PhD research on assessing impact of contemporary migration 
	* patterns on the Muslim community in the UK. 
	* Last updated 1 AUG 2024
	* Scot Hunter 
** ************************************************************************** **


** Mulitlevel models 

global allvars "occ10_master muslim pidp wave outcome* mcats fem dvage_ind age2 educscore engprof liberal gor_dv mar child health cardum ties religiosity wave dep_qt mus_qt eu_qt pubsector parttime major lfs_soc4_p04 lfs4eu_qt"
use $allvars using $pnew/employment_data.dta, clear 


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


* limit data to one observation per person per job 

capture drop pjtag 
egen pjtag = tag(pidp occ10_master)
keep if pjtag

* Generate a new variabe which records occupation size
capture drop n
bysort occ10_master: generate n = _N

* Generate a new variabe which tags one record for each startum
capture drop occ_tag
egen occ_tag = tag(occ10_master)

capture drop mus_occ 
egen mus_occ = mean(muslim), by(occ10_master)

format %9.2f mus_occ lfs_soc4_p04


save $pnew/nuet.dta, replace 

global parvar "dvage age2 c.educscore engprof lonse mar child health cardum ties liberal sity"

* Outcome2 

* Outcome 2 
foreach num of numlist 0 1 { 
use $pnew/nuet.dta, clear 
global vars2 "outcome2 mcats fem dvage_ind educscore gor_dv wave dep_qt mus_qt eu_qt pubsector parttime lfs4eu_qt" 
capture drop miss2 
egen miss2 = rmiss($vars2)
global miss2 "miss2 == 0 "
keep if miss2 == 0
keep if fem == `num'
mixed outcome2 if fem == `num'  & $miss2 || occ10_master:
est store z2A_`num'
* save the model results 
estimates save "z2A_`num'.ster", replace 
* Store the between occupation variance 
scalar z2Asigma2_`num' =  exp(_b[lns1_1_1:_cons])^2
// The mixed command estimates and returns the between-stratum variance as the 
// log of the SD of the stratum random effects. We must therefore exponentiate 
// and square this quantity to recover the between-stratum variance.
* Predict the mean outcome 
predict z2Am_`num', fitted 
// Calculated as the fixed-portion linear prediction plus contributions based on 
// predicted random effects.
mixed outcome2 ///
	i.mcats $parvar c.dep_qt c.mus_qt c.eu_qt ///
	pubsector parttime ib1.major c.lfs4eu_qt if fem == `num' & $miss2 ///
	|| occ10_master:, baselevels
est store z2B_`num' 
estimates save "z2B_`num'.ster", replace 

* Store the between occupation variance 
scalar z2Bsigma2_`num' = exp(_b[lns1_1_1:_cons])^2

* Predict the mean outcome 
predict z2Bm_`num', fitted 

* Predict the occupation random effect and its SE 
predict z2Bu_`num', reffect reses(z2Buse_`num')

* Predict the standard error of the fixed-portion linear predicition 
predict z2Bxbse_`num', stdp 
compress 
save "$pnew/z_individual_2_`num'.dta", replace 

} 




* Outcome 3
foreach num of numlist 0 1 { 
use $pnew/nuet.dta, clear
global vars3 "outcome3 mcats fem dvage_ind educscore gor_dv wave dep_qt mus_qt eu_qt pubsector parttime  lfs4eu_qt" 
capture drop miss3 
egen miss3 = rmiss($vars3)
global miss3 "miss3 == 0"
keep if miss3 == 0
keep if fem == `num'


mixed outcome3 if fem == `num'  & $miss3 || occ10_master:
est store z3A_`num'
estimates save "z3A_`num'.ster", replace 
scalar z3Asigma2_`num' =  exp(_b[lns1_1_1:_cons])^2
predict z3Am_`num', fitted 


mixed outcome3 ///
	i.mcats $parvar  c.dep_qt c.mus_qt c.eu_qt ///
	pubsector parttime ib1.major  c.lfs4eu_qt if fem == `num' & $miss3 ///
	|| occ10_master:, baselevels
est store z3B_`num' 
estimates save "z3B_`num'.ster", replace 
scalar z3Bsigma2_`num' = exp(_b[lns1_1_1:_cons])^2
predict z3Bm_`num', fitted 
predict z3Bu_`num', reffect reses(z3Buse_`num')
predict z3Bxbse_`num', stdp 
compress 
save "$pnew/z_individual_3_`num'.dta", replace 
} 



* Outcome 4
foreach num of numlist 0 1 { 
use $pnew/nuet.dta, clear
global vars4 "outcome4 mcats fem dvage_ind educscore gor_dv  wave dep_qt mus_qt eu_qt pubsector parttime  lfs4eu_qt" 
capture drop miss4
egen miss4 = rmiss($vars4)
global miss4 "miss4 == 0"
keep if miss4 == 0
keep if fem == `num'


mixed outcome4 if fem == `num' & $miss4 || occ10_master:
est store z4A_`num'
estimates save "z4A_`num'.ster", replace 
scalar z4Asigma2_`num' =  exp(_b[lns1_1_1:_cons])^2
predict z4Am_`num', fitted 


mixed outcome4 ///
	i.mcats $parvar  c.dep_qt c.mus_qt c.eu_qt ///
	pubsector parttime ib1.major c.lfs4eu_qt if fem == `num' & $miss4 ///
	|| occ10_master:, baselevels
est store z4B_`num' 
estimates save "z4B_`num'.ster", replace 
scalar z4Bsigma2_`num' = exp(_b[lns1_1_1:_cons])^2
predict z4Bm_`num', fitted 
predict z4Bu_`num', reffect reses(z4Buse_`num')
predict z4Bxbse_`num', stdp 
compress 
save "$pnew/z_individual_4_`num'.dta", replace 
} 


esttab z2A_0 z2B_0 z2A_1 z2B_1 z3A_0 z3B_0 z3A_1 z3B_1 z4A_0 z4B_0 z4A_1  z4B_1 , ///
 b(%8.2f) not stat(N r2) label


esttab z2A_0 z2B_0 z2A_1 z2B_1 z3A_0 z3B_0 z3A_1 z3B_1 z4A_0 z4B_0 z4A_1  z4B_1 , ///
 b(%8.2f) not stat(N r2) label
 
 
 
 esttab z2A_0 z2B_0 z3A_0 z3B_0 z4A_0 z4B_0, ///
 b(%8.2f) not stat(N r2) label
 
 
 esttab z2A_1 z2B_1 z3A_1 z3B_1 z4A_1  z4B_1, ///
 b(%8.2f) not stat(N r2) label
 

// foreach num of numlist 0 1 {
// 	use $pnew/nuet.dta, clear
// * Outcome 5
// global vars5 "outcome5 mcats fem dvage_ind educscore gor_dv urban wave dep_qt mus_qt eu_qt pubsector parttime lfs4eu_qt" 
// capture drop miss5
// egen miss5 = rmiss($vars5)
// global miss5 "miss5 == 0"
// keep if miss5 == 0
// keep if fem == `num'
//
// melogit outcome5 if fem == `num' & $miss5 || occ10_master:, or baselevels 
// est store z5A_`num' 
// est save "z5A_`num'.ster", replace 
// scalar z5Asigma2_`num' = _b[/var(_cons[occ10_master])]
// predict z5Axbu_`num', eta 
// predict z5Axb_`num', xb
//
// melogit outcome5  i.mcats $parvar  c.dep_qt c.mus_qt c.eu_qt ///
// pubsector parttime ib1.major  c.lfs4eu_qt if ///
// fem == `num' & $miss5 || occ10_master:, or baselevels 
// est store z5B_`num' 
// est save "z5B_`num'.ster", replace 
// scalar z5Bsigma2_`num' = _b[/var(_cons[occ10_master])]
// predict z5Bxbu_`num', eta 
// predict z5Bxb_`num', xb
// predict z5Bxbse_`num', stdp 
// predict z5Bu_`num', reffect reses(z5Buse_`num')
// compress 
// save "$pnew/z_individual_5_`num'.dta", replace 
// }
//


educscore engprof liberal gor_dv mar child health cardum ties religiosity wave dep_qt mus_qt eu_qt pubsector parttime major lfs_soc4_p04 lfs4eu_qt



*-------------------------------------------------------------------------------
**## Save the data at the occupation level 
*-------------------------------------------------------------------------------

foreach num of numlist 2 3 4  { 
	use "$pnew/z_individual_`num'_0.dta", clear
	collapse (count) n = outcome`num' ///
	(mean) outcome`num', ///
	by(occ10_master mcats fem dvage_ind educscore engprof liberal gor_dv mar child health cardum ties sity wave dep_qt mus_qt eu_qt pubsector parttime major lfs4eu_qt ///
	z`num'* mus_occ lfs_soc4_p04  )
	capture drop N_`num'
	bysort occ10_master : generate N_`num' = _N
	capture drop s_tag_`num'
	egen s_tag_`num' = tag(occ10_master)
	keep if s_tag_`num'
    save "$pnew/z_occupation_`num'_0.dta", replace 

}

foreach num of numlist 2 3 4  { 
	use "$pnew/z_individual_`num'_1.dta", clear
	collapse (count) n = outcome`num' ///
	(mean) outcome`num', ///
	by(occ10_master mcats fem dvage_ind educscore engprof liberal gor_dv mar child health cardum ties sity wave dep_qt mus_qt eu_qt pubsector parttime major  lfs4eu_qt ///
	z`num'* mus_occ lfs_soc4_p04  )
	capture drop N_`num'
	bysort occ10_master : generate N_`num' = _N
	capture drop s_tag_`num'
	egen s_tag_`num' = tag(occ10_master)
	keep if s_tag_`num'
    save "$pnew/z_occupation_`num'_1.dta", replace 
}
 

 ** Model results 
 
 foreach num of numlist 0 1 { 	
 use "$pnew/z_individual_2_`num'.dta", clear 
 estimates use "z2A_`num'.ster" 
 estimates replay, baselevels 
 estat icc 
 estimates use "z2B_`num'.ster"
 estimates replay, baselevels 
 estat icc 
 display %3.1f 100*(z2Asigma2_`num' - z2Bsigma2_`num') / z2Asigma2_`num'
 
 
 }
 
  foreach num of numlist 0 1 { 	
 use "$pnew/z_individual_3_`num'.dta", clear 
 estimates use "z3A_`num'.ster" 
 estimates replay, baselevels 
 estat icc 
 estimates use "z3B_`num'.ster"
 estimates replay, baselevels 
 estat icc 
 display %3.1f 100*(z3Asigma2_`num' - z3Bsigma2_`num') / z3Asigma2_`num'
 
 }
 
  foreach num of numlist 0 1 { 	
 use "$pnew/z_individual_4_`num'.dta", clear 
 estimates use "z4A_`num'.ster" 
 estimates replay, baselevels 
 estat icc 
 estimates use "z4B_`num'.ster"
 estimates replay, baselevels 
 estat icc 
 display %3.1f 100*(z4Asigma2_`num' - z4Bsigma2_`num') / z4Asigma2_`num'
 
 }
 
//   foreach num of numlist 0 1 { 	
//  use "$pnew/z_individual_5_`num'.dta", clear 
//  estimates use "z5A_`num'.ster" 
//  estimates replay, or baselevels 
//  estat icc 
//  roctab outcome5 z5Axb_`num'
//  roctab outcome5 z5Axbu_`num'
// 
//  estimates use "z2B_`num'.ster"
//  estimates replay, baselevels 
//  estat icc 
//  roctab outcome5 z5Bxb_`num'
//  roctab outcome5 z5Bxbu_`num'
// 
//  }

* Plots 


* Scatter 

	use "$pnew/z_individual_2_0.dta", clear
	collapse (count) n = outcome2 ///
	(mean) outcome2, ///
	by(occ10_master major mus_occ lfs_soc4_p04)
	capture drop N
	bysort occ10_master : generate N = _N
	capture drop s_tag
	egen s_tag = tag(occ10_master)
	keep if s_tag
    save "$pnew/for_scatter.dta", replace 
	
	su mus_occ lfs_soc4_p04
	capture drop tlm
	gen tlm = mus_occ + 1
	capture drop tmm
	gen tmm = lfs_soc4_p04 + 1
	su tlm tmm
	capture drop log_mus
	gen log_mus =  log(tlm) 
	capture drop log_p04 
	gen log_p04 = log(tmm)
	
	scatter log_mus log_p04, jitter(1) ///
	colorvar(major) msize(large) colorrule(%50) colordiscrete coloruseplegend ///
	plegend(label(9 "Elemtary occs.") label(8 "Process, plant & machine ops.") ///
	label(7 "Sales and customer service") label(6 "Personal service") ///
	label(5 "Skilled trades") label(4 "Admin. & Secretarial") ///
	label(3 "Associate & tech.") label(2 "Professional") ///
	label(1 "Managers and seinor officials")) ///
	ytitle( Log share of Muslims) xtitle( Log share of Post-04 Migrants)
	
	histogram log_mus , name(one, replace)
	histogram log_p04 , name(two, replace)
	


* Outcome 2 
local title0 "{bf: Men}"
local title1 "{bf: Women}"	  
local let0 "{bf: (A)}"
local let1 "{bf: (C)}"

foreach n of numlist 0 1 { 
	use $pnew/z_occupation_2_`n'.dta, clear 
	
	* Rank the predicted stratum random effects
egen z2Burank_`n'  = rank(z2Bu_`n' )
generate z2Bulo_`n' = z2Bu_`n'  - 1.96 * z2Buse_`n' 
generate z2Buhi_`n'  = z2Bu_`n'  + 1.96 * z2Buse_`n'
generate z2Busig_`n' = (z2Buhi_`n' < 0 | z2Bulo_`n' > 0)
	local letter " `let`n'' "
	local gender " `title`n'' "	
	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)
twoway  ///
	(rspike z2Buhi_`n' z2Bulo_`n' z2Burank_`n' , lcolor(gs4*0.45)) ///
	(scatter z2Bu_`n'  z2Burank_`n' if mus_qt == 1 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.20%80) mlcolor(gs4)) ///
	(scatter z2Bu_`n'  z2Burank_`n' if mus_qt == 2 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z2Bu_`n'  z2Burank_`n' if mus_qt == 3 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.60%80) mlcolor(gs4)) ///
	(scatter z2Bu_`n'  z2Burank_`n' if mus_qt == 4 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.80%80)  mlcolor(gs4)) ///
	(scatter z2Bu_`n'  z2Burank_`n' if mus_qt == 5 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.90%80) mlcolor(gs4)),	///
	ytitle("") ///
	xtitle("") subtitle(" `letter' `gender' ", pos(11)) ///
	yline(0, lcolor(pink) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF2_`n', replace) ///
	xsize(5)
	graph save $pgraphs/all_2_`n'.gph, replace 
	
	format %9.2f z2Bu_`n' z2Bulo_`n' z2Buhi_`n'
	sort z2Burank_`n'
	list z2Burank_`n'  occ10_master N mus_occ lfs_soc4_p04  ///
	z2Bu_`n' z2Bulo_`n' z2Buhi_`n'

}

 local title0 "{bf: Men}"
 local title1 "{bf: Women}"
 local let0 "{bf: (B)}"
 local let1 "{bf: (D)}"
foreach n of numlist 0 1 { 
	use $pnew/z_occupation_2_`n'.dta, clear 
	generate z2Bulo_`n' = z2Bu_`n' - 1.96 * z2Buse_`n'
	generate z2Buhi_`n' = z2Bu_`n' + 1.96 * z2Buse_`n'
	generate z2Busig_`n' = (z2Buhi_`n' < 0 | z2Bulo_`n' > 0)
	egen z2Busigrank_`n' = rank(z2Bu_`n') if z2Busig_`n' == 1
	local letter " `let`n'' "
	local gender " `title`n'' "
	egen z2Burank_`n'  = rank(z2Bu_`n' )

	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)
	
	
	gen labs = occ10_master
	
	twoway ///
	(rspike z2Bulo_`n' z2Buhi_`n'  z2Busigrank_`n' , lpat(dash) lcolor(gs4*0.45)) ///
	(scatter z2Bulo_`n' z2Busigrank_`n' if z2Buhi_`n' < 0 & mus_qt == 1 [fw=N], ///
		ms(O) mc(gs4*0.20%80) mlcolor(gs4)) /// 
	(scatter z2Bulo_`n' z2Busigrank_`n' if z2Buhi_`n' < 0 & mus_qt == 2 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z2Bulo_`n' z2Busigrank_`n' if z2Buhi_`n' < 0 & mus_qt == 3 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)) ///
	(scatter z2Bulo_`n' z2Busigrank_`n' if z2Buhi_`n' < 0 & mus_qt == 4 [fw=N], ///
		ms(O) mc(gs4*0.80%80) mlcolor(gs4)) ///
	(scatter z2Bulo_`n' z2Busigrank_`n' if z2Buhi_`n' < 0 & mus_qt == 5 [fw=N], ///
		ms(O) mc(gs4*0.95%80) mlcolor(gs4)) ///
	(scatter z2Buhi_`n' z2Busigrank_`n' if z2Bulo_`n' > 0 & mus_qt == 1 [fw=N], ///
		ms(O) mc(gs4*0.20%80) mlcolor(gs4)) /// 
	(scatter z2Buhi_`n' z2Busigrank_`n' if z2Bulo_`n' > 0 & mus_qt == 2 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) /// 
	(scatter z2Buhi_`n' z2Busigrank_`n' if z2Bulo_`n' > 0 & mus_qt == 3 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)) /// 
	(scatter z2Buhi_`n' z2Busigrank_`n' if z2Bulo_`n' > 0 & mus_qt == 4 [fw=N], ///
		ms(O) mc(gs4*0.80%80) mlcolor(gs4)) /// 
	(scatter z2Buhi_`n' z2Busigrank_`n' if z2Bulo_`n' > 0 & mus_qt == 5 [fw=N], ///
		ms(O) mc(gs4*0.95%80) mlcolor(gs4)) ///
	(scatter z2Bulo_`n' z2Busigrank_`n' if z2Buhi_`n' < 0 & mus_qt == 5 [fw=N], ///
		ms(none) mlab(labs) mlabangle(ninety) mlabposition(0) mlabcolor(lime) mlabsize(tiny)) ///	
     (scatter z2Buhi_`n' z2Busigrank_`n' if z2Buhi_`n' > 0 & mus_qt == 5 [fw=N], ///
		ms(none) mlab(labs) mlabangle(ninety) mlabposition(0) mlabcolor(lime) mlabsize(tiny)), ///
	ytitle("") ///
	xtitle("") subtitle(" `letter' `gender' (sig.)", pos(11) ) ///
	yline(0, lcolor(pink) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF2_`n', replace) ///
	xsize(5)
	graph save $pgraphs/sig_2_`n'.gph, replace 
	
	format %9.2f z2Bu_`n' z2Bulo_`n' z2Buhi_`n'
	sort z2Busigrank_`n'
	list z2Busigrank_`n' occ10_master N mus_occ lfs_soc4_p04  ///
	z2Bu_`n' z2Bulo_`n' z2Buhi_`n' if mus_qt == 5 & z2Busigrank_`n' != . 


}

****

					/// OUTCOME 3 FINANCIAL ANXIETY

*** Outcome 3		
local title0 "{bf: Men}"
local title1 "{bf: Women}"	  
local let0 "{bf: (A)}"
local let1 "{bf: (C)}"  
 
foreach n of numlist 0 1 { 
	use $pnew/z_occupation_3_`n'.dta, clear 
	
	* Rank the predicted stratum random effects
egen z3Burank_`n'  = rank(z3Bu_`n' )
generate z3Bulo_`n' = z3Bu_`n'  - 1.96 * z3Buse_`n' 
generate z3Buhi_`n'  = z3Bu_`n'  + 1.96 * z3Buse_`n'
generate z3Busig_`n' = (z3Buhi_`n' < 0 | z3Bulo_`n' > 0)

	local letter " `let`n'' "
	local gender " `title`n'' "
	local lab " `lab`n' " 
	
	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)


twoway  ///
	(rspike z3Buhi_`n' z3Bulo_`n' z3Burank_`n' , lcolor(gs4*0.45)) ///
	(scatter z3Bu_`n'  z3Burank_`n' if mus_qt == 1 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.20%80) mlcolor(gs4)) ///
	(scatter z3Bu_`n'  z3Burank_`n' if mus_qt == 2 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z3Bu_`n'  z3Burank_`n' if mus_qt == 3 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.60%80) mlcolor(gs4)) ///
	(scatter z3Bu_`n'  z3Burank_`n' if mus_qt == 4 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.80%80)  mlcolor(gs4)) ///
	(scatter z3Bu_`n'  z3Burank_`n' if mus_qt == 5 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.90%80) mlcolor(gs4)),	///
	ytitle("") ///
	xtitle("") subtitle(" `letter' `gender' ", pos(11)) ///
	yline(0, lcolor(gold) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF2_`n', replace) ///
	xsize(5)
	graph save $pgraphs/all_3_`n'.gph, replace 

}



use $pnew/z_occupation_3_0.dta, clear 
	generate z3Bulo_0 = z3Bu_0 - 1.96 * z3Buse_0
	generate z3Buhi_0 = z3Bu_0 + 1.96 * z3Buse_0
	generate z3Busig_0 = (z3Buhi_0 < 0 | z3Bulo_0 > 0)
	egen z3Busigrank_0 = rank(z3Bu_0) if z3Busig_0 == 1
	local letter " `let`n'' "
	local gender " `title`n'' "
	egen z3Burank_0  = rank(z3Bu_0 )

	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)
	
	format %9.2f z3Bu_0 z3Bulo_0 z3Buhi_0
	sort z3Busigrank_0
	list   N mus_qt occ10_master mus_occ lfs_soc4_p04 z3Busigrank_0 ///
	z3Bu_0 z3Bulo_0 z3Buhi_0  if z3Busigrank_0 != .

	gen labs = occ10_master
	
	twoway ///
	(rspike z3Bulo_0 z3Buhi_0  z3Busigrank_0 , lpat(dash) lcolor(gs4*0.45)) ///
	(scatter z3Bulo_0 z3Busigrank_0 if z3Buhi_0 < 0 & mus_qt == 2 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z3Bulo_0 z3Busigrank_0 if z3Buhi_0 < 0 & mus_qt == 3 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)) ///
	(scatter z3Bulo_0 z3Busigrank_0 if z3Buhi_0 < 0 & mus_qt == 5 [fw=N], ///
		ms(O) mc(gs4*0.95%80) mlcolor(gs4)) ///
	(scatter z3Buhi_0 z3Busigrank_0 if z3Bulo_0 > 0 & mus_qt == 2 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) /// 
	(scatter z3Buhi_0 z3Busigrank_0 if z3Bulo_0 > 0 & mus_qt == 3 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)) /// 
	(scatter z3Buhi_0 z3Busigrank_0 if z3Bulo_0 > 0 & mus_qt == 4 [fw=N], ///
		ms(O) mc(gs4*0.80%80) mlcolor(gs4)) /// 
	(scatter z3Buhi_0 z3Busigrank_0 if z3Bulo_0 > 0 & mus_qt == 5 [fw=N], ///
		ms(O) mc(gs4*0.95%80) mlcolor(gs4)) ///
	(scatter z3Bulo_0 z3Busigrank_0 if z3Buhi_0 < 0 & mus_qt == 5 [fw=N], ///
		ms(none) mlab(labs) mlabangle(ninety) mlabposition(0) mlabcolor(lime) mlabsize(tiny)) ///	
     (scatter z3Buhi_0 z3Busigrank_0 if z3Buhi_0 > 0 & mus_qt == 5 [fw=N], ///
		ms(none) mlab(labs) mlabangle(ninety) mlabposition(0) mlabcolor(lime) mlabsize(tiny)), ///
	ytitle("") ///
	xtitle("") subtitle(" (B) Men (sig.)", pos(11) ) ///
	yline(0, lcolor(gold) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF3_0, replace) ///
	xsize(5)
	graph save $pgraphs/sig_3_0.gph, replace 
	
	format %9.2f z3Bu_0 z3Bulo_0 z3Buhi_0
	sort z3Busigrank_0
	list z3Busigrank_0 occ10_master N mus_occ lfs_soc4_p04  ///
	z3Bu_0 z3Bulo_0 z3Buhi_0 if mus_qt == 5 & z3Busigrank_0 != . 


	use $pnew/z_occupation_3_1.dta, clear 
	generate z3Bulo_1 = z3Bu_1 - 1.96 * z3Buse_1
	generate z3Buhi_1 = z3Bu_1 + 1.96 * z3Buse_1
	generate z3Busig_1 = (z3Buhi_1 < 0 | z3Bulo_1 > 0)
	egen z3Busigrank_1 = rank(z3Bu_1) if z3Busig_1 == 1
	local letter " `let`n'' "
	local gender " `title`n'' "
	egen z3Burank_1  = rank(z3Bu_1 )

	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)
	
	format %9.2f z3Bu_1 z3Bulo_1 z3Buhi_1
	sort z3Busigrank_1
	list   N mus_qt occ10_master mus_occ lfs_soc4_p04 z3Busigrank_1 ///
	z3Bu_1 z3Bulo_1 z3Buhi_1  if z3Busigrank_1 != .

	gen labs = occ10_master
	
	twoway ///
	(rspike z3Bulo_1 z3Buhi_1  z3Busigrank_1 , lpat(dash) lcolor(gs4*0.45)) ///
	(scatter z3Bulo_1 z3Busigrank_1 if z3Buhi_1 < 0 & mus_qt == 1 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z3Bulo_1 z3Busigrank_1 if z3Buhi_1 < 0 & mus_qt == 2 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z3Bulo_1 z3Busigrank_1 if z3Buhi_1 < 0 & mus_qt == 3 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)) ///
	(scatter z3Bulo_1 z3Busigrank_1 if z3Buhi_1 < 0 & mus_qt == 4 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)) ///
	(scatter z3Bulo_1 z3Busigrank_1 if z3Buhi_1 < 0 & mus_qt == 5 [fw=N], ///
		ms(O) mc(gs4*0.95%80) mlcolor(gs4)) ///
		(scatter z3Buhi_1 z3Busigrank_1 if z3Bulo_1 > 0 & mus_qt == 1 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z3Buhi_1 z3Busigrank_1 if z3Bulo_1 > 0 & mus_qt == 2 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) /// 
	(scatter z3Buhi_1 z3Busigrank_1 if z3Bulo_1 > 0 & mus_qt == 3 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)) /// 
	(scatter z3Buhi_1 z3Busigrank_1 if z3Bulo_1 > 0 & mus_qt == 4 [fw=N], ///
		ms(O) mc(gs4*0.80%80) mlcolor(gs4)) /// 
	(scatter z3Buhi_1 z3Busigrank_1 if z3Bulo_1 > 0 & mus_qt == 5 [fw=N], ///
		ms(O) mc(gs4*0.95%80) mlcolor(gs4)) ///
	(scatter z3Bulo_1 z3Busigrank_1 if z3Buhi_1 < 0 & mus_qt == 5 [fw=N], ///
		ms(none) mlab(labs) mlabangle(ninety) mlabposition(0) mlabcolor(lime) mlabsize(tiny)) ///	
     (scatter z3Buhi_1 z3Busigrank_1 if z3Buhi_1 > 0 & mus_qt == 5 [fw=N], ///
		ms(none) mlab(labs) mlabangle(ninety) mlabposition(0) mlabcolor(lime) mlabsize(tiny)), ///
	ytitle("") ///
	xtitle("") subtitle(" (D) Women (sig.)", pos(11) ) ///
	yline(0, lcolor(gold) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF3_1, replace) ///
	xsize(5)
	graph save $pgraphs/sig_3_1.gph, replace 
	
	format %9.2f z3Bu_1 z3Bulo_1 z3Buhi_1
	sort z3Busigrank_1
	list z3Busigrank_1 occ10_master N mus_occ lfs_soc4_p04  ///
	z3Bu_1 z3Bulo_1 z3Buhi_1 if mus_qt == 5 & z3Busigrank_1 != . 


					/// OUTCOME 4 JOB RELATED SATISFACTION
					

local title0 "{bf: Men}"
local title1 "{bf: Women}"	  
local let0 "{bf: (A)}"
local let1 "{bf: (C)}"  
 
foreach n of numlist 0 1 { 
	use $pnew/z_occupation_4_`n'.dta, clear 
	
	* Rank the predicted stratum random effects
egen z4Burank_`n'  = rank(z4Bu_`n' )
generate z4Bulo_`n' = z4Bu_`n'  - 1.96 * z4Buse_`n' 
generate z4Buhi_`n'  = z4Bu_`n'  + 1.96 * z4Buse_`n'
generate z4Busig_`n' = (z4Buhi_`n' < 0 | z4Bulo_`n' > 0)

	local letter " `let`n'' "
	local gender " `title`n'' "
	local lab " `lab`n' " 
	
	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)


twoway  ///
	(rspike z4Buhi_`n' z4Bulo_`n' z4Burank_`n' , lcolor(gs4*0.45)) ///
	(scatter z4Bu_`n'  z4Burank_`n' if mus_qt == 1 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.20%80) mlcolor(gs4)) ///
	(scatter z4Bu_`n'  z4Burank_`n' if mus_qt == 2 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z4Bu_`n'  z4Burank_`n' if mus_qt == 3 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.60%80) mlcolor(gs4)) ///
	(scatter z4Bu_`n'  z4Burank_`n' if mus_qt == 4 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.80%80)  mlcolor(gs4)) ///
	(scatter z4Bu_`n'  z4Burank_`n' if mus_qt == 5 [fweight=N], ///
		msymbol(O) mcolor(gs4*0.90%80) mlcolor(gs4)),	///
	ytitle("") ///
	xtitle("") subtitle(" `letter' `gender' ", pos(11)) ///
	yline(0, lcolor(gold) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF2_`n', replace) ///
	xsize(5)
	graph save $pgraphs/all_4_`n'.gph, replace 
	
	sort z4Brank_`n'
	list   N mus_qt occ10_master mus_occ lfs_soc4_p04 z4Busigrank_0 ///
	z4Bu_0 z4Bulo_0 z4Buhi_0  if z4Busigrank_0 != .

}

					


use $pnew/z_occupation_4_0.dta, clear 
	generate z4Bulo_0 = z4Bu_0 - 1.96 * z4Buse_0
	generate z4Buhi_0 = z4Bu_0 + 1.96 * z4Buse_0
	generate z4Busig_0 = (z4Buhi_0 < 0 | z4Bulo_0 > 0)
	egen z4Busigrank_0 = rank(z4Bu_0) if z4Busig_0 == 1
	local letter " `let`n'' "
	local gender " `title`n'' "
	egen z4Burank_0  = rank(z4Bu_0 )

	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)
	
	format %9.2f z4Bu_0 z4Bulo_0 z4Buhi_0
	sort z4Busigrank_0
	list   N mus_qt occ10_master mus_occ lfs_soc4_p04 z4Busigrank_0 ///
	z4Bu_0 z4Bulo_0 z4Buhi_0  if z4Busigrank_0 != .

	gen labs = occ10_master
	
	twoway ///
	(rspike z4Bulo_0 z4Buhi_0  z4Busigrank_0 , lpat(dash) lcolor(gs4*0.45)) ///
	(scatter z4Bulo_0 z4Busigrank_0 if z4Buhi_0 < 0 & mus_qt == 2 [fw=N], ///
		ms(O) mc(gs4*0.40%80) mlcolor(gs4)) ///
	(scatter z4Bulo_0 z4Busigrank_0 if z4Buhi_0 < 0 & mus_qt == 4 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)), ///
	ytitle("") ///
	xtitle("") subtitle(" (B) Men (sig.)", pos(11) ) ///
	yline(0, lcolor(gold) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF3_0, replace) ///
	xsize(5)
	graph save $pgraphs/sig_4_0.gph, replace 
	
	format %9.2f z4Bu_0 z4Bulo_0 z4Buhi_0
	sort z4Busigrank_0
	list z4Busigrank_0 occ10_master N mus_occ lfs_soc4_p04  ///
	z4Bu_0 z4Bulo_0 z4Buhi_0  if z4Busigrank_0 != . 


	use $pnew/z_occupation_4_1.dta, clear 
	generate z4Bulo_1 = z4Bu_1 - 1.96 * z4Buse_1
	generate z4Buhi_1 = z4Bu_1 + 1.96 * z4Buse_1
	generate z4Busig_1 = (z4Buhi_1 < 0 | z4Bulo_1 > 0)
	egen z4Busigrank_1 = rank(z4Bu_1) if z4Busig_1 == 1
	local letter " `let`n'' "
	local gender " `title`n'' "
	egen z4Burank_1  = rank(z4Bu_1 )

	capture drop mus_qt
	xtile mus_qt = mus_occ, nq(5)
	
	format %9.2f z4Bu_1 z4Bulo_1 z4Buhi_1
	sort z4Busigrank_1
	list   N mus_qt occ10_master mus_occ lfs_soc4_p04 z4Busigrank_1 ///
	z4Bu_1 z4Bulo_1 z4Buhi_1  if z4Busigrank_1 != .

	gen labs = occ10_master
	
	twoway ///
	(rspike z4Bulo_1 z4Buhi_1  z4Busigrank_1 , lpat(dash) lcolor(gs4*0.45)) ///
	(scatter z4Bulo_1 z4Busigrank_1 if z4Buhi_1 < 0 & mus_qt == 4 [fw=N], ///
		ms(O) mc(gs4*0.60%80) mlcolor(gs4)),  ///
	ytitle("") ///
	xtitle("") subtitle(" (D) Women (sig.)", pos(11) ) ///
	yline(0, lcolor(gold) lwidth(thick)) ///
	xlabel(none) ///
	legend(off) ///
	scheme(s1mono) ///
	name(RF3_1, replace) ///
	xsize(5)
	graph save $pgraphs/sig_4_1.gph, replace 
	
	format %9.2f z4Bu_1 z4Bulo_1 z4Buhi_1
	sort z4Busigrank_1
	list z4Busigrank_1 occ10_master N mus_occ lfs_soc4_p04  ///
	z4Bu_1 z4Bulo_1 z4Buhi_1 if  z4Busigrank_1 != . 
	
	

	
				/// GRAPH COMBINE
graph combine $pgraphs/all_2_0.gph $pgraphs/sig_2_0.gph $pgraphs/all_2_1.gph  $pgraphs/sig_2_1.gph , row(4) col(1) xsize(4) ysize(7) title("Predicted Average Hourly Earnings", pos(9) orientation(vertical) size(*0.65)) subtitle("SOC10 Unit Group Rank", pos(6) size(*0.85))  iscale(*0.75)
graph save $pgraphs/ff2.gph, replace 
graph save $pgraphs/ff2.png, replace 
graph combine $pgraphs/all_3_0.gph $pgraphs/sig_3_0.gph $pgraphs/all_3_1.gph  $pgraphs/sig_3_1.gph, row(4) col(1) xsize(4) ysize(7) title("Predicted Financial Anxiety", pos(9) orientation(vertical) size(*0.65)) subtitle("SOC10 Unit Group Rank", pos(6) size(*0.85)) iscale(*0.75)
graph save $pgraphs/ff3.gph, replace 
graph save $pgraphs/ff3.png, replace 
graph combine $pgraphs/all_4_0.gph $pgraphs/sig_4_0.gph $pgraphs/all_4_1.gph  $pgraphs/sig_4_1.gph, row(4) col(1) xsize(4) ysize(7) title("Predicted Job Related Satisfaction", pos(9) orientation(vertical) size(*0.65)) subtitle("SOC10 Unit Group Rank", pos(6) size(*0.85))  iscale(*0.75)
graph save $pgraphs/ff4.gph, replace 
graph save $pgraphs/ff4.png, replace 



graph combine $pgraphs/ff2.gph $pgraphs/ff3.gph , row(1) 
