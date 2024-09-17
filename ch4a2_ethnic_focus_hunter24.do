** ************************************************************************** **
	* Chapter 4 Analysis 2 - Exploring religious differences within ethnic groups.
	* MGMT File
	* Part of PhD research on assessing impact of contemporary migration 
	* patterns the Muslim community in the UK. 
	* Last updated 1 March 2024
	* Scot Hunter 
** ************************************************************************** **


use $pnew/file_models_update.dta, clear

tab freshcats

// Indian sub-groups 
	* Muslim Indian (9)
	* Hindu Indian (15)
	* Sikh Indian  (16)
	* Muslin Indian 2 (29)
	* Hindu Indian 2 (215)
	* Sikh Indian 2 (216)
	* Other indians? 
label define is2c 0 "Muslim 2" 10 "Hindu 1" 2 "Sikh 1" 3 "Muslim 1" 4 "Hindu 2" 5 "Sikh 2"
capture drop indian_subs_2 
gen indian_subs_2 = . 
replace indian_subs_2 = 0 if freshcats == 29
replace indian_subs_2 = 1 if freshcats == 15 
replace indian_subs_2 = 2 if freshcats == 16 
replace indian_subs_2 = 3 if freshcats == 9
replace indian_subs_2 = 4 if freshcats == 215
replace indian_subs_2 = 5 if freshcats == 216
label values indian_subs_2 is2c

tab ethn_dv ff_religion if indian_subs_2 != ., m
* there are 4000 unaccounted for Indians? 
* 1463 indians no religion 
* 1278 indians christian 
* buddhist + other


* regs. 
global socovars "health cardum dvage age2 mar ib0.child $regdums educscore engprof ties liberal religiosity"
su $socovars 
*camsis
regress mcam10 i.indian_subs_2 $socovars   ///
i.wave if  participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store imen2
regress mcam10 i.indian_subs_2 $socovars   ///
i.wave if fem == 1 & participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store ifem2

esttab imen2 ifem2, b(%8.2f) se(%8.2f)  keep(*indian_subs*) stats(N r2 ll) star
label define bigr ///
0 "Muslim 2" 1 "Hindu 1" 2 "Sikh 1" 3 "Muslim 1" ///
4 "Hindu 2" 5 "Sikh 2"   6  "Christian/No Relg 1" ///
7 "Christian/No Relg 2"  8 "Other Relg 1" 9 "Other Relg 2" 
capture drop indian_subs_3
gen indian_subs_3 = . 
replace indian_subs_3 = 0 if freshcats == 29 
replace indian_subs_3 = 1 if freshcats == 15 
replace indian_subs_3 = 2 if freshcats == 16 
replace indian_subs_3 = 3 if freshcats == 9
replace indian_subs_3 = 4 if freshcats == 215
replace indian_subs_3 = 5 if freshcats == 216
replace indian_subs_3 = 6 if ethn_dv == 9 & ff_religion == 0 & generation == 1 | ethn_dv == 9 & ff_religion == 2 & generation == 1
replace indian_subs_3 = 7 if ethn_dv == 9 & ff_religion == 0 & generation == 2 | ethn_dv == 9 & ff_religion == 2 & generation == 2
replace indian_subs_3 = 8 if ethn_dv == 9 & ff_religion == 14 & generation == 1 | ethn_dv == 9 & ff_religion == 16 & generation == 1 | ethn_dv == 9 & ff_religion == 97 & generation == 1 | ethn_dv == 9 & ff_religion == . & generation == 1
replace indian_subs_3 = 9 if ethn_dv == 9 & ff_religion == 14 & generation == 2 | ethn_dv == 9 & ff_religion == 16 & generation == 2 | ethn_dv == 9 & ff_religion == 97 & generation == 2 | ethn_dv == 9 & ff_religion == . & generation == 2
label values indian_subs_3 bigr
tab ethn_dv indian_subs_3
tab ff_religion indian_subs_3
** black community 
tab ethn_dv ff_religion, m
capture drop black_subs 
gen black_subs = . 
replace black_subs = 1 if ethn_dv == 14 & ff_religion == 0 & generation == 1 | ethn_dv == 14 & ff_religion == 2 & generation == 1 // caribbean christian gen 1 
replace black_subs = 2 if ethn_dv == 14 & ff_religion == 0 & generation == 2 | ethn_dv == 14 & ff_religion == 2 & generation == 2 // caribbean christian gen 2 
// carribbean other 1
replace black_subs = 3 if ethn_dv == 14 & ff_religion == 13 & generation == 1 | ethn_dv == 14 & ff_religion == 14 & generation == 1 | ethn_dv == 14 & ff_religion == 15 & generation == 1 | ethn_dv == 14 & ff_religion == 16 & generation == 1 | ethn_dv == 14 & ff_religion == 97 & generation == 1 | ethn_dv == 14 & ff_religion == . & generation == 1
// carribbean other 2
replace black_subs = 4 if ethn_dv == 14 & ff_religion == 13 & generation == 2 | ethn_dv == 14 & ff_religion == 14 & generation == 2 | ethn_dv == 14 & ff_religion == 15 & generation == 2 | ethn_dv == 14 & ff_religion == 16 & generation == 2 | ethn_dv == 14 & ff_religion == 97 & generation == 2 | ethn_dv == 14 & ff_religion == . & generation == 2
replace black_subs = 5 if ethn_dv == 15 & ff_religion == 0 & generation == 1 | ethn_dv == 15 & ff_religion == 2 & generation == 1 // caribbean christian gen 1 
replace black_subs = 6 if ethn_dv == 15 & ff_religion == 0 & generation == 2 | ethn_dv == 15 & ff_religion == 2 & generation == 2 // caribbean christian gen 2 
// african other 1
replace black_subs = 7 if ethn_dv == 15 & ff_religion == 13 & generation == 1 | ethn_dv == 15 & ff_religion == 14 & generation == 1 | ethn_dv == 15 & ff_religion == 15 & generation == 1 | ethn_dv == 15 & ff_religion == 16 & generation == 1 | ethn_dv == 15 & ff_religion == 97 & generation == 1 | ethn_dv == 15 & ff_religion == . & generation == 1
// african other 2
replace black_subs = 8 if ethn_dv == 15 & ff_religion == 13 & generation == 2 | ethn_dv == 15 & ff_religion == 14 & generation == 2 | ethn_dv == 15 & ff_religion == 15 & generation == 2 | ethn_dv == 15 & ff_religion == 16 & generation == 2 | ethn_dv == 15 & ff_religion == 97 & generation == 2 | ethn_dv == 15 & ff_religion == . & generation == 2
replace black_subs = 0 if freshcats == 12
label define bls 1 "Caribbean Christ. 1 " 2 "Car Christ. 2" 3 "Carib. Oth. 1" 4 "Carib. Oth. 2" ///
5 "Afr. Chr. 1" 6 "Afr. Chr 2." 7 "Afr. Oth. 1" 8 "Arg Oth. 2"
label values black_subs bls


label define pbl 0 "Muslim P&B 2" 1 "Muslim P&B 1" 2 "Hindu P&B 1" 3 "Hindu P&B 2" ///
4 "Christ. P&B 1" 5 "Chirst. P&B 2"
capture drop pb_subs 
gen pb_subs = . 
replace pb_subs  = 0 if redcats == 10 | redcats == 11
replace pb_subs  = 1 if redcats == 210 | redcats == 211 
replace pb_subs  = 2 if ethn_dv == 10 & ff_religion == 13 & generation == 1 | ethn_dv == 11 & ff_religion == 13 & generation == 1 
replace pb_subs  = 3 if ethn_dv == 10 & ff_religion == 13 & generation == 2 | ethn_dv == 11 & ff_religion == 13 & generation == 2 
replace pb_subs  = 4 if ethn_dv == 10 & ff_religion == 2 & generation == 1 | ethn_dv == 11 & ff_religion == 2 & generation == 1 
replace pb_subs  = 5 if ethn_dv == 10 & ff_religion == 2 & generation == 2 | ethn_dv == 11 & ff_religion == 2 & generation == 2 
label values pb_subs pbl 


***********  ********************
** ************************************************************************** **
	* ANALYSIS (x1) - Additional analysis - comparing groups within Indian
	* and Black community in the UKHLS. 
	* Additional Analysis File 1. 
	* Part of PhD research on assessing impact of contemporary migration 
	* patterns on the Muslim community in the UK. 
	* Last updated 4 Apr 2024
	* Scot Hunter 
** ************************************************************************** **

global socovars "health cardum dvage age2 mar ib0.child $regdums educscore engprof ties liberal religiosity"

** INDIAN COMMUNITY
* run respective models with Second-generation Muslim Indian as ref cat. 
*paricipation - 
logit participation i.indian_subs_3 i.wave $socovars ///
if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store menpar 
logit participation i.indian_subs_3 i.wave $socovars ///
if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fempar 
*employment 
logit employment i.indian_subs_3 $socovars i.wave ///
if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store menemp
logit employment i.indian_subs_3 $socovars i.wave ///
if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store fememp
*camsis
regress mcam10 i.indian_subs_3 $socovars   ///
i.wave if  participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store mencam
regress mcam10 i.indian_subs_3 $socovars   ///
i.wave if fem == 1 & participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store femcam
*earnings
regress earningsxhours i.indian_subs_3 pubsector parttime $socovars ///
i.wave if fem == 0 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store menearn
regress earningsxhours i.indian_subs_3 pubsector parttime $socovars ///
i.wave if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store femearn
* summary tables. 
esttab  menpar fempar,   keep(*indian_subs_3*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_p ll)  b(%8.2f) wide label mtitles
esttab  menemp fememp,   keep(*indian_subs_3*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_p ll)  b(%8.2f) wide label mtitles
esttab  mencam femcam,   keep(*indian_subs_3*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 ll)  b(%8.2f) wide label mtitles
esttab  menearn femearn, keep(*indian_subs_3*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 ll)  b(%8.2f) wide label mtitles
** BLACK COMMUNITY
* run respective models with First Generation Muslim Black as ref cat. 

logit participation i.black_subs i.wave $socovars ///
if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store menpar2 
logit participation i.black_subs i.wave $socovars ///
if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fempar2 
*employment 
logit employment i.black_subs $socovars i.wave ///
if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store menemp2
logit employment i.black_subs $socovars i.wave ///
if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store fememp2
*camsis
regress mcam10 i.black_subs $socovars   ///
i.wave if  participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store mencam2
regress mcam10 i.black_subs $socovars   ///
i.wave if fem == 1 & participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store femcam2
*earnings
regress earningsxhours i.black_subs pubsector parttime $socovars ///
i.wave if fem == 0 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store menearn2
regress earningsxhours i.black_subs pubsector parttime $socovars ///
i.wave if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store femearn2
* summary tables. 
esttab  menpar2 fempar2,   keep(*black_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_p ll)  b(%8.2f) wide label mtitles
esttab  menemp2 fememp2,   keep(*black_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_p ll)  b(%8.2f) wide label mtitles
esttab  mencam2 femcam2,   keep(*black_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 ll)  b(%8.2f) wide label mtitles
esttab  menearn2 femearn2, keep(*black_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 ll)  b(%8.2f) wide label mtitles

** Pakistani and Bangladeshi Community 

* run respective models with second-generation Pakistani and Bangladeshi as ref.

logit participation i.pb_subs i.wave $socovars ///
if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store menpar3
logit participation i.pb_subs i.wave $socovars ///
if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fempar3 
*employment 
logit employment i.pb_subs $socovars i.wave ///
if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store menemp3
logit employment i.pb_subs $socovars i.wave ///
if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store fememp3
*camsis
regress mcam10 i.pb_subs $socovars   ///
i.wave if  participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store mencam3
regress mcam10 i.pb_subs $socovars   ///
i.wave if fem == 1 & participation == 1 & cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store femcam3
*earnings
regress earningsxhours i.pb_subs pubsector parttime $socovars ///
i.wave if fem == 0 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store menearn3
regress earningsxhours i.pb_subs pubsector parttime $socovars ///
i.wave if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store femearn3
* summary tables. 
esttab  menpar3 fempar3,   keep(*pb_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_p ll)  b(%8.2f) wide label mtitles
esttab  menemp3 fememp3,   keep(*pb_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_p ll)  b(%8.2f) wide label mtitles
esttab  mencam3 femcam3,   keep(*pb_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 ll)  b(%8.2f) wide label mtitles
esttab  menearn3 femearn3, keep(*pb_subs*) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 ll)  b(%8.2f) wide label mtitles


* EOF. 
