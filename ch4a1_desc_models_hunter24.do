** ************************************************************************** **
	* CHAPTER 4 ANALYSIS 1 - descriptives and models.
	* Part of PhD research on assessing impact of contemporary migration 
	* patterns on the Muslim community in the UK. 
	* Last updated 2 Apr 2024
	* Scot Hunter 
** ************************************************************************** **


use $ptemp/file8.dta, clear 
keep if dvage >= 16 & dvage < 66
capture drop age2
gen age2 = dvage*dvage
recode health 1=1 2=0
lab var dvage_ind "age"
lab var health "health"


tab health 
gen j_health = health if wave == 10

// codebook ///
// pidp wave nwei firstresp employment participation mcam10 earningsxhours pubsector parttime     ///
// redcats health cardum dvage age2 mar child york nwest wmid rofeng rofuk   ///
// educscore engprof ties liberal religiosity pubsector parttime 				///
// *_educscore *_health *_religiosity *_miss, compact
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 

cibar participation if muslim == 1, over( ethn_dv)




** Descriptive stats 
** Simple multivariate analysis - 
** comparing main outcomes between Muslims and Non-Muslims. 

lab define fe 1 "Female" 0 "Male"
lab values fem fe
lab define mu 1 "Muslim" 0 "Non-Muslim"
label values muslim mu
lab define pa 1 "Participating" 0 "Not participating"
label values participation pa
label define em 1 "Employed" 0 "Unemployed"
label values employment em 
cibar participation [aw=nwei], over(fem muslim) level(95) ciopts(type(rcap) lcolor(black) lwidth(thin)) graphopts(ylab(0.00(0.10)1, labsize(small)) title({bf:Participating in the labour market}, size(vsmall)) ytitle(mean proportion) legend(row(1) pos(12) ring(0)) graphregion(margin(vsmall))) barcolor(purple%55 orange%55)
graph save $pgraphs/par.gph, replace
cibar employment [aw=nwei] if participation == 1, over(fem muslim) level(95) ciopts(type(rcap) lcolor(black) lwidth(thin)) graphopts(ylab(0.00(0.10)1, labsize(small)) title({bf:Employed}, size(vsmall)) ytitle(mean proportion) ) barcolor(purple%55 orange%55 )
graph save $pgraphs/emp.gph, replace
cibar mcam10 [aw=nwei] if participation == 1, over(fem muslim) level(95) ciopts(type(rcap) lcolor(black) lwidth(thin)) graphopts(ylab(5(4)55, labsize(small)) title({bf:Average CAMSIS10 score}, size(vsmall)) ytitle(mean) graphregion(margin(vsmall))) barcolor(purple%55 orange%55)
graph save $pgraphs/cam.gph, replace
cibar earningsxhours [aw=nwei] if participation == 1, over(fem muslim) level(95) ciopts(type(rcap) lcolor(black) lwidth(thin)) graphopts(ylab(5(0.50)20, labsize(small)) title({bf:Average houlry earnings}, size(vsmall)) ytitle(mean) graphregion(margin(vsmall))) barcolor(purple%55 orange%55)
graph save $pgraphs/ear.gph, replace
grc1leg2 $pgraphs/par.gph $pgraphs/emp.gph $pgraphs/cam.gph $pgraphs/ear.gph, xsize(8) ysize(13)

 * ---------------------------------------------------------------------------- *
 				* RELIGIOUS COMPARISONS *
 * ---------------------------------------------------------------------------- * 
clonevar relg = ff_religion
recode relg 2 = 0 
tab relg

tab jbnssec8_dv, m
recode  jbnssec8_dv . = -9
capture drop topro 
gen topro = jbnssec8_dv
recode topro 1 2 = 1 -9 = . * = 0 

mean topro [pw=nwei] /* overall mean average - across genders */
mean topro [pw=nwei], over(fem) cformat(%8.1g) /* average for men and women */ 

mean topro if participation == 1 [pw=nwei], over(relg fem) cformat(%8.1g)

capture drop reason
gen reason = jbstat
recode reason 1 2 3 5 = . 4 = 1 7 = 2 6 = 3 8 = 4 8 9 10 11 12 13 97 = 5
label define reason 1 "retired" 2 "student" 3 "looking after family or home" 4 "long term illness or disabled" 5 "other"
label values reason reason 
capture drop looking 
gen looking=(reason==3)

cibar looking  [aw=nwei], over(relg fem)

capture label drop relg 
label define relg 0 "Christ./no relg." 12 "Muslim" 13 "Hindu" 14 "Jewish" 15 "Sikh" 16 "Buddhist" 97 "Other"
label values relg relg 

global barts "barcolor(black%66 orange%85 black%55 black%45 black%35 black%25 black%15) baropts(lcolor(black) lwidth(vthin) fintensity(100))"
global cits "ciopts(type(rcap) lcolor(black) lwidth(med))"


mean participation [aw=nwei], over(relg fem) cformat(%8.1g)
mean looking [aw=nwei], over(relg fem) cformat(%8.1g)
mean topro [aw=nwei], over(relg fem) cformat(%8.1g)

cibar participation [aw=nwei], over(relg fem) level(95) $cits $barts graphopts(ylab( , labsize(vsmall)) title({bf:(a) Proportion who are participating in the labour market}, size(small)) ytitle(mean) graphregion(margin(vsmall)) legend(row(1))) 
graph save $pgraphs/eg1.gph, replace 

cibar looking  [aw=nwei], over(relg fem) level(95) $cits $barts graphopts(ylab( , labsize(vsmall)) title({bf:(b) Proportion who are inactive due to looking after family or home †}, size(small)) ytitle(mean) graphregion(margin(vsmall))) 
graph save $pgraphs/eg2.gph, replace 

cibar employment if participation == 1 [aw=nwei], over(relg fem ) level(95) $cits $barts graphopts(ylab(0.65(0.05)1, labsize(vsmall)) title({bf:(c) Proportion of those participating who are employed}, size(small)) ytitle(mean) graphregion(margin(vsmall))) 
graph save $pgraphs/eg3.gph, replace 

cibar topro if participation == 1 [aw=nwei], over(relg fem ) level(95) $cits $barts graphopts(ylab( , labsize(vsmall)) title({bf:(d) Proportion employed in 'top professions' ‡}, size(small)) ytitle(mean) graphregion(margin(vsmall))) 
graph save $pgraphs/eg4.gph, replace 

cibar mcam10 if participation == 1 [aw=nwei], over(relg fem ) level(95) $cits $barts graphopts(ylab( , labsize(vsmall)) title({bf:(e) Average CAMSIS10 score}, size(small)) ytitle(mean) graphregion(margin(vsmall))) 
graph save $pgraphs/eg5.gph, replace 

cibar earningsxhours if participation == 1 [aw=nwei], over(relg fem) level(95) $cits  $barts graphopts(ylab(5(5)28, labsize(vsmall)) title({bf:(f) Average hourly earnings}, size(small)) ytitle(mean)  graphregion(margin(vsmall))) 
graph save $pgraphs/eg6.gph, replace 

grc1leg2 $pgraphs/eg1.gph $pgraphs/eg2.gph $pgraphs/eg3.gph $pgraphs/eg4.gph $pgraphs/eg5.gph  $pgraphs/eg6.gph, col(2)
graph save $pgraphs/relg_comps.gph, replace 
graph export $pgraphs/relg_comps.gph, as(jpg)

 mean i.ff_religion [pwe=nwei], cformat(%8.1g)
 est store relg_mean

mean employment, over(ff_religion)
mean employment [pwe=nwei], over(ff_religion)
est store emp_relg 
coefplot emp_relg, recast(bar) bcolor(%76) citop ciopts(type(rcap) lcolor(black) lwidth(thick)) ///
xsize(8) ysize(12) graphregion(margin(vsmall)) bfintensity(100) vert 

cibar participation [aw=nwei], over(ff_religion fem ) level(95) ciopts(type(rcap) lcolor(black) lwidth(thin)) graphopts( title({bf:Employment Rate}, size(vsmall)) ytitle(mean) graphregion(margin(vsmall))) 
cibar employment [aw=nwei] if participation == 1, over(ff_religion fem ) level(95) ciopts(type(rcap) lcolor(black) lwidth(thin)) graphopts( title({bf:Employment Rate}, size(vsmall)) ytitle(mean) graphregion(margin(vsmall))) 
cibar  [aw=nwei] if participation == 1, over(ff_religion fem ) level(95) ciopts(type(rcap) lcolor(black) lwidth(thin)) graphopts( title({bf:Employment Rate}, size(vsmall)) ytitle(mean) graphregion(margin(vsmall))) 
 
 * ---------------------------------------------------------------------------- *
 				* DEMOGRAPHIC CHARACTERISTICS OF EACH GROUP *
 * ---------------------------------------------------------------------------- * 
table redcats [aw=nwei], stat(mean fem dvage mar child health cardum)  stat(count freshcats) nformat(%8.2g) 
mean fem  mar health cardum child [aw=nwei], over(redcats) cformat(%8.1g) 
estimates store desc1

foreach var in fem mar dvage_ind health child cardum { 
	quietly regress `var' i.redcats [pw=nwei]
	est store `var'_dmod
}
est tab *_dmod, star stat(N r2_p ll) b(%8.4g)


 * ---------------------------------------------------------------------------- *
 				         * GOVERNMENT OFFICE REGION *
 * ---------------------------------------------------------------------------- *
table redcats [aw=nwei], stat(m i.regions) stat(count freshcats) nformat(%8.2g) 
mean london york nwest rofeng rofuk [aw=nwei], over(redcats) cformat(%8.1g) 
estimates store desc3
coefplot desc3, recast(bar) bcolor(%76) citop ciopts(type(rcap) lcolor(black) lwidth(thick)) ///
xsize(8) ysize(12) graphregion(margin(vsmall)) bfintensity(100)
*for significance:
foreach var in london $regdums { 
	quietly regress `var' i.redcats [pw=nwei]
	est store `var'_mod
}
est tab *_mod, star stat(N r2_p ll) b(%8.4g)
 * ---------------------------------------------------------------------------- *
 				             * QUALIFICATIONS *
 * ---------------------------------------------------------------------------- *
gen nolab=hiqual_dv
table redcats [aw=nwei], stat(m i.nolab educscore i.engprof) stat(n hiqual_dv) nformat(%8.2f) /* in word doc*/ 
tabulate nolab, gen(eddums)
foreach var in eddums1 eddums2 eddums3 eddums4 eddums5 eddums6  educscore engprof {
	quietly regress `var' i.redcats [pw=nwei]
	est store `var'_cmod
}
est tab *_cmod, star stat(N r2_p ll) b(%8.4g)
 * ---------------------------------------------------------------------------- *
 				             * LABOUR MARKET OUTCOMES *
 * ---------------------------------------------------------------------------- *

 table redcats fem [aw=nwei], stat(m looking) nformat(%8.2f)
regress looking i.redcats  [pw=nwei]
 bysort fem: regress looking i.redcats  [pw=nwei]
 
 ** Participation (gender split). 
table redcats fem [aw=nwei], stat(m participation) nformat(%8.2f)
table redcats  fem [aw=nwei], stat(n participation)
bysort fem: regress participation i.redcats [pw=nwei]
 regress participation i.redcats [pw=nwei]
 table redcats [aw=nwei] if fem == 0 & participation == 1, stat(m employment parttime pubsector) stat(m mcam10 jbhrs earningsxhours) nformat(%8.2f)
 table redcats [aw=nwei] if fem == 1 & participation == 1, stat(m employment parttime pubsector) stat(m mcam10 jbhrs earningsxhours) nformat(%8.2f)
foreach var in employment parttime pubsector mcam10 jbhrs earningsxhours {
	quietly regress `var' i.redcats [pw=nwei] if participation == 1 & fem == 0
	est store `var'_dmmod
} 
est tab *_dmmod, star stat(N r2_p ll) b(%8.4g)
foreach var in employment parttime pubsector mcam10 jbhrs earningsxhours {
	quietly regress `var' i.redcats [pw=nwei] if participation == 1 & fem == 1
	est store `var'_dfmod
} 
est tab *_dfmod, star stat(N r2_p ll) b(%8.4g)
capture drop majorsoc
gen majorsoc = jbsoc10_cc 
recode majorsoc 100/199 =1 200/299 =2 300/399 =3 400/499 =4 500/599 = 5 ///
600/699 = 6  700/799 = 7 800/899 = 8 900/999 = 9 
tab majorsoc
tab  majorsoc  [aw=nwei] if muslim == 1 & participation ==1
bysort fem: tab  majorsoc  [aw=nwei] if muslim == 1 & participation ==1
bysort fem: regress majorsoc  [aw=nwei] if muslim == 1 & participation ==1


mean topro [pw=nwei], over(redcats fem) cformat(%8.1g)
regress topro i.redcats if participation == 1 [pw=nwei]

 table redcats fem if participation == 1 [aw=nwei], stat(m topro) nformat(%8.2f)
bysort fem: regress topro i.redcats if participation == 1 [pw=nwei]



 * ---------------------------------------------------------------------------- *
 				             * SOCIAL CULTURAL VARIABLES  *
 * ---------------------------------------------------------------------------- *
table redcats [aw=nwei], stat(m ties liberal religiosity) nformat(%8.2g)
table redcats [aw=nwei] if fem == 0, stat(m ties liberal religiosity) nformat(%8.2g)
table redcats [aw=nwei] if fem == 0, stat(n freshcats)
table redcats [aw=nwei] if fem == 1, stat(m ties liberal religiosity) nformat(%8.2g)
table redcats [aw=nwei] if fem == 1, stat(n freshcats)

foreach var in ties liberal religiosity {
	bysort fem: regress `var' i.redcats [pw=nwei]
}
est tab  *_scmod, star stat(N r2_p ll) b(%8.4g)



*** lets check for statistical differences between redcats and majority group using regression with same conditions as will be used in the regression. 

** particpation **
logit participation i.redcats if fem == 0 & par_miss == 0 [pw=nwei]
logit participation i.redcats if fem == 1 & par_miss == 0 [pw=nwei]
logit participation i.redcats if par_miss == 0 [pw=nwei]
* counts - 
tab redcats participation  if fem == 0 & par_miss == 0 [aw=nwei]
tab redcats participation  if fem == 1 & par_miss == 0 [aw=nwei]
tab redcats participation  if par_miss == 0 [aw=nwei]
*
logit employment i.redcats if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei]
logit employment i.redcats if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei]
* counts - 
tab redcats employment  if fem == 0 & emp_miss == 0 & participation == 1  [aw=nwei]
tab redcats employment if fem == 1 & emp_miss == 0 & participation == 1  [aw=nwei]
*CAMSIS10
regress mcam10 i.redcats if fem == 0 & cam_miss == 0 & participation == 1 [pw=nwei]
regress mcam10 i.redcats if fem == 1 & cam_miss == 0 & participation == 1 [pw=nwei]
*EARNINGS 
regress earningsxhours i.redcats if fem == 0 & earningmiss == 0 & participation == 1 [pw=nwei]
regress mcam10 i.redcats if fem == 1 & earningmiss == 0 & participation == 1 [pw=nwei]
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
** Regression analysis 


** (1) PARTICIPATION 
**
** Men *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
logit participation i.redcats i.wave ///
 if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store mpar_basic 

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
 if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store mpar_hcpa

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
 if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store mpar_soco

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
 if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store mpar_int

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity    ///
  mindian2_educscore mpakistani2_educscore mbangladeshi2_educscore ///
  mblack1_health mpakistani1_health  mpakistani1_religiosity ///
  mbangladeshi2_religiosity /// 
 if fem == 0 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store mpar_fin

** Women *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
logit participation i.redcats i.wave ///
 if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fpar_basic 

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fpar_hcpa

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fpar_soco

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fpar_int

logit participation i.redcats i.wave ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity   ///
mwhiteb0_educscore  mbangladeshi1_educscore  mbangladeshi1_health ///
 if fem == 1 & par_miss == 0  [pw=nwei], robust cluster(pidp)
est store fpar_fin
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 

est table mpar_basic mpar_hcpa mpar_soco mpar_fin ///
 fpar_basic fpar_hcpa fpar_soco  fpar_fin, star stat(N r2_p ll) b(%8.4g) var(30)
 
esttab mpar_basic mpar_hcpa mpar_soco mpar_fin using $ptemp/mpar01.csv ///
, b(%8.2f) se(%8.2f) drop(*wave*) stats(N r2_p ll) star wide replace
esttab fpar_basic fpar_hcpa fpar_soco fpar_fin using $ptemp/fpar01.csv, ///
 b(%8.2f) se(%8.2f)  drop(*wave*) stats(N r2_p ll) star wide replace
 
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
* Selection effect.  
est restore mpar_soco 
capture drop mpar_pr
predict mpar_pr
est restore fpar_soco 
capture drop fpar_pr
predict fpar_pr
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
** (2) EMPLOYMENT 
**
logit employment i.redcats i.wave ///
 if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store memp_basic_without


logit employment i.redcats i.wave  mpar_pr ///
 if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store memp_basic 

est tab  memp_basic_without memp_basic , star stat(r2)

logit employment i.redcats i.wave  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
 if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store memp_hcpa

logit employment i.redcats i.wave   mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
 if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store memp_soco

logit employment i.redcats i.wave  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
 if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store memp_int

logit employment i.redcats i.wave  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
	mwhiteb0_religiosity mblack1_religiosity  mpakistani2_health  ///
if fem == 0 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store memp_fin

** Women 
logit employment i.redcats i.wave ///
 if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store femp_basic_without 

logit employment i.redcats i.wave fpar_pr ///
 if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store femp_basic 

est tab  femp_basic_without femp_basic , star stat(r2_p)


logit employment i.redcats i.wave  fpar_pr ///
health cardum  dvage age2 mar child $regdums educscore engprof /// 
 if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store femp_hcpa

logit employment i.redcats i.wave fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
 if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store femp_soco

logit employment i.redcats i.wave fpar_pr ///
health cardum  dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
 if fem == 1 & emp_miss == 0 & participation == 1  [pw=nwei], robust cluster(pidp) 
est store femp_int

logit employment i.redcats i.wave  fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity  ///
	mblack1_educscore  mpakistani1_educscore mindian2_educscore ///
	mindian1_health   mpakistani2_religiosity  ///
 if fem == 1 & emp_miss == 0  & participation == 1 [pw=nwei], robust cluster(pidp) 
est store femp_fin

est table memp_basic memp_hcpa memp_soco  memp_fin ///
femp_basic femp_hcpa femp_soco  femp_fin , star stat(N r2_p ll) b(%8.4g) var(30)

esttab memp_basic memp_hcpa memp_soco memp_fin using $ptemp/memp01.csv ///
, b(%8.2f) se(%8.2f) drop(*wave*) stats(N r2_p ll) star wide replace
esttab femp_basic femp_hcpa femp_soco femp_fin using$ptemp/femp01.csv, ///
 b(%8.2f) se(%8.2f) drop(*wave*) stats(N r2_p ll) star wide replace
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
** (3) CAMSIS10
**
regress mcam10 i.redcats i.wave ///
if fem == 0 & participation == 1 &  cam_miss == 0 [pw=nwei] , robust cluster(pidp)
est store mcam_basic_without
regress mcam10 i.redcats i.wave mpar_pr ///
if fem == 0 & participation == 1 &  cam_miss == 0 [pw=nwei] , robust cluster(pidp)
est store mcam_basic
est tab mcam_basic_without mcam_basic, star stat(r2)
regress mcam10 i.redcats mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
i.wave if fem == 0 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store mcam_hcpa 
regress mcam10 i.redcats  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
i.wave if fem == 0 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store mcam_soco
regress mcam10 i.redcats  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
i.wave if fem == 0 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store mcam_int
regress mcam10 i.redcats  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
  mwhiteb0_educscore mpakistani1_educscore   mbangladeshi1_educscore ///
  mbangladeshi1_religiosity mindian2_religiosity  mpakistani2_religiosity ///
i.wave if fem == 0 & participation == 1 &  cam_miss == 0  [pw=nwei]  , robust cluster(pidp)
est store mcam_fin
* women
regress mcam10 i.redcats i.wave  ///
if fem == 1 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store fcam_basic_without
regress mcam10 i.redcats i.wave fpar_pr ///
if fem == 1 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store fcam_basic
est tab fcam_basic_without fcam_basic, star stat(r2)
regress mcam10 i.redcats fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
i.wave if fem == 1 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store fcam_hcpa 
regress mcam10 i.redcats  fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
i.wave if fem == 1 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store fcam_soco
regress mcam10 i.redcats  fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
i.wave if fem == 1 & participation == 1 &  cam_miss == 0  [pw=nwei] , robust cluster(pidp)
est store fcam_int
regress mcam10 i.redcats  fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
 mwhiteb0_educscore mblack1_educscore mpakistani1_educscore  ///
 mbangladeshi1_educscore mpakistani2_educscore mbangladeshi2_educscore ///
 mpakistani1_health mbangladeshi1_religiosity ///
i.wave if fem == 1 & participation == 1 &  cam_miss == 0  [pw=nwei]  , robust cluster(pidp)
est store fcam_fin

est table mcam_basic mcam_hcpa mcam_soco mcam_int mcam_fin ///
fcam_basic fcam_hcpa fcam_soco fcam_int fcam_fin ,		  ///
star stat(N  r2 ll) b(%8.2f) 

est table mcam_basic mcam_hcpa mcam_soco  mcam_fin ///
fcam_basic fcam_hcpa fcam_soco  fcam_fin ,		  ///
star stat(N  r2 ll) b(%8.4g) 

esttab mcam_basic mcam_hcpa mcam_soco mcam_fin using $ptemp/mcam01.csv ///
, b(%8.2f) se(%8.2f)  drop(*wave) stats(N r2 ll) star wide replace
esttab fcam_basic fcam_hcpa fcam_soco fcam_fin using$ptemp/fcam01.csv, ///
 b(%8.2f) se(%8.2f) drop(*wave) stats(N r2 ll) star wide replace
 
* earnings
 
 regress earningsxhours pubsector parttime i.redcats i.wave  ///
if fem == 0 & participation == 1 &  earningmiss == 0 [pw=nwei] , robust cluster(pidp)
est store mpay_basic_without
regress earningsxhours pubsector parttime i.redcats i.wave mpar_pr ///
if fem == 0 & participation == 1 &  earningmiss == 0 [pw=nwei] , robust cluster(pidp)
est store mpay_basic
est tab mpay_basic_without mpay_basic, star 
regress earningsxhours pubsector parttime i.redcats mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
i.wave if fem == 0 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store mpay_hcpa 
regress earningsxhours pubsector parttime i.redcats  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
i.wave if fem == 0 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store mpay_soco
regress earningsxhours pubsector parttime i.redcats  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
i.wave if fem == 0 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store mpay_int
regress earningsxhours pubsector parttime i.redcats  mpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
  mpakistani1_educscore  mbangladeshi1_educscore mindian2_educscore ///
	mpakistani2_educscore mbangladeshi2_educscore ///
i.wave if fem == 0 & participation == 1 &  earningmiss == 0  [pw=nwei]  , robust cluster(pidp)
est store mpay_fin
* women
regress earningsxhours pubsector parttime i.redcats i.wave  ///
if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store fpay_basic_without
regress earningsxhours pubsector parttime i.redcats i.wave fpar_pr ///
if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store fpay_basic
est tab fpay_basic_without fpay_basic, star
regress earningsxhours pubsector parttime i.redcats fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
i.wave if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store fpay_hcpa 
regress earningsxhours pubsector parttime i.redcats  fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
i.wave if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store fpay_soco
regress earningsxhours pubsector parttime i.redcats  fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity *_educscore *_health *_religiosity  ///
i.wave if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei] , robust cluster(pidp)
est store fpay_int
regress earningsxhours pubsector parttime i.redcats  fpar_pr ///
health cardum dvage age2 mar child $regdums educscore engprof /// 
ties liberal religiosity ///
 mwhiteb0_educscore mblack1_educscore  mpakistani1_educscore ///
 mbangladeshi2_educscore mbangladeshi2_health  mindian2_religiosity ///
 mpakistani2_religiosity ///
i.wave if fem == 1 & participation == 1 &  earningmiss == 0  [pw=nwei]  , robust cluster(pidp)
est store fpay_fin

est table mpay_basic mpay_hcpa mpay_soco mpay_int mpay_fin ///
 fpay_basic fpay_hcpa fpay_soco fpay_int fpay_fin , star stat(N  r2 ll) b(%8.4g) var(30)

est table mpay_basic mpay_hcpa mpay_soco  mpay_fin fpay_basic ///
 fpay_hcpa fpay_soco  fpay_fin , star stat(N  r2 ll) b(%8.4g) var(30)

 esttab mpay_basic mpay_hcpa mpay_soco mpay_fin  using $ptemp/mpay_1.csv, ///
 b(%8.2f) se(%8.2f) drop(*wave) stats(N r2 ll) star wide replace
esttab fpay_basic fpay_hcpa fpay_soco fpay_fin  using $ptemp/fpay_1.csv, ///
 b(%8.2f) se(%8.2f) drop(*wave) stats(N r2 ll) star wide replace 
 
 ***
 
esttab mpay_basic fpay_basic mpay_hcpa fpay_hcpa mpay_soco fpay_soco mpay_fin fpay_fin,  ///
 b(%8.2f) se(%8.2f) drop(*wave) stats(N r2 ll) star  replace


 

 ***** final tables for online appendix ****************************************
esttab mpar_basic mpar_hcpa mpar_soco  mpar_fin fpar_basic ///
fpar_hcpa fpar_soco  fpar_fin using $ptemp/huge1.csv,  ///
b(%8.3f) se(%8.2f) drop(*wave) star(* 0.05) stats(N r2_p ll)  ///
nogaps label mtitles wide onecell compress replace
 ***** final tables for word (without se) ****************************************
esttab mpar_basic mpar_hcpa mpar_soco  mpar_fin fpar_basic ///
fpar_hcpa fpar_soco  fpar_fin using $ptemp/huge1_nose.csv,  ///
b(%8.3f) not   drop(*wave) star(* 0.05) stats(N r2_p ll) nonotes  ///
nogaps label mtitles wide onecell compress replace

 
esttab memp_basic memp_hcpa memp_soco  memp_fin femp_basic ///
femp_hcpa femp_soco  femp_fin using $ptemp/huge2.csv,  ///
b(%8.3f) se(%8.2f) drop(*wave) star(* 0.05) stats(N r2_p ll)  ///
nogaps label mtitles wide onecell compress replace
 ***** final tables for word (without se) ****************************************
esttab memp_basic memp_hcpa memp_soco  memp_fin femp_basic ///
femp_hcpa femp_soco  femp_fin using $ptemp/huge2_nose.csv,  ///
b(%8.3f) not drop(*wave) star(* 0.05) stats(N r2_p ll)  ///
nogaps label mtitles wide onecell compress replace


 
esttab mcam_basic mcam_hcpa mcam_soco  mcam_fin fcam_basic ///
fcam_hcpa fcam_soco  fcam_fin using $ptemp/huge3.csv,  ///
b(%8.3f) se(%8.3f) drop(*wave) star(* 0.05) stats(N r2 ll)  ///
nogaps label mtitles wide onecell compress replace
 ***** final tables for word (without se) ****************************************
esttab mcam_basic mcam_hcpa mcam_soco  mcam_fin fcam_basic ///
fcam_hcpa fcam_soco  fcam_fin using $ptemp/huge3_nose.csv,  ///
b(%8.3f) not drop(*wave) star(* 0.05) stats(N r2 ll)  ///
nogaps label mtitles wide onecell compress replace

esttab mpay_basic mpay_hcpa mpay_soco  mpay_fin fpay_basic ///
fpay_hcpa fpay_soco  fpay_fin using $ptemp/huge4.csv,  ///
b(%8.2f) se(%8.3f) drop(*wave) star(* 0.05) stats(N r2 ll)  ///
nogaps label mtitles wide  compress replace
 ***** final tables for word (without se) ****************************************
esttab mpay_basic mpay_hcpa mpay_soco  mpay_fin fpay_basic ///
fpay_hcpa fpay_soco  fpay_fin using $ptemp/huge4_nose.csv,  ///
b(%8.3f) not drop(*wave) star(* 0.05) stats(N r2 ll)  ///
nogaps label mtitles wide  compress replace
 
 
 
 
 
 
 ***** final graphs for word ***************************************************
 
coefplot ///
(mpar_soco, mcolor(orange) ciopts(recast(rcap) color(orange)) )    ///
(fpar_soco, mcolor(purple) ciopts(recast(rcap) color(purple)) ), bylabel(Participation) || ///
(memp_soco, mcolor(orange) ciopts(recast(rcap) color(orange)) )    ///
(femp_soco, mcolor(purple) ciopts(recast(rcap) color(purple)) ), bylabel(Employment)  || ///
(mcam_soco, mcolor(orange) ciopts(recast(rcap) color(orange))  )    ///
(fcam_soco, mcolor(purple) ciopts(recast(rcap) color(purple)) ), bylabel(CAMSIS) || ///
(mpay_soco, mcolor(orange) ciopts(recast(rcap) color(orange))  )    ///
(fpay_soco, mcolor(purple) ciopts(recast(rcap) color(purple))  ),  bylabel(Log. Avg. Pay) || , ///
keep(*.redcats) legend(pos(6) row(1) ///
label(1 "Men") label(2 "Men") label(3 "Women") label(4  "Women")) ///
coeflabels(, labsize(small)) xline(0) ///
headings(0.redcats = "{bf:Refrence Cateogry}" ///
1.redcats = "{bf:British Ancestry}" ///
9.redcats = "{bf:First Generation}" ///
29.redcats = "{bf:Second Generation}") byopts(xrescale row(1)) 
graph save $pgraphs/dotsplot_up.gph , replace


coefplot ///
(mpar_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpar_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpar_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpar_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Participation) || ///
///
(memp_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(memp_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(femp_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(femp_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Employment) || ///
///
(mcam_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mcam_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fcam_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fcam_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(CAMSIS) || ///
///
(mpay_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpay_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpay_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpay_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Log. Avg. Pay) || , ///
recast(bar) barwidth(0.45) keep(*.redcats) legend(pos(6) row(1) ///
label(1 "Men (not sig.)") label(2 "Men (sig.)") label(3 "Women (not sig.)") label(4  "Women (sig.)")) ///
coeflabels(, labsize(small)) xline(0) ///
headings(0.redcats = "{bf:Refrence Cateogry}" ///
1.redcats = "{bf:British Ancestry}" ///
9.redcats = "{bf:First Generation}" ///
29.redcats = "{bf:Second Generation}") noci byopts(xrescale row(1))
graph save $pgraphs/barsplot_up.gph , replace

 
 

 
 
 * participation
 coefplot ///
(mpar_basic, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpar_basic, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpar_basic, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpar_basic, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Basic) || ///
(mpar_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpar_hcpa, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpar_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpar_hcpa, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Human Capital) || ///
(mpar_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpar_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpar_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpar_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Socio-cultural) || ///
(mpar_fin, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpar_fin, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpar_fin, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpar_fin, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Final Model) || , ///
 recast(bar) barwidth(0.45) keep(*.redcats) legend(pos(6) row(1) ///
label(1 "Men (not sig.)") label(2 "Men (sig.)") label(3 "Women (not sig.)") label(4  "Women (sig.)")) ///
coeflabels(, labsize(small)) xline(0) ///
headings(0.redcats = "{bf:Refrence Cateogry}" ///
1.redcats = "{bf:British Ancestry}" ///
9.redcats = "{bf:First Generation}" ///
29.redcats = "{bf:Second Generation}") noci byopts(xrescale row(1))
 graph save $pgraphs/par_up.gph , replace

 
 *employment
coefplot ///
(memp_basic, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(memp_basic, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(femp_basic, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(femp_basic, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Basic) || ///
(memp_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(memp_hcpa, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(femp_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(femp_hcpa, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Human Capital) || ///
(memp_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(memp_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(femp_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(femp_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Socio-cultural) || ///
(memp_fin, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(memp_fin, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(femp_fin, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(femp_fin, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Final Model) || , ///
 recast(bar) barwidth(0.45) keep(*.redcats) legend(pos(6) row(1) ///
label(1 "Men (not sig.)") label(2 "Men (sig.)") label(3 "Women (not sig.)") label(4  "Women (sig.)")) ///
coeflabels(, labsize(small)) xline(0) ///
headings(0.redcats = "{bf:Refrence Cateogry}" ///
1.redcats = "{bf:British Ancestry}" ///
9.redcats = "{bf:First Generation}" ///
29.redcats = "{bf:Second Generation}") noci byopts(xrescale row(1))
graph save $pgraphs/emp_up.gph , replace

 *camsis
coefplot ///
(mcam_basic, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mcam_basic, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fcam_basic, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fcam_basic, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Basic) || ///
(mcam_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mcam_hcpa, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fcam_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fcam_hcpa, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Human Capital) || ///
(mcam_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mcam_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fcam_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fcam_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Socio-cultural) || ///
(mcam_fin, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mcam_fin, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fcam_fin, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fcam_fin, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Final Model) || , ///
 recast(bar) barwidth(0.45) keep(*.redcats) legend(pos(6) row(1) ///
label(1 "Men (not sig.)") label(2 "Men (sig.)") label(3 "Women (not sig.)") label(4  "Women (sig.)")) ///
coeflabels(, labsize(small)) xline(0) ///
headings(0.redcats = "{bf:Refrence Cateogry}" ///
1.redcats = "{bf:British Ancestry}" ///
9.redcats = "{bf:First Generation}" ///
29.redcats = "{bf:Second Generation}") noci byopts(xrescale row(1))
 graph save $pgraphs/cam10_up.gph , replace

 
* pay
coefplot ///
(mpay_basic, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpay_basic, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpay_basic, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpay_basic, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Basic) || ///
(mpay_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpay_hcpa, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpay_hcpa, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpay_hcpa, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Human Capital) || ///
(mpay_soco, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpay_soco, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpay_soco, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpay_soco, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Socio-cultural) || ///
(mpay_fin, if(@pval>.05) bcolor(gray%0) blcolor(orange%75)) /// not sig
(mpay_fin, if(@pval<.05) bcolor(orange%75) blcolor(black)) ///
(fpay_fin, if(@pval>.05) bcolor(gray%0) blcolor(purple%75)) /// not sig
(fpay_fin, if(@pval<.05) bcolor(purple%75) blcolor(black)),  bylabel(Final Model) || , ///
 recast(bar) barwidth(0.45) keep(*.redcats) legend(pos(6) row(1) ///
label(1 "Men (not sig.)") label(2 "Men (sig.)") label(3 "Women (not sig.)") label(4  "Women (sig.)")) ///
coeflabels(, labsize(small)) xline(0) ///
headings(0.redcats = "{bf:Refrence Cateogry}" ///
1.redcats = "{bf:British Ancestry}" ///
9.redcats = "{bf:First Generation}" ///
29.redcats = "{bf:Second Generation}") noci byopts(xrescale row(1))
 graph save $pgraphs/pay_up.gph , replace

 
 
 
********************************************************************************

							    * FINISH *

********************************************************************************




 
save $pnew/file_models_update.dta, replace
 
