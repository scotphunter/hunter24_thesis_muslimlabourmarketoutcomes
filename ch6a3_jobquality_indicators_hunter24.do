********************************************************************************
*** Job quality variables. 
***************************. 

use pidp psu strata ethn_dv generation using $pprotect/xwavedat_protect.dta, clear
sort pidp 
sav $ptemp/allwave.dta, replace 

* in every wave. 
foreach wav in a b c d e f g h i j k l {
use pidp `wav'*sex_dv `wav'_jbsoc00 `wav'*oprlg0ni  `wav'*mastat_dv `wav'*urban_dv `wav'_dvage `wav'_jbstat `wav'*intdaty_dv `wav'*fimngrs_dv `wav'_hiqual_dv `wav'*jbhrs `wav'*paytyp `wav'*jbot `wav'*jbotpd `wav'*jbsat `wav'*jbterm1 `wav'*gor_dv `wav'*nchild_dv `wav'*jbnssec5_dv using $pprotect/`wav'_indresp_protect.dta, clear
	sort pidp 
	renpfix `wav'_
	gen wave = "`wav'"
	sav $ptemp/`wav'_jbqual.dta, replace 
}

use $ptemp/a_jbqual.dta, clear
foreach wav in b c d e f g h i j k l {
	append using  $ptemp/`wav'_jbqual.dta
}
sort pidp wave 
sav $ptemp/all_jbqual.dta, replace 
* in every second wave. 
foreach wav in b d f h j l { 
use pidp `wav'*jbonus `wav'*oprlg `wav'*oprlg1  `wav'*jbrise `wav'*jbpen `wav'*wkends `wav'*wktime `wav'*jbflex* `wav'*jbfx* `wav'*wkaut* `wav'*depenth* `wav'*jwbs* `wav'*jbsec `wav'*jbxpch* ///
using $pprotect/`wav'_indresp_protect.dta, clear
	sort pidp 
	renpfix `wav'_
	gen wave = "`wav'"
	sav  $ptemp/`wav'_jbqual_alt.dta, replace 
}
use $ptemp/b_jbqual.dta, clear
foreach wav in   d  f  h  j  l {
	append using  $ptemp/`wav'_jbqual_alt.dta
}
sort pidp wave 
sav $ptemp/all_jbqual_alt.dta, replace
foreach wav in b  d e f g h i j k l {
	use pidp `wav'_ff_jbsize using $pprotect/`wav'_indresp_protect.dta, clear
	gen wave = "`wav'"
	renpfix `wav'_
	sav  $ptemp/`wav'_jbsize.dta, replace 
}

use $ptemp/b_jbsize.dta, clear 
foreach wav in d e f g h i j k l {
	append using $ptemp/`wav'_jbsize.dta 
}
sort pidp wave
sav $ptemp/all_jbsize.dta, replace


use $ptemp/allwave.dta, clear
merge 1:m pidp using $ptemp/all_jbqual.dta 
keep if _merge == 3 
drop _merge 
merge m:m pidp using $ptemp/all_jbqual_alt.dta
keep if _merge == 1 | _merge == 3 
drop _merge
merge m:m pidp using  $ptemp/all_jbsize.dta
keep if _merge == 1 | _merge == 3 
drop _merge
drop nxtjbhrs jbflex9 jbflex10 *use*  jbxpchc jbxpchd jbxpche   jbflex96 

sort pidp wave
list pidp wave in 1/100

* organise variables by different aspects of job quality 
global monetary "jbonus jbrise jbpen" 
global wlbalance "wkends wktime jbflex1 jbflex2 jbflex3 jbflex4 jbflex5 jbflex6 jbflex7 jbflex8 jbfxinf"
global intrins "wkaut1 wkaut2 wkaut3 wkaut4 wkaut5 depenth1 depenth2 depenth3 depenth4 depenth5 depenth6 "
global empqual "jbsec jbxpcha  jbxpchb"
foreach var in $monetary $wlbalance $intrins $empqual { 
	recode `var' -100/-1 = .
}
capture drop empstat 
gen empstat = (jbstat==2  | jbstat==9 | jbstat==10 | jbstat==11)  
label var empstat "In work"
* filling in missing data with respeonses from nearest wave. 
keep if empstat 
	* limited to employees only (unemployed and self employed excluded).
	* carry responses to employment conditions forward/backword to other waves 
	* for respondents who remained in the same job with the same employer. 
	
sav $ptemp/before_fill.dta, replace 

use $ptemp/before_fill.dta, clear
capture drop waveno
encode wave, gen(waveno)
lab var waveno "wave numbered"
lab var wave "wave unumbered"
sort pidp waveno

by pidp: gen same_job = (jbsoc00 == jbsoc00[_n-1]) & (_n > 1)
list pidp waveno same_job jbsoc00 $monetary in 50/100 

recode ff_jbsize -100/-1 = . 
foreach myvar in ff_jbsize $monetary {
	by pidp (waveno), sort: replace `myvar' = `myvar'[_n+1] if `myvar' == . 
}

list pidp waveno same_job jbsoc00 $monetary $wlbalance $intrins $empqual in 1/50
	* this code took from the next available observation - 
 foreach num of numlist 1/5 {
 	di "`num'"
	foreach myvar in ff_jbsize $monetary $wlbalance $intrins $empqual {
		by pidp (waveno), sort: replace `myvar' = `myvar'[_n-1] if `myvar' == . &  same_job == 1 
		by pidp (waveno), sort: replace `myvar' = `myvar'[_n+1] if `myvar' == . & same_job == 1 
	}
}
foreach myvar in $monetary $wlbalance $intrins $empqual {
    by pidp (waveno), sort: replace `myvar' = `myvar'[_n+1] if `myvar' == . & _n == 1
}
drop same_job 

** monetary *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
su fimngrs_dv jbhrs paytyp $monetary 
foreach var in fimngrs_dv jbhrs paytyp {
	recode `var' -10000000000/-1 = . 
}
* indicator 1 - hourly wage quartiles. 
capture drop wage
gen wage = fimngrs_dv / 4 /* weekly wage */
capture drop hwage 
gen hwage = wage / jbhrs /* hourly wage */ 
lab var hwage "hourly wage"
capture drop loghwage
gen loghwage = log(hwage) /* log version */
lab var loghwage "log hourly wage"
capture drop hwage_qt
xtile hwage_qt = hwage, nq(4) /*wage quartiles*/
table hwage_qt, stat(m hwage)
lab var hwage_qt "hourly wage quartiles"
drop wage 
gen highwage = hwage_qt == 4 
gen lowwage = hwage_qt == 1
* indicator 2 - paid per hour or salaried 
tab paytyp
capture drop paidhr 
gen paidhr = (paytyp == 3) if paytyp !=.
lab var paidhr "paid by the hour"
* indicator 3 - includes bonuses or profit share 
recode jbonus 1=1 2=0 
* indicator 4 - incramental raises
recode jbrise 2=0 
* indicator 5 - employer includes a pension scheme. 
recode jbpen 2=0

** worklife balance *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
su jbot jbotpd $wlbalance
* idicator 1 - standard hours 
capture drop standhrs 
gen standhrs = wktime 
recode standhrs 1/3 = 1 4/97 = 0 
label define stn 1 "stnd hrs (morn./aftr./daytime)" 0 "unstandard - eve etc."
label values standhrs stn 
lab var standhrs "standard working hours"
* indicator 2 - weekend working
capture drop wkndhrs 
gen wkndhrs = (wkends== 1 | wkends== 2) if wkends !=.
lab var wkndhrs "usually works on the weekend" 
* indicator 3 - flexible arrangments (offers more than 2)
capture drop flexco 
gen flexco = jbflex1 + jbflex2 + jbflex3 + jbflex4 + jbflex5 + jbflex6 + jbflex7 + jbflex8
gen flex2pls = (flexco >= 2 )
drop flexco
lab var flex2pls "employers offer 2 or more flexible arrangments"
* indicator 4 - informal flex - for example by re-arranging your start or finish times if you need to?. 
capture drop flexinf 
gen flexinf = (jbfxinf == 1)
lab var flexinf "informal flexible working arrangements" 

** intrinsic quality *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
su jbsat $intrins 
* idicator 1 - low autonomy & high autonomy
alpha wkaut* /* 0.85 */
capture drop all_auto
gen all_auto = wkaut1 + wkaut2 + wkaut3 + wkaut4 + wkaut5
xtile auto_qt = all_auto, nq(4)
gen lowauto = (auto_qt == 4)
lab var lowauto "low on autonomy in workscale"
gen highauto = (auto_qt == 1)
lab var highauto "high on autonomy in workscale"
drop all_auto

* indicator 3 - disatisfied with job 
recode jbsat -100/-1 = . 
gen disat = jbsat 
recode disat 1/3 = 1 4/8 = 0 * = .  
lab var disat "disatisfied with job"

*indicator 4 - high worry scale and the quartiles 
alpha depenth* /* 0.91 */ 
capture drop wryscle 
gen wryscle = depenth1 + depenth2 +  depenth3 +  depenth4 +  depenth5 +  depenth6 
xtile wryscle_qt = wryscle, nq(4)
gen highworry = (wryscle_qt==4) if wryscle != .
lab var highworry "high on worry about job scale"
gen lowworry = (wryscle_qt==1) if wryscle != .
lab var lowworry "low on worry about job scale"
drop wryscle 

** employment quality *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
su jbterm1 $empqual
recode jbterm1 -100/-1 = .
* indcator 1 - insecure work 
capture drop insecure 
gen insecure = (jbsec == 3 | jbsec == 4) if jbsec !=. 
lab var insecure "(very)unlikely to have job security in next 12 m."
* indicator 2 - temporary  work 
capture drop temp 
gen temp =(jbterm1 == 2) if jbterm1 !=. 
lab var temp "temporary contract"
*indicator 3 - expect training/promotion 
recode jbxpcha 3 = . 
capture drop expects 
tab jbxpcha jbxpchb
gen expects = (jbxpcha == 1) & (jbxpchb == 1)
lab var expects "expects training/promotion in job"
capture drop eth2
clonevar eth2=ethn_dv
recode eth2 (2=1) (3/8=9) (12 13 16 17=97) (-9/-1=.)
label define ethn_dv 1 "white", modify
label var eth2 "ethnic group (reduced)"
capture drop fem 
gen fem = (sex_dv == 2) if sex_dv != . 
lab var fem "gender = female"
capture drop educ 
recode hiqual_dv 1 2 = 1 3 = 2 4 = 3 5 = 4 9 = 5
gen educ = 6 - hiqual_dv  
lab var educ "hiqest qual. held (1 = no qual. 5 = degree)"
lab define educ 1 "no qual." 2 "other qual." 3 "gcse etc." 4 "a-level etc." 5 "degree"
gen cohab=(mastat_dv==2 | mastat_dv==3 | mastat_dv==10)  if mastat_dv !=.
label variable cohab "married or cohabiting"
capture drop urbarea
gen urbarea = (urban_dv == 1)
capture drop southeast
gen southeast = (gor_dv == 8  | gor_dv == 7)
capture drop child 
gen child = nchild_dv > 0
foreach var in oprlg1 oprlg0ni {
	replace `var' = 0 if oprlg==2
	capture drop max1_`var 
	egen max1_`var'=rowmax(`var')
	tab max1_`var' 
}
gen religion=max1_oprlg1
recode religion 2 3 4 5 6 7 8 9 10 11 17 = 2 
replace religion = 2  if max1_oprlg0ni == 2   /* Christian */ 
replace religion = 12 if max1_oprlg0ni == 5   /* Muslim   */
replace religion = 16 if max1_oprlg0ni == 3   /* Buddhist */ 
replace religion = 13 if max1_oprlg0ni == 4   /* Hindu    */ 
replace religion = 97 if max1_oprlg0ni == 6   /* Other.   */ 
label define  a_oprlg1 0 "no relg." 2 "christian", modify
label values religion a_oprlg1


label define rc 0 "ch. w brit." 1 "mu w. brit" 2 "indian 1" 3 "pakistani 1" 4 "bangladeshi 1" 5 "mu black 1" 6 "mu other 1"  7 "indian 2" 8 "pakistani 2" 9 "bangladeshi 2"  10 "mu other 2" 


capture drop miss
egen miss = rmiss(fem religion educ eth2 dvage urbarea cohab southeast child lowwage highwage paidhr jbonus jbrise jbpen  standhrs wkndhrs flexinf flex2pls insecure temp expects lowauto highauto disat highworry)
codebook if miss == 0, compact
keep if dvage > 15 & dvage < 65 
keep if miss == 0


sav $ptemp/after_fill.dta, replace 

//  sem (lowwage highwage paidhr jbonus jbrise jbpen   standhrs wkndhrs flexinf flex2pls insecure temp expects lowauto highauto disat highworry  <-), logit lclass(C 7), logit
//
//
// gsem (lowwage highwage paidhr jbonus jbrise jbpen   standhrs wkndhrs flexinf flex2pls insecure temp expects lowauto highauto disat highworry <- )
//
//
// ** Treating job quality as a multivariate outcome in a
// *  multilevel framework:  this can be analysed by reformatting the data so that there 
// *    is one row for each jbsat item per person, done in the following lines: 
//
// regress fem religion educ eth2 dvage urbarea cohab southeast child lowwage highwage paidhr jbonus jbrise jbpen  standhrs wkndhrs flexinf flex2pls insecure temp expects lowauto highauto disat highworry
/
//

use $ptemp/after_fill.dta, clear

quietly gsem (highwage paidhr jbonus jbrise jbpen  standhrs wkndhrs flexinf flex2pls insecure temp expects highauto disat highworry  <-), logit lclass(C 5)
est store gsem1 


estat lcgof 
est restore gsem1
estat lcmean, nose
// estat lcprob

predict cpost*, classposteriorpr
egen max=rowmax(cpost*)
capture drop predclass 
gen predclass = .
replace predclass = 1 if cpost1==max
replace predclass = 2 if cpost2==max
replace predclass = 3 if cpost3==max
replace predclass = 4 if cpost4==max
replace predclass = 5 if cpost5==max

clonevar nssec = jbnssec5_dv 
recode nssec -100/-1 = .



mlogit predclass i.fem i.eth2 dvage


capture drop jobtypes 
gen jobtypes = predclass 
recode jobtypes 1 = 5 2 = 3 3 = 4 4 = 1 5 = 2 
lab define types 1 "high-quality, flexible jobs" 2 "high intrinsic/low rewards" 3 "polarised (mixed)" 4 "unhappy 9-5" 5 "bad jobs"
lab values jobtypes types 

tab nssec jobtypes, col 

catplot jobtypes nssec , percent(nssec) asyvars stack vertical legend(pos(6) row(1))

sav $ptemp/withmodel.dta, replace


foreach wav in a b c d e f g h i j k l {
use pidp `wav'*jbnssec8_dv using $pprotect/`wav'_indresp_protect.dta, clear
	sort pidp 
	renpfix `wav'_
	gen wave = "`wav'"
	sav $ptemp/`wav'_jbnssec8.dta, replace 
}
use $ptemp/a_jbnssec8.dta, clear
foreach wav in b c d e f g h i j k l {
	append using  $ptemp/`wav'_jbnssec8.dta
}
sav $ptemp/all_jbnssec8.dta, replace 

merge m:m pidp wave using $ptemp/withmodel.dta
keep if _merge == 3 
drop _merge
sav $ptemp/withmodel2.dta, replace 

	*CAMSIS10 files. 
use "/Users/scothunter/Library/CloudStorage/OneDrive-UniversityofStirling/thesis/directory/data/camsis/a_camsis00.dta", clear
sum if soc2000 >= 1000 & soc2000 <= 9999
keep if soc2000 >= 1000 & soc2000 <= 9999
keep if ukempst==0 
rename soc2000 jbsoc00 
rename mcamsis mcamsis00
keep jbsoc00 mcamsis00
sort jbsoc00
sav "$ptemp/mc0.dta", replace
use $ptemp/withmodel2.dta, clear 
sort jbsoc00
merge m:1 jbsoc00 using "$ptemp/mc0.dta"
keep if _merge == 1 | _merge == 3 
drop _merge
sort pidp wave
sav $ptemp/withmodel3.dta, replace 

use pidp redcats *ons* *lfs* nwei using $ptemp/file8.dta, clear 
sort pidp 
rename redcats ethcats
egen onet = tag(pidp)
keep if onet
sav $ptemp/redcats.dta, replace 
use $ptemp/withmodel3.dta, clear
merge m:1 pidp using $ptemp/redcats.dta
keep if _merge == 1 | _merge == 3
drop _merge 

tab ethcats jobtypes
recode mcamsis00 -100/-1 = .
table jobtypes, stat(m mcamsis00)
recode jbnssec8_dv -100/-1 = .
catplot jobtypes jbnssec8_dv , percent(jbnssec8_dv) ///
asyvars stack  legend(pos(6) row(2)) bar(5, bfcolor(orange*0.8)) bar(4, bfcolor(orange*0.3) blcolor(orange*0.8)) bar(3, bfcolor(purple*0.2) blcolor(purple*1.2)) bar(2, bfcolor(purple*0.7) blcolor(purple*1.2)) bar(1, bcolor(purple)) ysc(r(-5 100)) blabel(bar, format(%9.1f) size(tiny) pos(base))

catplot jobtypes ethcats if fem == 0 , percent(ethcats) ///
asyvars stack  legend(pos(6) row(2)) bar(5, bfcolor(orange*0.8)) bar(4, bfcolor(orange*0.3) blcolor(orange*0.8)) bar(3, bfcolor(purple*0.2) blcolor(purple*1.2)) bar(2, bfcolor(purple*0.7) blcolor(purple*1.2)) bar(1, bcolor(purple)) ysc(r(-5 100)) blabel(bar, format(%9.1f) size(tiny) pos(base))

catplot jobtypes ethcats if fem == 1 , percent(ethcats) ///
asyvars stack  legend(pos(6) row(2)) bar(5, bfcolor(orange*0.8)) bar(4, bfcolor(orange*0.3) blcolor(orange*0.8)) bar(3, bfcolor(purple*0.2) blcolor(purple*1.2)) bar(2, bfcolor(purple*0.7) blcolor(purple*1.2)) bar(1, bcolor(purple)) ysc(r(-5 100)) blabel(bar, format(%9.1f) size(tiny) pos(base))

su lfs_soc4_p04 
gen logsoc4 = log(lfs_soc4_p04)
su ons11_lsoa_p04
gen loglsoa = log(ons11_lsoa_p04)


mlogit jobtypes i.ethcats [pw=nwei], baseoutcome(1) robust cluster(pidp)
est store ml_base 
mlogit jobtypes i.ethcats educ dvage urbarea cohab southeast child [pw=nwei], baseoutcome(1) robust cluster(pidp)
est store ml_sd 
mlogit jobtypes i.ethcats educ dvage urbarea cohab southeast child loglsoa [pw=nwei], baseoutcome(1) robust cluster(pidp)
est store m_local
mlogit jobtypes i.ethcats educ dvage urbarea cohab southeast child ons11_lsoa_p04 lfs_soc4_p04 [pw=nwei], baseoutcome(1)robust cluster(pidp)
est store m_occ 

est table m*, star stat(N)

clonevar pla = jobtypes 
gen njbt = 6 - pla

foreach num of numlist 1/5 { 
	capture drop job`num' 
	gen job`num'=(njbt==`num')
}

*men
foreach num of numlist 1/5 {
logit job`num' i.ethcats if fem == 0 [pw=nwei],  robust cluster(pidp)
	est store mb_`num'
logit job`num' i.ethcats educ dvage urbarea cohab southeast child if fem == 0 [pw=nwei], robust cluster(pidp)
est store msd_`num'
logit job`num' i.ethcats educ dvage urbarea cohab southeast child loglsoa if fem == 0 [pw=nwei], robust cluster(pidp)
est store mloc_`num'
logit job`num' i.ethcats educ dvage urbarea cohab southeast child loglsoa logsoc4 if fem == 0 [pw=nwei], robust cluster(pidp)
est store mocc_`num'
est tab  mb_`num' msd_`num' mloc_`num' mocc_`num', star stat(N r2_p) b(%8.4g) var(30)
}


coefplot (mb_1, msymbol(D)  mcolor(black) ciopts(recast(rcap) color(black))) (msd_1, msymbol(T) mcolor(black*0.4) ciopts(recast(rcap) color(black*0.4))) (mloc_1, msymbol(S) mcolor(gray) ciopts(recast(rcap) color(gray))) (mocc_1, msymbol(O) mcolor(gray*0.40) ciopts(recast(rcap) color(gray*0.40))) , bylabel(Bad job) || m*_2, bylabel(Unhappy 9to5)  || m*_3, bylabel(Polarized) || m*_4, bylabel(High intrinsic/low reward) || m*_5, bylabel(High-quality) drop(_cons educ dvage urbarea cohab southeast child loglsoa logsoc4) legend(row(1) label(2 "Base") label(4 "SD") label(6 "Local") label(8  "Occ")) xline(0, lstyle(foreground) lcolor(black)) byopts(xrescale row(1)  title("(a) Ethno-religious penalty (95% CI) on Job Quality: Men", size(vsmall)))   xtitle("predicted average marginal effects (%-point)", size(vsmall))


*women
foreach num of numlist 1/5 {
logit job`num' i.ethcats if fem == 1 [pw=nwei],  robust cluster(pidp)
	est store fb_`num'
logit job`num' i.ethcats educ dvage urbarea cohab southeast child if fem == 1 [pw=nwei], robust cluster(pidp)
est store fsd_`num'
logit job`num' i.ethcats educ dvage urbarea cohab southeast child loglsoa if fem == 1 [pw=nwei], robust cluster(pidp)
est store floc_`num'
logit job`num' i.ethcats educ dvage urbarea cohab southeast child loglsoa logsoc4 if fem == 1 [pw=nwei], robust cluster(pidp)
est store focc_`num'
est tab  fb_`num' fsd_`num' floc_`num' focc_`num', star stat(N r2_p) b(%8.4g) var(30)
}



coefplot (fb_1, msymbol(D)  mcolor(black) ciopts(recast(rcap) color(black))) (fsd_1, msymbol(T) mcolor(black*0.4) ciopts(recast(rcap) color(black*0.4))) (floc_1, msymbol(S) mcolor(gray) ciopts(recast(rcap) color(gray))) (focc_1, msymbol(O) mcolor(gray*0.40) ciopts(recast(rcap) color(gray*0.40))) , bylabel(Bad job) || f*_2, bylabel(Unhappy 9to5)  || f*_3, bylabel(Polarized) || f*_4, bylabel(High intrinsic/low reward) || f*_5, bylabel(High-quality) drop(_cons educ dvage urbarea cohab southeast child loglsoa logsoc4) legend(row(1) label(2 "Base") label(4 "SD") label(6 "Local") label(8  "Occ")) xline(0, lstyle(foreground) lcolor(black)) byopts(xrescale row(1)  title("(b) Ethno-religious penalty (95% CI) on Job Quality: Women", size(vsmall)) )  xtitle("predicted average marginal effects (%-point)", size(vsmall))



sav $ptemp/withmodel4.dta, replace

