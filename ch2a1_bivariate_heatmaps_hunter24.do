** ************************************************************************** **
	* Chapter 2 Analysis 1 - Creating Bivariate Heat Maps 
	* Part of PhD research on assessing impact of contemporary migration 
	* patterns the Muslim community in the UK. 
	* Last updated 6 FEB 2024
	* Scot Hunter 
** ************************************************************************** **



cd "/Users/scothunter/Library/CloudStorage/OneDrive-UniversityofStirling/thesis/directory/data/geounits"

*create gor shape file 
spshape2dta "/Users/scothunter/thesis/geo/NUTS_Level_1_January_2018_Boundaries.shp" , saving(gor) replace
geoframe create gor, replace 
save gor.dta, replace
* create lad shape file 
use lad11cd ladlab raw_lad* log_lad*  rgn using $geo/macro_master.dta, clear
egen ltag = tag(lad11cd)
keep if ltag 
drop ltag
save $geo/lad_for_bimap.dta, replace
spshape2dta $geo/lad_2011_.shp, saving(lad) replace
label var _ID "Local Authority District boundaries"
geoframe create lad, replace
rename lad11cd geo_code
encode geo_code, gen(lad11cd)
merge 1:1 lad11cd using $geo/lad_for_bimap.dta,
keep if _merge == 3
drop _merge 
label var _ID "Local Authority District boundaries"
sav laddta.dta, replace

*create lsoa shape file 
use lsoa11cd lsoalab raw_lsoa* log_lsoa* log_lad* ladlab raw_lad* lsoa_total_n raw_lad_muslim rgn using $geo/macro_master.dta, clear
egen ltag = tag(lsoa11cd)
keep if ltag 
egen ladtag =tag(raw_lad_muslim )
save $geo/lsoa_for_bimap.dta, replace
spshape2dta $geo/infuse_lsoa_lyr_2011_clipped.shp, saving(poli) replace
geoframe create poli, replace
encode geo_code, gen(lsoa11cd)
merge 1:1 lsoa11cd using $geo/lsoa_for_bimap.dta,
drop _merge 
sav mapdata.dta, replace 

use mapdata.dta, clear
* Whole UK 

bimap ///
	raw_lsoa_muslim raw_lsoa_eu ///
	using poli_shp,  palette(bluered) ///
	cut(pctile) percent values ///
	ocolor() osize(none) ///
    texty("Share of Muslims") textx("Share of post-04 EU migrants") ///
	polygon(data("gor_shp")) ///
	label(data("gor.dta") xcoord(_CX) ycoord(_CY) label(_ID)  color(white)  select(drop if _ID == 2 | _ID == 7) size(large) pos(0 0 7)) 
 graph save $geo/final_uk_map2.gph, replace

	
capture frame drop 
copy frame default gor 
frame gor { 
	clear 
	set obs 15 
	gen x = _n 
	gen y = _n 
global textopts ", size(vsmall) j(left) placement(3)"
scatter y x, msymbol(none) xlabel(, nogrid) ylabel(, nogrid) ///
yscale(off) xscale(off) fxsize(30) ///
text(14 1 "{bf:Government Office Regions}" $textopts ) ///
text(13 1 "1 North East  (2.7% /  1.6%)" $textopts) ///
text(12 1 "2 North West (5.5% / 2.2%)" $textopts) ///
text(11 1 "3 Yorkshire and the Humber (6.6% / 2.5%)" $textopts) ///
text(10 1  "4 East Midlands (3.9% / 3.1%)" $textopts) ///
text(9 1  "5 West Midlands (7.1% / 2.6%)" $textopts) ///
text(8 1 " 6 East of England (3.4% / 3.1%)" $textopts) ///
text(7 1 " 7 London (13% / 5.2%)" $textopts) ///
text(6 1 " 8 South West (1.9% / 2.5%)" $textopts) ///
text(5 1 " 9 South East (3.2% / 2.7%)" $textopts) ///
text(4 1 "10 Wales (2.4% / 2%)" $textopts) ///
text(3 1 "11 Scotland (2.4% / 2.7% )" $textopts) ///
text(2 1 "12 Northern Ireland (1.1% / 3.3%)" $textopts) ///
text(1 1 "(Muslim share/post-04 EU Migrant share in GORs)", size(tiny) j(left) placement(3) ) 
graph save $geo/gortext.gph, replace
}

graph combine $geo/gortext.gph $geo/final_uk_map2.gph , row(1)
graph save $geo/big.gph, replace

*used map editor 
// $geo/gor_combo.gph



use laddta.dta, clear
list rgn _ID if rgn == 3
global lon_lad "_ID == 294 | _ID == 295 | _ID == 296 | _ID == 297 | _ID == 298 | _ID == 299 | _ID == 300 | _ID == 301 | _ID == 302 | _ID == 303 | _ID == 304 | _ID == 305 | _ID == 306 | _ID == 307 | _ID == 308 | _ID == 309 | _ID == 310  | _ID == 311 | _ID == 312 | _ID == 313 | _ID == 314 | _ID == 315 | _ID == 316 | _ID == 317 | _ID == 318 | _ID == 319 | _ID == 320 | _ID == 321 | _ID == 322 | _ID == 323 | _ID == 324 | _ID == 325 | _ID == 326"
list rgn _ID if rgn == 10 /* west midlands */ 
global wmid_lad " _ID == 19 | _ID == 20 | _ID == 21 | _ID == 51 | _ID == 214 | _ID == 215 | _ID == 216 | _ID == 217 | _ID == 218 | _ID == 219 | _ID == 220 | _ID == 221 | _ID == 240 | _ID == 241 | _ID == 242 | _ID == 243 | _ID == 244 | _ID == 252 | _ID == 253 | _ID == 254 | _ID == 255 | _ID == 256 | _ID == 257 | _ID == 282 | _ID == 283 | _ID == 284 | _ID == 285 | _ID == 286 | _ID == 287 | _ID == 288"
list rgn _ID if rgn == 11 /* yorkshire*/
global york_lad "_ID == 10 | _ID == 11 | _ID == 12 | _ID == 13 | _ID == 14 | _ID == 190 | _ID == 191 | _ID == 192 | _ID == 193 | _ID == 194 | _ID == 195 | _ID == 196 | _ID == 273 | _ID == 274 | _ID == 275 | _ID == 276 | _ID == 289 | _ID == 290 | _ID == 291 | _ID == 292 | _ID == 293"
list rgn _ID if rgn == 5 /* north west */ 
global nw_lad "_ID == 6 | _ID == 7 | _ID == 8 | _ID == 9 | _ID == 49 | _ID == 50 | _ID == 66 | _ID == 67 | _ID == 68 | _ID == 69 | _ID == 70 | _ID == 71 | _ID == 150 | _ID == 151 | _ID == 152 | _ID == 153 | _ID == 154 | _ID == 155 | _ID == 156 | _ID == 157 | _ID == 158 | _ID == 159 | _ID == 260 | _ID == 261 | _ID == 262 | _ID == 263 | _ID == 264 | _ID == 265 | _ID == 266 | _ID == 267 | _ID == 268 | _ID == 269 | _ID == 270 | _ID == 271 | _ID == 272" 

use mapdata.dta, clear
bimap ///
	raw_lsoa_muslim raw_lsoa_eu using poli_shp if rgn == 3  ///
	,  palette(bluered) ///
	cut(pctile) percent values ///
	ocolor() osize(none) ///
    texty("Share of Muslims in LSOA") ///
	textx("Share of post-04 EU migrants in LSOA") ///
	polygon(data("lad_shp")  osize(0.5) select(keep if $lon_lad) ///
	legenda(on) leglabel(Local Authority District boundaries) ) ///
	label(data("laddta") xcoord(_CX) ycoord(_CY) label(_ID) color(white) ///
	select(keep if $lon_lad) size(*0.9 ..) pos(0 6)) 	
	graph save $geo/lon_lad_bimap.gph, replace 
bimap ///
	raw_lsoa_muslim raw_lsoa_eu using poli_shp if rgn == 10  ///
	,  palette(bluered) ///
	cut(pctile) percent values ///
	ocolor() osize(none) ///
    texty("Share of Muslims in LSOA") ///
	textx("Share of post-04 EU migrants in LSOA") ///
	polygon(data("lad_shp")  osize(0.5) select(keep if $wmid_lad) ///
	legenda(on) leglabel(Local Authority District boundaries) ) ///
	label(data("laddta") xcoord(_CX) ycoord(_CY) label(_ID) color(white) ///
	select(keep if $wmid_lad) size(*0.9 ..) pos(0 6))
	graph save $geo/wmid_lad_bimap.gph, replace 
bimap ///
	raw_lsoa_muslim raw_lsoa_eu using poli_shp if rgn == 11  ///
	,  palette(bluered) ///
	cut(pctile) percent values ///
	ocolor() osize(none) ///
    texty("Share of Muslims in LSOA") ///
	textx("Share of post-04 EU migrants in LSOA") ///
	polygon(data("lad_shp")  osize(0.5) select(keep if $york_lad) ///
	legenda(on) leglabel(Local Authority District boundaries) ) ///
	label(data("laddta") xcoord(_CX) ycoord(_CY) label(_ID) color(white) ///
	select(keep if $york_lad) size(*0.9 ..) pos(0 6)) 	
	graph save $geo/york_lad_bimap.gph, replace 
bimap ///
	raw_lsoa_muslim raw_lsoa_eu using poli_shp if rgn == 5  ///
	,  palette(bluered) ///
	cut(pctile) percent values ///
	ocolor() osize(none) ///
    texty("Share of Muslims in LSOA") ///
	textx("Share of post-04 EU migrants in LSOA") ///
	polygon(data("lad_shp")  osize(0.5) select(keep if $nw_lad) ///
	legenda(on) leglabel(Local Authority District boundaries) ) ///
	label(data("laddta") xcoord(_CX) ycoord(_CY) label(_ID) color(white) ///
	select(keep if $nw_lad) size(*0.9 ..) pos(0 6))  	
	graph save $geo/nw_lad_bimap.gph, replace 

// use laddta.dta, replace
// list _ID ladlab raw_lad_muslim raw_lad_eu if rgn == 3

capture frame drop london 
frame copy default london
frame london {
	clear 
	set obs 36
	gen x = _n 
	gen y = _n 
	global textopts ", size(vsmall) j(left) placement(3)"
	scatter y x, msymbol(none) xlabel(, nogrid) ylabel(, nogrid) ///
	yscale(off) xscale(off) fxsize(30) ///
text(36 1 "{bf:London Local Authority Districts}" "{bf:sorted by share of Muslims}" "{bf:(from high to low)}" $textopts ) ///
text(33 1 "323 Tower Hamlets (36%  /  3.3%)" $textopts) ///
text(32 1 "318 Newham (33% / 9.9%)" $textopts) ///
text(31 1 "319 Redbridge (24% / 4.9%)" $textopts) ///
text(30 1 "324 Waltham Forest (23% / 10%)" $textopts) ///
text(29 1 "298 Brent (20% / 9.3%)" $textopts) ///
text(28 1 "326 Westminster (19% / 2.8%)" $textopts) ///
text(27 1 "303 Enfield (18% / 5.2%)" $textopts) ///
text(26 1 "302 Ealing (17% / 11%)" $textopts) ///
text(25 1 "307 Haringey (15% / 8.9%)" $textopts) ///
text(24 1 "305 Hackney (15% / 4%)" $textopts) ///
text(23 1 "311 Hounslow (15% / 8%)" $textopts) ///
text(22 1 "295 Barking and Dagenham (15% / 7%)" $textopts) ///
text(21 1 "308 Harrow (13% / 6.8%)" $textopts) ///
text(20 1 "300 Camden (13% / 2.8%)" $textopts) ///
text(19 1 "310 Hillingdon (12% / 4.3%)" $textopts) ///
text(18 1 "296 Barnet (11% / 7%)" $textopts) ///
text(17 1 "306 Hammersmith and Fulham (11% / 3.9%)" $textopts) ///
text(16 1 "313 Kensington and Chelsea (11% / 2.6%)" $textopts) ///
text(15 1 "312 Islington (10% / 2.5%)" $textopts) ///
text(14 1 "321 Southwark (9.5% / 3.4%)" $textopts) ///
text(13 1 "317 Merton (9.1% / 7%)" $textopts) ///
text(12 1 "301 Croydon (9.1% / 3.9%)" $textopts) ///
text(11 1 "325 Wandsworth (9.1% / 4.5%)" $textopts) ///
text(10 1 "315 Lambeth (8.1% / 4.7%)" $textopts) ///
text(9 1 "304 Greenwich (7.8% / 4.1%)" $textopts) ///
text(8 1 "316 Lewisham (7.4% / 4.4%)" $textopts) ///
text(7 1 "314 Kingston upon Thames (6.9% / 3.6%)" $textopts) ///
text(6 1 "294 City of London (6.5% / 2.3%)" $textopts) ///
text(5 1 "322 Sutton (5.1% / 3.2%)" $textopts) ///
text(4 1 "320 Richmond upon Thames (4.3% / 3%)" $textopts) ///
text(3 1 "299 Bromley (3.5% / 2.5%)" $textopts) ///
text(2 1 "297 Bexley (3.4% / 2.3%)" $textopts) ///
text(1 1 "309 Havering (3% / 2.6%)" $textopts) ///
text(0 1 "(Muslim share/post-04 EU Migrant share in LADs)", size(tiny) j(left) placement(3)) 
graph save $geo/lontext.gph, replace 
} 


// use laddta.dta, replace
// gsort -raw_lad_muslim
// list _ID ladlab raw_lad_muslim raw_lad_eu if rgn == 10

capture frame drop wmidlands 
frame copy default wmidlands 
frame wmidlands {
	clear 
	set obs 33
	gen x = _n 
	gen y = _n 
	global textopts ", size(vsmall) j(left) placement(3)"
	scatter y x, msymbol(none) xlabel(, nogrid) ylabel(, nogrid) ///
	yscale(off) xscale(off) fxsize(30) ///
text(33 1 "{bf:West Midlands Local Authority Districts}" "{bf:sorted by share of Muslims}" "{bf:(from high to low)}" $textopts ) ///
text(30 1 "282 Birmingham (23% / 2.7%)" $textopts ) ///
text(29 1 "287 Walsall (9.2% / 2.1%)" $textopts) ///
text(28 1 "285 Sandwell (9.2% / 4.3%)" $textopts) ///
text(27 1 "283 Coventry (8.5% / 4.7%)" $textopts) ///
text(26 1 "21 Stoke-on-Trent (7% / 2.3%)" $textopts) ///
text(25 1 "215 East Staffordshire (7% / 4%)" $textopts) ///
text(24 1 "284 Dudley (5.1% / 1.5%)" $textopts) ///
text(23 1 "288 Wolverhampton (4.6% / 3.3%)" $textopts) ///
text(22 1 "254 Redditch (4.4% / 4.6%)" $textopts) ///
text(21 1 "255 Worcester (3.9% / 3.3%)" $textopts) ///
text(20 1 "286 Solihull (3.5% / 1.5%)" $textopts) ///
text(19 1 "241 Nuneaton and Bedworth (3.3% / 2.5%)" $textopts)  ///
text(18 1 "20 Telford and Wrekin (2.8% / 3.2%)" $textopts) ///
text(17 1 "242 Rugby (2.2% / 5%)" $textopts) ///
text(16 1 "217 Newcastle-under-Lyme (2.1% / 1.7%)" $textopts) ///
text(15 1 "244 Warwick (1.9% / 2.3%)" $textopts) ///
text(14 1 "219 Stafford (1.9% / 2.1%)" $textopts) ///
text(13 1 "257 Wyre Forest (1.7% / 2.1%)" $textopts) ///
text(12 1 "252 Bromsgrove (1.5% / 1.3%)" $textopts) ///
text(11 1 "216 Lichfield (1.4% / 1.8%)" $textopts) ///
text(10 1 "218 South Staffordshire (1.3% / 1.2%)" $textopts) ///
text(9 1 "51 Shropshire (1.3% / 2%)" $textopts) ///
text(8 1 "256 Wychavon (1.3% / 3.5%)" $textopts) ///
text(7 1 "221 Tamworth (1.3% / 2.3%)" $textopts) ///
text(6 1 "253 Malvern Hills (1.3% / 1.6%)" $textopts) ///
text(5 1 "243 Stratford-on-Avon (1.2% / 2.8%)" $textopts) ///
text(4 1 "214 Cannock Chase (1.2% / 1.5%)" $textopts)  ///
text(3 1 "19 Herefordshire, County of (1.2% / 4.3%)" $textopts) ///
text(2 1 "220 Staffordshire Moorlands (1.2% / 1.6%)" $textopts) ///
text(1 1 "240 North Warwickshire (1.2% / 1.7%)" $textopts) ///
text(0 1 "(Muslim share/post-04 EU Migrant share in LADs)", size(tiny) j(left) placement(3)) 
graph save $geo/wmidtext.gph, replace 
}

// list _ID ladlab raw_lad_muslim raw_lad_eu if rgn == 11
	
capture frame drop york
frame copy default york
frame york {
	clear 
	set obs 24
	gen x = _n 
	gen y = _n 
	global textopts ", size(vsmall) j(left) placement(3)"
	scatter y x, msymbol(none) xlabel(, nogrid) ylabel(, nogrid) ///
	yscale(off) xscale(off) fxsize(30) ///
text(23 1 "{bf:Yorkshire and the Humber}" "{bf:Local Authority Districts}" "{bf:sorted by share of Muslims}" "{bf:(from high to low)}" $textopts ) ///
text(21 1 "289 Bradford (26% / 3%)" $textopts) ///
text(20 1 "291 Kirklees (16% / 2.3%)" $textopts) ///
text(19 1 "276 Sheffield (8.7% / 1.9%)" $textopts) ///
text(18 1 "290 Calderdale (8.3% / 2.3%)" $textopts) ///
text(17 1 "292 Leeds (6.4% / 2.7%)" $textopts) ///
text(16 1 "275 Rotherham (4.7% / 1.8%)" $textopts) ///
text(15 1 "10 Kingston upon Hull, City of (3.1% / 4.4%)" $textopts) ///
text(14 1 "293 Wakefield (3% / 3.2%)" $textopts) ///
text(13 1 "13 North Lincolnshire (2.8% / 4.3%)" $textopts) ///
text(12 1 "274 Doncaster (2.7% / 3.7%)" $textopts) ///
text(11 1 "14 York (2% / 2.5%)" $textopts) ///
text(10 1 "190 Craven (1.9% / 1.8%)" $textopts) ///
text(9 1 "12 North East Lincolnshire (1.8% / 2.3%)" $textopts) ///
text(8 1 "195 Scarborough (1.5% / 2.4%)" $textopts) ///
text(7 1 "273 Barnsley (1.4% / 2.2%)" $textopts) ///
text(6 1 "11 East Riding of Yorkshire (1.4% / 1.8%)" $textopts) ///
text(5 1 "192 Harrogate (1.4% / 2.7%)" $textopts) ///
text(4 1 "191 Hambleton (1.3% / 1.8%)" $textopts) ///
text(3 1 "193 Richmondshire (1.3% / 1.7%)" $textopts) ///
text(2 1 "196 Selby (1.1% / 2.8%)" $textopts) /// 
text(1 1 "194 Ryedale (1.1% / 1.8%)" $textopts) ///
text(0 1 "(Muslim share/post-04 EU Migrant share in LADs)", size(tiny) j(left) placement(3)) 
graph save $geo/yorktext.gph, replace 
	} 
	
// list _ID ladlab raw_lad_muslim raw_lad_eu if rgn ==  5

capture frame drop nwest
frame copy default nwest
frame nwest {
	clear 
	set obs 43
	gen x = _n 
	gen y = _n 
	global textopts ", size(vsmall) j(left) placement(3)"
	scatter y x, msymbol(none) xlabel(, nogrid) ylabel(, nogrid) ///
	yscale(off) xscale(off) fxsize(30) ///
text(43 1 "{bf:North West Local Authority Districts}" "{bf:sorted by share of Muslims}" "{bf:(from high to low)}" $textopts ) ///
text(40 1 "8 Blackburn with Darwen (28% / 2.8%)" $textopts) ///
text(39 1 "261 Oldham (19% / 1.8%)" $textopts) ///
text(38 1 "155 Pendle (18% / 3.1%)" $textopts) ///
text(37 1 "260 Manchester (17% / 3.4%)" $textopts) ///
text(36 1 "262 Rochdale (15% / 2.6%)" $textopts) ///
text(35 1 "266 Trafford (6.7% / 2.4%)" $textopts) ///
text(34 1 "265 Tameside (5.4% / 2.3%)" $textopts) ///
text(33 1 "158 Rossendale (4.8% / 1.4%)" $textopts) ///
text(32 1 "264 Stockport (4.3% / 1.6%)" $textopts) ///
text(31 1 "269 Liverpool (4.3% / 2.3%)" $textopts) ///
text(30 1 "263 Salford (3.6% / 3.6%)" $textopts) ///
text(29 1 "154 Lancaster (2.3% / 3.1%)" $textopts) ///
text(28 1 "151 Chorley (2.1% / 1.7%)" $textopts) ///
text(27 1 "7 Warrington (2% / 2.5%)" $textopts) ///
text(26 1 "9 Blackpool (1.7% / 2.9%)" $textopts) ///
text(25 1 "157 Ribble Valley (1.7% / 1.7%)" $textopts) ///
text(24 1 "267 Wigan (1.7% / 1.9%)" $textopts) ///
text(23 1 "49 Cheshire East (1.7% / 2.7%)" $textopts) ///
text(22 1 "272 Wirral (1.6% / 1.5%)" $textopts) ///
text(21 1 "159 South Ribble (1.5% / 1.7%)" $textopts) ///
text(20 1 "50 Cheshire West and Chester (1.5% / 2.1%)" $textopts) ///
text(19 1 "152 Fylde (1.5% / 1.6%)" $textopts) ///
text(18 1 "271 Sefton (1.4% / 2.2%)" $textopts) ///
text(17 1 "68 Carlisle (1.4% / 2.9%)" $textopts) ///
text(16 1 "270 St. Helens (1.3% / 1.4%)" $textopts) ///
text(15 1 "69 Copeland (1.3% / 1.4%)" $textopts) ///
text(14 1 "268 Knowsley (1.3% / 1.3%)" $textopts) ///
text(13 1 "161 Wyre (1.3% / 1.3%)" $textopts) ///
text(12 1 "160 West Lancashire (1.2% / 1.9%)" $textopts) ///
text(11 1 "67 Barrow-in-Furness (1.2% / 1.5%)" $textopts) ///
text(10 1 "70 Eden (1.2% / 2.3%)" $textopts) ///
text(9 1 "6 Halton (1.2% / 1.5%)" $textopts) ///
text(8 1 "66 Allerdale (1.2% / 1.5%)" $textopts) ///
text(7 1 "71 South Lakeland (1.2% / 2.4%)" $textopts) ///
text(6 1 "Barrow-in-Furness (1.2% / 1.5%)" $textopts) ///
text(5 1 "70 Eden (1.2% / 2.3%)" $textopts) ///
text(4 1 "6 Halton (1.2% / 1.5%)" $textopts) ///
text(3 1 "66 Allerdale (1.2% / 1.5%)" $textopts) ///
text(2 1 "71 South Lakeland (1.2% / 2.4%)" $textopts) ///
text(1 1 "Barrow-in-Furness (1.2% / 1.5%)" $textopts) ///
text(0 1 "(Muslim share/post-04 EU Migrant share in LADs)", size(tiny) j(left) placement(3)) 
graph save $geo/nwesttext.gph, replace 
}

graph combine $geo/lontext.gph $geo/lon_lad_bimap.gph , row(1) ///
title( "(b) Bivariate share of Muslim and post-04 Migrants in LSOA neighbourhoods (London)", size(vsmall))
graph save $geo/g1_london.gph, replace 

graph combine $geo/wmidtext.gph $geo/wmid_lad_bimap.gph , row(1) ///
title( "(c) Bivariate share of Muslim and post-04 Migrants in LSOA neighbourhoods (West Midlands)", size(vsmall))
graph save $geo/g2_wmid.gph, replace 

graph combine $geo/yorktext.gph $geo/york_lad_bimap.gph , row(1) ///
title( "(d) Bivariate share of Muslim and post-04 Migrants in LSOA neighbourhoods (Yorkshire and the Humber)", size(vsmall))
graph save $geo/g3_york.gph, replace 

graph combine $geo/nwesttext.gph $geo/nw_lad_bimap.gph , row(1) ///
title( "(e) Bivariate share of Muslim and post-04 Migrants in LSOA neighbourhoods (North West)", size(vsmall))
graph save $geo/g4_nwest.gph, replace 

// graph combine  $geo/g1.gph $geo/g2.gph $geo/g3.gph $geo/g4.gph, row(4) 
// graph save $geo/gg.gph, replace 
//
// graph combine $geo/big.gph $geo/gg.gph , row(1)
