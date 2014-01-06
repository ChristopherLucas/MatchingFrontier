/* Analysis for Aid Shocks

  This file puts the dataset together.

  Note that while most of this analysis is in Stata 9.2,
  some of it is in R 2.10.1.  The R code is in with the stata
  code, usually indicated with something like:
	********************
	** R code for blah blah
	*******************
  Just copy and paste it in to R and it should run, as long as you specify
  your own directories.

  The chunks of R code that are necessary are NOT commented out.
  This means that you can highlight all of this script, run it in
  Stata, and it will crash where you need to jump over to R and 
  run something.

  Some chunks of unnecessary code are commented out.

  The files necessary to run this analysis are all in the folder ~\data.
  Set your directory to the main folder with this script in it.

	You'll want to enter the following directly from the command line: 
					set mem 500M
					set more off
					cd "your/directory/here"
					** Or whatever your directory is.
 	from the command line, so that the file can run all the way through.
*/


****************************************************************
** setting up the data
****************************************************************
clear
set mem 500M
set more off

** make a folder to hold all the junk files that we'll be generating
** and using along the way.
capture mkdir "crap"

**  I use gleditsch's gdp data as a base 
set mem 500M
insheet using "data\expgdp_v5.0.asc", delimiter(" ")
gen countryname_g="proper(countryname)"
rename stateid countrycode_g
run "data\Standardize Country Codes.demaid.do"
move  countryname_g countrycode_g
drop countrynumcode_g origin
rename statenum countrynumcode_g
sort  countryname_g year
rename  pop population //(Units are in 1000's)
rename  rgdpch rgdpc
gen ln_population=ln(population*1000)
gen ln_population_sq=ln_population^2
gen ln_rgdpc=ln(rgdpc)
notes  ln_rgdpc: This is from Gleditsch.  "The real GDP per capita figures are real figures in constant US dollars (base 1996)" TS
gen ln_rgdpc_sq=ln_rgdpc^2

egen countrynum=group(countryname)
tsset countrynum year

gen rgdp=rgdpc*population
gen rgdpcgrowth=((rgdpc-l.rgdpc)/l.rgdpc)*100
gen rgdpgrowth=((rgdp-l.rgdp)/l.rgdp)*100
rename countryname_g countryname


**  This adds assorted vars from the WDI 
save "crap\Master dataset LD.1.1.dta", replace
clear
use "data\Selected WDI vars.dta", clear
run "data\Standardize Country Names.demaid.do"
run "data\Standardize Country Codes.demaid.do"
move  countrycode_g year
drop if  countrycode_g=="Country Code (Gleditsch)"
/* in WDI, Czech Rep. and Slovakia are seperate before 1993, but 
   there is not good way of capturing Czechoslovakia.  */
drop if  stateinyeart_g==. &  microstateinyeart_g==.
save "crap\WDI data for merge.dta", replace
clear
use "crap\Master dataset LD.1.1.dta", clear
merge countryname year using "crap\WDI data for merge.dta", unique sort _merge(_merge_wdi_vars)
move _merge_wdi_vars year
drop if _merge_wdi_vars==2  /*drops after 2000 and micro-states */
drop _merge_wdi_vars

gen ln_landarea=ln(landarea)

  ** Save the data
save "crap\junk vars for CW.dta", replace


** creating the first DV from scratch with the PRIO data
** Data downloaded from http://www.pcr.uu.se/research/UCDP/data_and_publications/datasets.htm on Jan 19, 2009
clear
set mem 500M
insheet using "data\prio.txt", tab
keep if  type==3 | type==4
gen prio=1

keep  id location year prio

sort id year
gen warstart=1 if id[_n-1]!=id[_n]

sort location year id
gen multiwar1 = 1 if id[_n-1] != id[_n] & year[_n-1]==year[_n]
replace multiwar1 = 1 if id[_n+1] != id[_n] & year[_n+1]==year[_n]

gen newwar1=1 if multiwar1==1 & warstart==1

egen newwar = max(newwar1), by(location year)
keep location year prio newwar


*keep  location year prio
rename location countryname
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
duplicates drop
save "crap\prio.dta", replace


****
** we also make a variable with just the PRIO type 3 conflicts (internal) -- because the US has a type 4 in 2001 with al-qaida
** creating the first DV from scratch with the PRIO data
** Data downloaded from http://www.pcr.uu.se/research/UCDP/data_and_publications/datasets.htm on Jan 19, 2009
clear
set mem 500M
insheet using "data\prio.txt", tab
keep if  type==3
gen priotype3=1

keep  id location year priotype3

sort id year
gen warstart=1 if id[_n-1]!=id[_n]

sort location year id
gen multiwar1 = 1 if id[_n-1] != id[_n] & year[_n-1]==year[_n]
replace multiwar1 = 1 if id[_n+1] != id[_n] & year[_n+1]==year[_n]

gen newwar1=1 if multiwar1==1 & warstart==1

egen newwar = max(newwar1), by(location year)
keep location year priotype3 newwar

rename location countryname
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
duplicates drop
save "crap\priotype3.dta", replace


****
** We also make a var that is for 1000 battle deaths or more 
** WITH ONSET BEING THE TIME THEY REACH 1000 DEATHS
/* "cumint": This variable takes into account the temporal dimension of the conflict. It is a dummy variable that
codes whether the conflict since the onset has exceeded 1,000 battle-related deaths. A conflict is
coded as 0 as long as it has not over time resulted in more than 1,000 battle-related deaths. Once a
conflict reaches this threshold, it is coded as 1.*/
** Data downloaded from http://www.pcr.uu.se/research/UCDP/data_and_publications/datasets.htm on Jan 19, 2009
clear
set mem 500M
insheet using "data\prio.txt", tab
keep if  type==3 | type==4
keep if cumint==1
gen prio1000=1

keep  id location year prio1000

sort id year
gen warstart=1 if id[_n-1]!=id[_n]

sort location year id
gen multiwar1 = 1 if id[_n-1] != id[_n] & year[_n-1]==year[_n]
replace multiwar1 = 1 if id[_n+1] != id[_n] & year[_n+1]==year[_n]

gen newwar1=1 if multiwar1==1 & warstart==1

egen newwar = max(newwar1), by(location year)
keep location year prio1000 newwar

rename location countryname
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
duplicates drop
save "crap\prio1000.dta", replace

** And now we make a variable that includes only the onset of all wars that eventually
** reach 1000 battle deaths.

clear
set mem 500M
insheet using "data\prio.txt", tab
keep if  type==3 | type==4
egen maxcumint = max(cumint), by(id)
keep if maxcumint==1
gen prio1k=1

keep  id location year prio1k

sort id year
gen warstart=1 if id[_n-1]!=id[_n]

sort location year id
gen multiwar1 = 1 if id[_n-1] != id[_n] & year[_n-1]==year[_n]
replace multiwar1 = 1 if id[_n+1] != id[_n] & year[_n+1]==year[_n]

gen newwar1=1 if multiwar1==1 & warstart==1

egen newwar = max(newwar1), by(location year)
keep location year prio1k newwar

rename location countryname
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
duplicates drop
save "crap\prio1k.dta", replace


**  Makes the Polity IV data for later merging
clear
insheet using "data\p4v2006.csv"
rename  country countryname
** These fixes cut the data off at 1945--not ideal but doesn't matter here 
drop if countryname=="Germany" & year==1945
drop if countryname=="Germany" & year==1990
drop if countryname=="Yugoslavia" & durable==37
drop if countryname=="Yemen" & year==1990
drop if countryname=="Ethiopia" &  year==1993 & polity2==0
drop if countryname=="Vietnam North" & year==1976
drop if year<1945
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
move  countrycode_g year
drop if  countrycode_g=="Country Code (Gleditsch)"
drop if countryname=="Yugoslavia (Serbia)" & year==2006
keep countryname year polity polity2
sort countryname year
  ** I think this recreates what Fearon and Laitin did except that the polity coding for Afghanistan has changed
  ** to compare, run (after merging): edit countryname year instab_old instab polity2_old polity2 polity_old polity 
gen instab=0 if polity!=.
replace instab=1 if abs(polity[_n-1]-polity[_n-2])>=3 & countryname[_n]==countryname[_n-2] & polity[_n-1]!=-66 & polity[_n-2]!=-66
replace instab=1 if abs(polity[_n-2]-polity[_n-3])>=3 & countryname[_n]==countryname[_n-3] & polity[_n-2]!=-66 & polity[_n-3]!=-66
replace instab=1 if abs(polity[_n-3]-polity[_n-4])>=3 & countryname[_n]==countryname[_n-4] & polity[_n-3]!=-66 & polity[_n-4]!=-66
replace instab=1 if polity[_n-1]==-77 | polity[_n-1]==-88 & countryname[_n]==countryname[_n-1]
replace instab=1 if polity[_n-2]==-77 | polity[_n-2]==-88 & countryname[_n]==countryname[_n-2]
replace instab=1 if polity[_n-3]==-77 | polity[_n-3]==-88 & countryname[_n]==countryname[_n-3]
replace instab=0 if polity==-66

tempfile polity
save "`polity'"




********************************************
** Merge in PITF variables, following (AJPS Replication), gotten here: http://globalpolicy.gmu.edu/pitf/AJPSmat.htm
********************************************
  ** Unfortunately, they didn't include the whole dataset, so I have to
  ** recreate the variables.  But I can check if I did it right, against theirs

  ** First, regime type (5 categories)
clear
insheet using "data\p4v2006.csv"
rename  country countryname
** These fixes cut the data off at 1945--not ideal but doesn't matter here 
drop if countryname=="Germany" & year==1945
drop if countryname=="Germany" & year==1990
drop if countryname=="Yugoslavia" & durable==37
drop if countryname=="Yemen" & year==1990
drop if countryname=="Ethiopia" &  year==1993 & polity2==0
drop if countryname=="Vietnam North" & year==1976
drop if year<1945
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
move  countrycode_g year
drop if  countrycode_g=="Country Code (Gleditsch)"
drop if countryname=="Yugoslavia (Serbia)" & year==2006
keep countryname year exrec parcomp
  ** See the table on page 196 of the AJPS article for more details
gen fullautocracy =1 if (parcomp==1 | parcomp==0) & (exrec>=1 & exrec<=5)
replace fullautocracy = 0 if fullautocracy!=1 & exrec>0

gen partautocracy =1 if ((parcomp>=2 & parcomp<=5) & (exrec>=1 & exrec<=5)) | ((parcomp==0 | parcomp==1) & (exrec>=6 & exrec<=8))
replace partautocracy = 0 if partautocracy!=1 & exrec>0

gen partdemocracy = 1 if ((parcomp==2 | parcomp==4) & (exrec>=6 & exrec<=8)) | ((parcomp==5) & (exrec>=6 & exrec<=7))
replace partdemocracy = 0 if partdemocracy!=1 & exrec>0

gen factionaldemoc = 1 if parcomp==3 & (exrec>=6 & exrec<=8)
replace factionaldemoc = 0 if factionaldemoc !=1 & exrec>0

gen fulldemocracy = 1 if parcomp==5 & exrec==8
replace fulldemocracy = 0 if fulldemocracy !=1 & exrec>0


sort countryname year
save "crap\PITFdemoc.dta", replace

  
  ** Infant mortality -- from the PITF website
  ** http://globalpolicy.gmu.edu/pitf/pitfdata.htm
  ** the var is LOGCIMR
clear
use "data\PITFp4data.dta", clear
keep  sftgcode year sftgname  logcimr
rename sftgname countryname

egen countrynum=group(country)
tsset countrynum year

foreach i of numlist 1/10 {
  capture drop leadlogcimr
  gen leadlogcimr = l`i'.logcimr
  replace logcimr = leadlogcimr if logcimr==. & leadlogcimr!=.
}

rename logcimr infant

run "data\New Standardize Country Names.do"
  ** There are some problematic duplicates here
  ** Not sure what to do about them
drop if infant==.
duplicates tag countryname year, gen(dup)
drop if dup==1
drop dup

save "crap\infantmort.dta", replace
  ** the var to look for is "infant"



  ** Bad neighborhood

  ** original data is here: http://www.systemicpeace.org/inscr/inscr.htm
clear
insheet using "data\MEPV04ex.csv", clear names
  ** the variable nciv is the count of neighboring countries with a civil or ethnic conflict
keep scode ccode country year nciv
rename country countryname
run "data\New Standardize Country Names.do"
drop if year<1960

save "crap\badneighbor.dta", replace
  ** variable to look for is "nciv"


  ** State-led discrimination

  ** The MAR data are available here: http://www.cidcm.umd.edu/mar/data.asp
  ** The variables I'm looking for are POLDIS and ECDIS
  ** I downloaded it with MARgene  on feb 17 2010
  ** I used "carry forward" interpolation
clear
insheet using "data\MAR.17feb2010.csv", clear names
keep  year_1 ccode_mean country_min  ecdis_max poldis_max
rename year_1 year
rename ccode_mean ccode
rename country_min countryname
rename ecdis_max ecdis
rename poldis_max poldis
run "data\New Standardize Country Names.do"

  ** there is a GERMANY/WEST GERMANY duplicate problem, but there is no score
duplicates tag countryname year, gen(dup)
drop if dup==1
drop dup

  ** none of the discrimination data goes back before 1980 --
  ** I use the leads to get it back to 1975
replace ecdis = . if ecdis==-99
replace poldis = . if poldis==-99

egen countrynum = group(countryname)
tsset countrynum year

foreach i of numlist 1/3{
  capture drop leadecdis
  gen leadecdis = f`i'.ecdis
  replace ecdis = leadecdis if ecdis==. & leadecdis!=. & year < 1980
}

foreach i of numlist 1/3{
  capture drop leadpoldis
  gen leadpoldis = f`i'.poldis
  replace poldis = leadpoldis if poldis==. & leadpoldis!=. & year < 1980
}

gen statediscrim = 1 if (ecdis>=4 & ecdis!=.) | (poldis>=4 & poldis!=.)

** NOTE: this doesn't have 2004 in it, so I just lag it
** I should be lagging all of these information variables anyways
gen lstatediscrim = l.statediscrim
replace lstatediscrim = 0 if lstatediscrim!=1

save "crap\statediscrim.dta", replace
  ** variable to look for is "lstatediscrim"


*********************************************
** End PITF dataset creation
*********************************************

********************************************
** Add the WDI infant mortality varaibles
********************************************
insheet using "data\water and health.csv", clear
rename timename year
drop if year<1960
drop if countryname == "Macao, China"
run "data\New Standardize Country Names.do"
egen countrynum = group(countryname)
tsset countrynum year
tssmooth ma infantmort = mortalityrateinfantper1000livebi, w(4 1 0)
drop countrynum

save "crap\infantwdi.dta", replace
  ** variable to look for is " mortalityrateinfantper1000livebi" of "infantmort"


********************************************
** Add CIRI and PTS
********************************************
insheet using "data\CIRI 2007.csv", clear
rename ctry countryname
  ** deal with some duplicates
drop if countryname=="Russia" & year<=1991
drop if countryname=="Soviet Union" & year>=1992
drop if countryname=="Serbia" & year <=2005
drop if countryname=="Serbia and Montenegro" & (year<=1991 | year>= 1998)
drop if countryname=="Serbia and Montenegro" & (year<=2002 | year>= 2006)
drop if countryname=="Yugoslavia" & year >= 1992
drop if countryname=="Yugoslavia, Federal Republic of" & (year<=1999 | year>=2003)
drop if countryname=="Yemen" & year <=1990
drop if countryname=="Yemen Arab Republic" & year>=1991
run "data\New Standardize Country Names.do"
keep countryname year physint

save "crap\ciri.dta", replace
  ** variable to look for is "physint"


  ** PTS
use "data\PTS.dta", clear
rename COW_num ccode1
gen country=""
  ** drops "west bank and gaza"
drop if wbcode =="WBG"
  ** fixes USSR duplicates
drop if wbcode=="RUS" & year <=1991
drop if wbcode=="USSR" & year>=1992
run "data\COW convert.do"
replace country = "USSR" if wbcode=="USSR"
rename country countryname
run "data\New Standardize Country Names.do"


save "crap\pts.dta", replace
  ** variable to look for is "PTSave_filled"




********************************************
** Add Banks Riots
********************************************
  
insheet using "data\CNTSDATA.csv", clear
rename country countryname
drop if year<1973
run "data\New Standardize Country Names.do"

drop if countrycode=="missing"

keep countryname year domestic*
rename domestic1 assassinbanks
rename domestic2 strikesbanks
rename domestic6 riotsbanks
rename domestic8 demonstrationsbanks
gen bankscombined = assassinbanks + strikesbanks + riotsbanks + demonstrationsbanks

save "crap\banks.dta", replace
  ** variable to look for is "domestic"


********************************
** End Extra variables
********************************


** Then starting with the other variables drawn from Sambanis
clear
use "data\ISA version.dta", clear
drop polity2
rename country countryname
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
merge countryname year using "crap\junk vars for CW.dta", unique sort _merge(_mergejunk)
*merge countryname year using "wdi aid vars.dta", unique sort _merge(_mergewdi)

  ** add PITF vars
merge countryname year using "crap\PITFdemoc.dta", unique sort _merge(_mergePITF1)
merge countryname year using "crap\infantmort.dta", unique sort _merge(_mergePITF2)
merge countryname year using "crap\badneighbor.dta", unique sort _merge(_mergePITF3)
merge countryname year using "crap\statediscrim.dta", unique sort _merge(_mergePITF4)


  ** add WDI infant mortality vars
merge countryname year using "crap\infantwdi.dta", unique sort _merge(_mergeinfant)
drop if _mergeinfant==2
  ** merge CIRI
merge countryname year using "crap\ciri.dta", unique sort _merge(_mergeciri)
drop if _mergeciri==2
  ** merge Banks' riots
merge countryname year using "crap\banks.dta", unique sort _merge(_mergebanks)
drop if _mergebanks==2

  ** PTS 
merge countryname year using "crap\pts.dta", unique sort _merge(_mergepts)
drop if _mergepts==2


** extend the constant variables through 2004
rename instab instab_old
rename polity2 polity2_old
rename polity polity_old

sort countryname year
replace logmtn=logmtn[_n-1] if logmtn==. & countryname[_n]==countryname[_n-1]
replace ethfrac=ethfrac[_n-1] if ethfrac==. & countryname[_n]==countryname[_n-1]
replace relfrac=relfrac[_n-1] if relfrac==. & countryname[_n]==countryname[_n-1]
replace ncontig=ncontig[_n-1] if ncontig==. & countryname[_n]==countryname[_n-1]
replace ncontig=ncontig[_n-1] if ncontig==. & countryname[_n]==countryname[_n-1]

** update nwstate (are there any?)  FOR THE MOMENT I ASSUME NOT
sort countryname year
*assert countryname[_n]!=countryname[_n-1] & year> 1999 & year < 2005
  ** assertion should be false for all obs.
replace nwstate=0 if nwstate==. & countryname[_n]==countryname[_n-1]


  ** merge in a new version of Polity
  ** use the new version of Polity to code "instab"
merge countryname year using "`polity'", unique sort _merge(_mergePolity)


** The old "oil" variable
** update "oil" using WDI data
** THIS IS A HACK!!! FIX LATER
replace oil=oil[_n-1] if oil==. & countryname[_n]==countryname[_n-1]

  ** This merges in Ross's oil data
save crap/fulldata, replace
use "data\Ross Oil & Gas 1932-2006 public.dta", clear
rename cty_name countryname
run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"
drop if stateinyeart_g!=1
keep countryname year oil_gas_rentTOTAL
rename oil_gas_rentTOTAL rossoil
replace rossoil=rossoil/1000000000
save "crap\ross merge.dta", replace

use crap/fulldata, clear
merge countryname year using "crap\ross merge", unique sort _merge(_mergeRoss)
replace oil=rossoil
drop _mergeRoss
erase "crap\ross merge.dta"


capture drop countrynum
egen countrynum=group(countryname)
tsset countrynum year

capture drop rgdp
gen rgdp=rgdpc*(population*1000)
gen lnrgdp=ln(rgdp)

gen llnrgdp=l.lnrgdp
gen lpolity2=l.polity2
gen lln_rgdpc=l.ln_rgdpc
gen gdpenlag2=l.gdpen

** lagging a couple of other vars
gen linstab=l.instab
gen lln_population=l.ln_population



/*  This adds an OECD variable, as defined by the World Bank, 2006 */
gen OECD=1 if countryname=="United States of America" | countryname=="Austria" | countryname=="Belgium" | countryname=="Canada" | countryname=="Denmark" | countryname=="France" | countryname=="German Federal Republic" | countryname=="Greece" | countryname=="Iceland" | countryname=="Ireland" | countryname=="Italy/Sardinia" | countryname=="Luxembourg" | countryname=="Netherlands" | countryname=="Norway" | countryname=="Portugal" | countryname=="Spain" | countryname=="Sweden" | countryname=="Switzerland" | countryname=="Turkey/Ottoman Empire" | countryname=="United Kingdom"
replace OECD=1 if countryname=="Japan" | countryname=="Finland" | countryname=="Australia" | countryname=="New Zealand"
/*Not sure about these ones--these are the new members?*/
replace OECD=1 if countryname=="Mexico" | countryname=="Czech Republic"  | countryname=="Hungary" | countryname=="Poland" | countryname=="Korea, Republic of" | countryname=="Slovakia"
replace OECD=0 if OECD!=1


** Make a donor GDP variable 
gen aiddonor = 1 if countryname=="United States of America" | countryname=="Austria" | countryname=="Belgium" | countryname=="Canada" | countryname=="Denmark" | countryname=="France" | countryname=="German Federal Republic" | countryname=="Greece" | countryname=="Iceland" | countryname=="Ireland" | countryname=="Italy/Sardinia" | countryname=="Luxembourg" | countryname=="Netherlands" | countryname=="Norway" | countryname=="Portugal" | countryname=="Spain" | countryname=="Sweden" | countryname=="Switzerland" | countryname=="United Kingdom"
replace aiddonor=1 if countryname=="Japan" | countryname=="Finland" | countryname=="Australia" | countryname=="New Zealand"

egen donorrgdpc = total(rgdpc) if aiddonor==1, by(year)
replace donorrgdpc =. if donorrgdpc ==0
egen donorgdp = max(donorrgdpc), by(year)
gen lndonorgdp = ln(donorgdp)
gen lndonorrgdpc = ln(donorrgdpc)
tsset countrynum year
gen llndonorgdp=l.lndonorgdp



**  Merge in the PRIO data
merge countryname year using "crap\prio.dta", unique sort _merge(_mergeprio)

  ** prio is the onset of conflict, with subsequent consecutive conflict years dropped,
  **  and additional conflicts that start during the first conflict dropped.
replace prio=0 if prio==.
sort countryname year
replace prio=. if prio[_n]==1 & prio[_n-1]==1 & countryname[_n]==countryname[_n-1]
replace prio=. if prio[_n]==1 & prio[_n-1]==. & countryname[_n]==countryname[_n-1]

  ** prio2 has the subsequent consecutive conflict years coded as 0 and new conflicts 
  **  that start during the first conflict coded as 1.
gen prio2 = prio
replace prio2=0 if prio==.
replace prio2=1 if newwar==1

  ** make a var that is just the FIRST onset
egen totalonsets = total(prio) if year>=1980, by(countryname)

gen firstwar=0
replace firstwar = 1 if prio==1 & year>=1980
replace firstwar = firstwar[_n] + firstwar[_n-1] if countryname[_n]==countryname[_n-1]
replace firstwar = . if firstwar>1
replace firstwar = . if firstwar[_n]==1 & firstwar[_n-1]==1
replace firstwar = . if firstwar[_n]==1 & firstwar[_n-1]==.
drop totalonsets

drop _merge
  ** fix the cowcodes

replace cowcode =cowcode[_n-1] if cowcode ==. & countryname[_n]==countryname[_n-1]
** The cowcodes need to be figured out here, because they are missing for some countries and years.

sort cowcode year


**  Merge in the PRIO type 3 data
merge countryname year using "crap\priotype3.dta", unique sort _merge(_mergepriotype3)
replace priotype3=0 if priotype3==.
sort countryname year
replace priotype3=. if priotype3[_n]==1 & priotype3[_n-1]==1 & countryname[_n]==countryname[_n-1]
replace priotype3=. if priotype3[_n]==1 & priotype3[_n-1]==. & countryname[_n]==countryname[_n-1]


**  Merge in the PRIO 1000 death data
merge countryname year using "crap\prio1000.dta", unique sort _merge(_mergeprio1000)
replace prio1000=0 if prio1000==.
sort countryname year
replace prio1000=. if prio1000[_n]==1 & prio1000[_n-1]==1 & countryname[_n]==countryname[_n-1]
replace prio1000=. if prio1000[_n]==1 & prio1000[_n-1]==. & countryname[_n]==countryname[_n-1]

**  Merge in the PRIO 1k death data
merge countryname year using "crap\prio1k.dta", unique sort _merge(_mergeprio1k)
replace prio1k=0 if prio1k==.
sort countryname year
replace prio1k=. if prio1k[_n]==1 & prio1k[_n-1]==1 & countryname[_n]==countryname[_n-1]
replace prio1k=. if prio1k[_n]==1 & prio1k[_n-1]==. & countryname[_n]==countryname[_n-1]



save crap/fulldata, replace

/**/
** Add in 2008 CRS data from the OECD CD

local filenames  data73-79 data80-89 data1990 data1991 data1992 data1993 data1994 data1995 data1996 data1997 data1998 data1999 data2000 data2001 data2002 data2003 data2004_1 data2004_2 data2005_1 data2005_2 data2006_1 data2006_2

foreach FILE of local filenames {
  di "`FILE'"
  insheet using "data/`FILE'.csv", clear
  
  ** Rename the variables
rename v1 commitmentyear 
rename v2 donorcode
rename v3 donorname
rename v4 agencyname
rename v5 transactionno
rename v6 projectnumber
rename v7 nature_of_submission
rename v8 recipientcode
rename v9 recipientname
rename v10 commitmentdate
rename v11 flowcode
rename v12 flowname
rename v13 purposecode
rename v14 purposename
rename v15 shortdescription
rename v16 longdescription 
rename v17 projecttitle
rename v18 channel_of_delivery
rename v19 geography
rename v20 grantelement
rename v21 usd_amount
rename v22 usd_amounttied
rename v23 usd_amountuntied
rename v24 usd_amountpartialtied
rename v25 typerepayment
rename v26 numberrepayment
rename v27 repaydate1
rename v28 repaydate2
rename v29 interest1
rename v30 gender
rename v31 environment
rename v32 PDGG
rename v33 FTC
rename v34 investmentproject
rename v35 usd_IRTC
rename v36 part

  ** keep only the commitment data (disbursements are also line item entries in this data).
  keep if usd_amount!=.

  ** amounts are in 1000's US dollars -- convert to US dollars
  replace usd_amount=usd_amount*1000

*Convert project amounts into 2000 dollars.

*** This is the new conversion using conversion factors from here:
/*
   http://oregonstate.edu/cla/polisci/faculty-research/sahr/cv2000.pdf
   http://oregonstate.edu/cla/polisci/faculty-research/sahr/cv2006.pdf
   http://oregonstate.edu/cla/polisci/faculty-research/sahr/sahr.htm
   Conversion factors in Excel format starting 1665 are available at addresses above.
  (c) 2005 Robert C. Sahr, Political Science Department, Oregon State University, Corvallis, OR  97331-6206
    e-mail:  Robert.Sahr@oregonstate.edu;  WWW:  http://oregonstate.edu/cla/polisci/faculty/sahr-robert
*/

capture rename commitmentyear year
gen usd_amount2k=.
replace usd_amount2k=usd_amount    if year==2000
replace usd_amount2k=usd_amount/0.258 if year==1973
replace usd_amount2k=usd_amount/0.286 if year==1974
replace usd_amount2k=usd_amount/0.312 if year==1975
replace usd_amount2k=usd_amount/0.330 if year==1976
replace usd_amount2k=usd_amount/0.352 if year==1977
replace usd_amount2k=usd_amount/0.379 if year==1978
replace usd_amount2k=usd_amount/0.422 if year==1979
replace usd_amount2k=usd_amount/0.479 if year==1980
replace usd_amount2k=usd_amount/0.528 if year==1981
replace usd_amount2k=usd_amount/0.56 if year==1982
replace usd_amount2k=usd_amount/0.578 if year==1983
replace usd_amount2k=usd_amount/0.603 if year==1984
replace usd_amount2k=usd_amount/0.625 if year==1985
replace usd_amount2k=usd_amount/0.636 if year==1986
replace usd_amount2k=usd_amount/0.66 if year==1987
replace usd_amount2k=usd_amount/0.687 if year==1988
replace usd_amount2k=usd_amount/0.72 if year==1989
replace usd_amount2k=usd_amount/0.759 if year==1990
replace usd_amount2k=usd_amount/0.791 if year==1991
replace usd_amount2k=usd_amount/0.815 if year==1992
replace usd_amount2k=usd_amount/0.839 if year==1993
replace usd_amount2k=usd_amount/0.861 if year==1994
replace usd_amount2k=usd_amount/0.885 if year==1995
replace usd_amount2k=usd_amount/0.911 if year==1996
replace usd_amount2k=usd_amount/0.932 if year==1997
replace usd_amount2k=usd_amount/0.947 if year==1998
replace usd_amount2k=usd_amount/0.967 if year==1999
replace usd_amount2k=usd_amount/1.028 if year==2001
replace usd_amount2k=usd_amount/1.045 if year==2002
replace usd_amount2k=usd_amount/1.069 if year==2003
replace usd_amount2k=usd_amount/1.097 if year==2004
replace usd_amount2k=usd_amount/1.134 if year==2005
replace usd_amount2k=usd_amount/1.171 if year==2006


  ** Sum up total aid for all sectors by recipient
  egen totalA=total(usd_amount2k), by(recipientcode year)
  egen totalaid=max(totalA), by(recipientcode year)
  drop totalA


  ** drop the extraneous variables and the extra observations
  keep  year  recipientcode recipientname totalaid
  duplicates drop

  ** saves the dataset
  save "crap/`FILE'.dta", replace
}



use "crap/data73-79.dta", clear
local datasets2 data80-89 data1990 data1991 data1992 data1993 data1994 data1995 data1996 data1997 data1998 data1999 data2000 data2001 data2002 data2003 data2004_1 data2004_2 data2005_1 data2005_2 data2006_1 data2006_2

foreach dtafile of local datasets2 {
  append using "crap/`dtafile'.dta"
}

foreach NUM of numlist 2004/2006 {
  egen holdervar = sum(totalaid) if year==`NUM', by(recipientname)
  replace totalaid = holdervar if holdervar!=.
  drop holdervar
}

duplicates drop

save "crap/CRS08 73-06.dta", replace

** Erase the extra 
local datasets  data73-79 data80-89 data1990 data1991 data1992 data1993 data1994 data1995 data1996 data1997 data1998 data1999 data2000 data2001 data2002 data2003 data2004_1 data2004_2 data2005_1 data2005_2 data2006_1 data2006_2

foreach file of local datasets {
  erase "crap/`file'.dta"
}



** After the dataset is all together
**  Make all the names the same
replace  recipientname=rtrim( recipientname)
ren recipientname countryname

run "data\Standardize Country Names.master do file.RHR.08.28.07.do"
run "data\Standardize Country Codes.master do file.08.28.07.do"

** Gets rid of blank countries and Yugoslavia which has duplicate problems
duplicates tag countryname year, gen(dup)
drop if dup>0
drop dup

** sorts the observations
sort countryname year
keep if stateinyeart_g==1

save "crap\CRS08 73-06.dta", replace


/**/

use "crap\CRS08 73-06.dta", clear
capture rename totalaid totalaidoecd
save "crap\CRS08 73-06.dta", replace

  ** MERGE IN THE DATA
use crap/fulldata, clear
merge countryname year using "crap\CRS08 73-06.dta", unique sort

save crap/fulldata, replace

** Merging in PLAID 1.9

*********************************************************
** Build the aid data with PLAID 1.9
*********************************************************

/*
** On the RCE
** This section was done on the RCE server at Harvard.  It's commented out because
** it needs a big computer to open the dataset.  The final dataset is included so
** you don't need to find a big computer to build it.  In fact, the original data
** file is so big that I'm not including it in the replication archive.  You can
** find it at aiddata.org, or you can email me at nielsen.rich@gmail.com if you 
** really want to reproduce this part from scratch.

set mem 5000M
set more off

  ** This file "PLAID19.csv" was downloaded from the PLAID website on Feb 15th or so.
cd "/nfs/projects/r/rewards"
insheet using "PLAID19.csv", clear names

  ** rename the 1.9 varnames to match 1.6 varnames -- taken from "renaming variables.do" on the R drive
    
quietly rename recipient rname         
quietly rename commitment_amount_usd_constant usdco         

egen plaidtotal = sum(usdco), by(rname year)
keep rname year plaidtotal
duplicates drop
saveold "plaid for aidshocks.dta", replace

*/
*****************************************************
** END making plaid data on RCE
*****************************************************


** Use FTP to move "plaid for aidshocks.dta"

save crap/fulldata, replace

use "data\plaid for aidshocks.dta", clear

rename rname countryname

  ** there are duplicates in here that aren't easy to deal with
drop if countryname=="Russia" & year==1991
drop if countryname=="Serbia" & year>=1991

run "data\New Standardize Country Names.do"
  
  ** merge it into the controls
merge countryname year using crap/fulldata, unique sort _merge(_merge19)
drop _merge19
replace plaidtotal=0 if plaidtotal==. & year>=1973 & year<=2006
rename plaidtotal totalaid
save crap/fulldata, replace


*********************************************

** DISBURSEMENT data
*********************************************************
** Build the aid data with PLAID 1.9 DISBURSEMENTS
*********************************************************

/*
** On the RCE
** Again, this section was done on a Linux server at Harvard.  See the comment in the section above.

set mem 5000M
set more off

  ** This file "PLAID19_CRS_DISB.csv" was downloaded from the PLAID website on March 15 or so.
cd "/nfs/projects/r/rewards"
insheet using "PLAID19_CRS_DISB.csv", clear names

  ** rename the 1.9 varnames to match 1.6 varnames -- taken from "renaming variables.do" on the R drive
    
quietly rename recipient rname         

egen plaidtotaldisb = sum(disbursement_amount_usd_constant), by(rname year)
keep rname year plaidtotaldisb
duplicates drop
saveold "plaid disb for aidshocks.dta", replace

*/
*****************************************************
** END making plaid DISBURSEMENTS data on RCE
*****************************************************

** Use FTP to move "plaid disb for aidshocks.dta"


use "data\plaid disb for aidshocks.dta", clear

rename rname countryname

run "data\New Standardize Country Names.do"
  
  ** merge it into the controls
merge countryname year using crap/fulldata, unique sort _merge(_merge19disb)
drop _merge19disb
replace plaidtotaldisb=0 if plaidtotaldisb==. & year>=1973 & year<=2006
rename plaidtotaldisb totalaiddisb
save crap/fulldata, replace



***************************************************
** Make military aid data
***************************************************

/*
##############################################
## This code formats the data in R
## set your working directory

for(k in 1:6){
  filename <- paste("GBK",k,".csv",sep="")
dat <- read.csv(filename, header=T)
head(dat)

cnames <- gsub("   ","",gsub("  ","",as.character(dat$Country)))

newdat <- c()
for(i in 2:ncol(dat)){
  y <- rep(gsub("X","",colnames(dat)[i]),nrow(dat))
  aid <- gsub(",","",as.character(dat[,i]))
  aid[aid=="."] <- NA
  newdat <- rbind(newdat,cbind(cnames,y,aid))
}
newdat
obj <- paste("newdat",k, sep="")
assign(obj, newdat)
}
  
newdat <- rbind(newdat1,newdat2,newdat3,newdat4,newdat5,newdat6)
write.csv(newdat,"greenbook.csv",na="")
########################################
*/

** Assuming it is formatted, then run:
insheet using "data\greenbook.csv", clear
rename cnames countryname
rename y year
run "data\New Standardize Country Names.do"
drop if aid==.

  ** http://oregonstate.edu/cla/polisci/faculty-research/sahr/cv2000.pdf
** convert aid to 2000 dollars
gen usmilaid = aid/1.250
drop aid
save "crap\usmilaid.dta", replace

use crap/fulldata, clear


*********************************************
*********************************************


** We do everything for both PLAID data, OECD data, and disbursement data

************************************
** PLAID data manipulation
************************************

capture rename totalaid aid_usd_2000

replace aid_usd_2000 = 0 if aid_usd_2000 == .
drop if _merge == 2
*drop _merge

ren aidpergdp aidpergdpOLD
ren aidgdpchange aidgdpchangeOLD
ren lagaidpergdp  lagaidpergdpOLD
ren lagaidgdpchange lagaidgdpchangeOLD


/* clean up some things to check out how well it fits */
replace aid_usd_2000 = 0 if aid_usd_2000 == .
*gen aidpergdp = aid_usd_2000 / gdp2000
gen aidpergdp = aid_usd_2000 / rgdp

  ** note that Gelditch GDP data is also in 2000 dollars according to 
  ** C:\Documents and Settings\Rich\Desktop\Desktop\Data\Gleditsch Improved Trade and GDP data\UPDATE through 2004\expgdpv5.0\expgdp_v5.0.readme

  *****************************************************
  ** To do the analysis with straight aid totals run:
  *drop aidpergdp
  *gen aidpergdp = aid_usd_2000
  ** Noting that now "aidpergdp" is really just aid in 2000 dollars.
  *****************************************************



gen aidgdpchange = aidpergdp[_n] - aidpergdp[_n-1] 
  ** Probably should have run this line instead, but it's too late
*gen aidgdpchange = aidpergdp[_n] - aidpergdp[_n-1] if (countryname[_n] == countryname[_n-1])

** To see the data, do: edit rgdp aid_usd_2000 aidpergdp aidgdpchange countryname year

drop countrynum
egen countrynum=group(countryname)
tsset countrynum year
gen lagaidpergdp = L.aidpergdp
gen lagaidgdpchange = L.aidgdpchange




************************************
** OECD data manipulation
************************************

capture rename totalaidoecd aid_usd_2000oecd

replace aid_usd_2000oecd = 0 if aid_usd_2000oecd == .
drop if _merge == 2
*drop _merge


/* clean up some things to check out how well it fits */
replace aid_usd_2000oecd = 0 if aid_usd_2000oecd == .
*gen aidpergdp = aid_usd_2000 / gdp2000
gen aidpergdpoecd = aid_usd_2000oecd / rgdp

  ** note that Gelditch GDP data is also in 2000 dollars according to 
  ** C:\Documents and Settings\Rich\Desktop\Desktop\Data\Gleditsch Improved Trade and GDP data\UPDATE through 2004\expgdpv5.0\expgdp_v5.0.readme

  *****************************************************
  ** To do the analysis with straight aid totals run:
  *drop aidpergdpoecd
  *gen aidpergdpoecd = aid_usd_2000oecd
  ** Noting that now "aidpergdpoecd" is really just aid in 2000 dollars.
  *****************************************************



gen aidgdpchangeoecd = aidpergdpoecd[_n] - aidpergdpoecd[_n-1] 
  ** Probably should have run this line instead, but it's too late
*gen aidgdpchangeoecd = aidpergdpoecd[_n] - aidpergdpoecd[_n-1] if (countryname[_n] == countryname[_n-1])

** To see the data, do: edit rgdp aid_usd_2000 aidpergdp aidgdpchange countryname year

drop countrynum
egen countrynum=group(countryname)
tsset countrynum year
gen lagaidpergdpoecd = L.aidpergdpoecd
gen lagaidgdpchangeoecd = L.aidgdpchangeoecd




************************************
** PLAID DISBURSEMENT data manipulation
************************************

capture rename totalaiddisb aid_usd_2000disb

replace aid_usd_2000disb = 0 if aid_usd_2000disb == .
drop if _merge == 2
drop _merge


/* clean up some things to check out how well it fits */
replace aid_usd_2000disb = 0 if aid_usd_2000disb == .
*gen aidpergdp = aid_usd_2000 / gdp2000
gen aidpergdpdisb = aid_usd_2000disb / rgdp

  ** note that Gelditch GDP data is also in 2000 dollars according to 
  ** C:\Documents and Settings\Rich\Desktop\Desktop\Data\Gleditsch Improved Trade and GDP data\UPDATE through 2004\expgdpv5.0\expgdp_v5.0.readme

  *****************************************************
  ** To do the analysis with straight aid totals run:
  *drop aidpergdpdisb
  *gen aidpergdpdisb = aid_usd_2000disb
  ** Noting that now "aidpergdpdisb" is really just aid in 2000 dollars.
  *****************************************************



gen aidgdpchangedisb = aidpergdpdisb[_n] - aidpergdpdisb[_n-1] 
  ** Probably should have run this line instead, but it's too late
*gen aidgdpchangedisb = aidpergdpdisb[_n] - aidpergdpdisb[_n-1] if (countryname[_n] == countryname[_n-1])

** To see the data, do: edit rgdp aid_usd_2000 aidpergdp aidgdpchange countryname year

drop countrynum
egen countrynum=group(countryname)
tsset countrynum year
gen lagaidpergdpdisb = L.aidpergdpdisb
gen lagaidgdpchangedisb = L.aidgdpchangedisb



************************************
** PLAID + US Military Aid
************************************

merge countryname year using "crap\usmilaid.dta", _merge(_mergemilaid) sort

replace usmilaid = 0 if usmilaid==.
gen aid_usd_2000mil = aid_usd_2000 + usmilaid

/* clean up some things to check out how well it fits */
replace aid_usd_2000mil = 0 if aid_usd_2000mil == .
gen aidpergdpmil = aid_usd_2000mil / rgdp

  ** note that Gelditch GDP data is also in 2000 dollars according to 
  ** C:\Documents and Settings\Rich\Desktop\Desktop\Data\Gleditsch Improved Trade and GDP data\UPDATE through 2004\expgdpv5.0\expgdp_v5.0.readme

  *****************************************************
  ** To do the analysis with straight aid totals run:
  *drop aidpergdpmil
  *gen aidpergdpmil = aid_usd_2000mil
  ** Noting that now "aidpergdp" is really just aid in 2000 dollars.
  *****************************************************



gen aidgdpchangemil = aidpergdpmil[_n] - aidpergdpmil[_n-1] 
  ** probably should have run this line, but it doesn't make too much of a difference
*gen aidgdpchangemil = aidpergdpmil[_n] - aidpergdpmil[_n-1] if (countryname[_n] == countryname[_n-1])


drop countrynum
egen countrynum=group(countryname)
tsset countrynum year
gen lagaidpergdpmil = L.aidpergdpmil
gen lagaidgdpchangemil = L.aidgdpchangemil




/************************************************************************************/




/*	This adds the Cold War var.  */
gen ColdWar=1 if year>=1992
replace ColdWar=0 if year<=1991




**********************************************
/* NEW: merge in UNSC rotating membership */

merge countryname year using "data\SC rotating members.dta", unique sort
  ** Note that I built this data file by hand.  I recorded the process in
  ** "make UN SC member data.smcl".
drop if _merge==2
drop _merge
rename var4 scmember
replace scmember=0 if scmember!=1
  ** make a "year after" var
tsset countrynum year

  ** make an ex-member var
capture drop exscmember
gen exscmember = 1 if scmember==0 & l.scmember==1
replace exscmember = 1 if l.scmember==0 & l2.scmember==1
*replace exscmember = 1 if l2.scmember==0 & l3.scmember==1
replace exscmember = 0 if exscmember!=1

  ** 
capture drop scmemberplus
gen scmemberplus = scmember
replace scmemberplus = 1 if scmember==0 & l.scmember==1
     
**********************************************



drop peaceyrs _spline*

drop if year==9999
tsset countrynum year

** make lags of the key variables
foreach var of varlist physint PTSave_filled assassinbanks riotsbanks strikesbanks demonstrationsbanks infantmort nciv partautocracy partdemocracy factionaldemoc fulldemocracy oil {
  gen l`var' = l.`var'
}


save crap/fulldata, replace


************************************
** PLAID data
************************************
** makes smoothed aid/gdp vars
tssmooth ma smaidch=aidgdpchange, window(2 0 0)
tssmooth ma sm2aidch=aidgdpchange, window(3 0 0)
tssmooth ma sm3aidch=aidgdpchange, window(4 0 0)
tssmooth ma sm4aidch=aidgdpchange, window(5 0 0)

tssmooth ma smaidde= aidpergdp, window(2 0 0)
tssmooth ma sm4aidde= aidpergdp, window(5 0 0)

gen negaid=smaidch if smaidch<0
replace negaid=0 if negaid==.

gen negaid2=sm2aidch if sm2aidch<0
replace negaid2 = 0 if negaid2==.

gen posaid=smaidch if smaidch>0
replace posaid=0 if posaid==.

*******************************************
** To drop the crappy aid data years prior to 1975
drop if year<1975
*******************************************


** A dummy indicating whether an observation is in the sample
*gen inmysample = 1 if warst6!=. &  lln_rgdpc!=. &   ln_population!=. &   oil!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 
*gen inmysample = 1 if lln_rgdpc!=. &   ln_population!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 

*gen inmysample = 1 if prio!=. & lln_rgdpc!=. &   ln_population!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 

** run the model here to specify what the obs. are
relogit prio negaid lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
gen inmysample = e(sample)



_pctile smaidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11 = 1 if smaidch<=`shock'
replace aidshock11 = 0 if aidshock11==.

_pctile smaidch if inmysample==1, p(25)
local shock=r(r1)
gen aidshock22 = 1 if smaidch<=`shock'
replace aidshock22 = 0 if aidshock22==.

gen verynegaid = smaidch if smaidch<=`shock'
replace verynegaid = 0 if verynegaid==.


_pctile lagaidgdpchange if inmysample==1, p(15)
local shock=r(r1)
gen aidshock33 = 1 if lagaidgdpchange <=`shock'
replace aidshock33 = 0 if aidshock33==.
tssmooth ma smaidsh = aidshock33, window(4 1 0)

_pctile smaidch if inmysample==1, p(10)
local shock=r(r1)
gen aidshock44 = 1 if smaidch<=`shock'
replace aidshock44 = 0 if aidshock44==.

_pctile smaidch if inmysample==1, p(20)
local shock=r(r1)
gen aidshock55 = 1 if smaidch<=`shock'
replace aidshock55 = 0 if aidshock55==.

**************************************************
** Adding positive aid shocks -- corresponding to the negative shocks
**************************************************
_pctile smaidch if inmysample==1, p(85)
local shock=r(r1)
gen aidshock11pos = 1 if smaidch>=`shock'
replace aidshock11pos = 0 if aidshock11pos==.

_pctile smaidch if inmysample==1, p(75)
local shock=r(r1)
gen aidshock22pos = 1 if smaidch>=`shock'
replace aidshock22pos = 0 if aidshock22pos==.


_pctile lagaidgdpchange if inmysample==1, p(85)
local shock=r(r1)
gen aidshock33pos = 1 if lagaidgdpchange <=`shock'
replace aidshock33pos = 0 if aidshock33pos==.
tssmooth ma smaidshpos = aidshock33pos, window(4 1 0)


_pctile smaidch if inmysample==1, p(90)
local shock=r(r1)
gen aidshock44pos = 1 if smaidch>=`shock'
replace aidshock44pos = 0 if aidshock44pos==.

_pctile smaidch if inmysample==1, p(80)
local shock=r(r1)
gen aidshock55pos = 1 if smaidch>=`shock'
replace aidshock55pos = 0 if aidshock55pos==.


***************************************
** Making shocks at every possible value from 5 to 50
foreach NUM of numlist 5/50 {
_pctile smaidch if inmysample==1, p(`NUM')
local shock=r(r1)
gen aidshockvar`NUM' = 1 if smaidch<=`shock'
replace aidshockvar`NUM' = 0 if aidshockvar`NUM'==.
}
***************************************

***************************************
** Making POSITIVE shocks at every possible value from 50 to 95
foreach NUM of numlist 50/95 {
_pctile smaidch if inmysample==1, p(`NUM')
local shock=r(r1)
gen aidshockposvar`NUM' = 1 if smaidch>=`shock'
replace aidshockposvar`NUM' = 0 if aidshockposvar`NUM'==.
}
***************************************

** Shocks with longer moving averages
  
  ** 3 year moving average
_pctile sm2aidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11s2 = 1 if sm2aidch<=`shock'
replace aidshock11s2 = 0 if aidshock11s2==.

  ** 4 year moving average
_pctile sm3aidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11s3 = 1 if sm3aidch<=`shock'
replace aidshock11s3 = 0 if aidshock11s3==.

  ** 5 year moving average
_pctile sm4aidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11s4 = 1 if sm4aidch<=`shock'
replace aidshock11s4 = 0 if aidshock11s4==.



************************************
** PLAID + US MILITARY AID data
************************************
** makes smoothed aid/gdp vars
tssmooth ma smaidchmil=aidgdpchangemil, window(2 0 0)
tssmooth ma sm2aidchmil=aidgdpchangemil, window(3 0 0)
tssmooth ma sm3aidchmil=aidgdpchangemil, window(4 0 0)
tssmooth ma sm4aidchmil=aidgdpchangemil, window(5 0 0)

tssmooth ma smaiddemil= aidpergdpmil, window(2 0 0)
tssmooth ma sm4aiddemil= aidpergdpmil, window(5 0 0)

gen negaidmil=smaidchmil if smaidchmil<0
replace negaidmil=0 if negaidmil==.

gen negaid2mil=sm2aidchmil if sm2aidchmil<0
replace negaid2mil = 0 if negaid2mil==.

gen posaidmil=smaidchmil if smaidchmil>0
replace posaidmil=0 if posaidmil==.

*******************************************
** To drop the crappy aid data years prior to 1975
drop if year<1975
*******************************************

** run the model here to specify what the obs. are
relogit prio negaidmil lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
gen inmysamplemil = e(sample)


  ** NOTE:  for aidshock11mil1 I used the same cutoff as aidshock11 (smaidch instead of smaidchmil)
_pctile smaidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11mil1 = 1 if smaidchmil<=`shock'
replace aidshock11mil1 = 0 if aidshock11mil1==.

_pctile smaidchmil if inmysamplemil==1, p(15)
local shock=r(r1)
gen aidshock11mil = 1 if smaidchmil<=`shock'
replace aidshock11mil = 0 if aidshock11mil==.
  ** because I changed my mind elsewhere...
gen aidshock11mil2 = aidshock11mil


_pctile smaidchmil if inmysamplemil==1, p(25)
local shock=r(r1)
gen aidshock22mil = 1 if smaidchmil<=`shock'
replace aidshock22mil = 0 if aidshock22mil==.

gen verynegaidmil = smaidchmil if smaidch<=`shock'
replace verynegaidmil = 0 if verynegaidmil==.


_pctile lagaidgdpchangemil if inmysamplemil==1, p(15)
local shock=r(r1)
gen aidshock33mil = 1 if lagaidgdpchangemil <=`shock'
replace aidshock33mil = 0 if aidshock33mil==.
tssmooth ma smaidshmil = aidshock33mil, window(4 1 0)

_pctile smaidchmil if inmysamplemil==1, p(10)
local shock=r(r1)
gen aidshock44mil = 1 if smaidchmil<=`shock'
replace aidshock44mil = 0 if aidshock44mil==.

_pctile smaidchmil if inmysamplemil==1, p(20)
local shock=r(r1)
gen aidshock55mil = 1 if smaidchmil<=`shock'
replace aidshock55mil = 0 if aidshock55mil==.

**************************************************
** Adding positive aid shocks -- corresponding to the negative shocks
**************************************************
_pctile smaidchmil if inmysamplemil==1, p(85)
local shock=r(r1)
gen aidshock11posmil = 1 if smaidchmil>=`shock'
replace aidshock11posmil = 0 if aidshock11posmil==.

_pctile smaidchmil if inmysamplemil==1, p(75)
local shock=r(r1)
gen aidshock22posmil = 1 if smaidchmil>=`shock'
replace aidshock22posmil = 0 if aidshock22posmil==.

_pctile smaidchmil if inmysamplemil==1, p(90)
local shock=r(r1)
gen aidshock44posmil = 1 if smaidchmil>=`shock'
replace aidshock44posmil = 0 if aidshock44posmil==.

_pctile smaidchmil if inmysamplemil==1, p(80)
local shock=r(r1)
gen aidshock55posmil = 1 if smaidchmil>=`shock'
replace aidshock55posmil = 0 if aidshock55posmil==.



***************************************
** Making shocks at every possible value from 5 to 50
foreach NUM of numlist 5/50 {
_pctile smaidchmil if inmysamplemil==1, p(`NUM')
local shock=r(r1)
gen aidshockvarmil`NUM' = 1 if smaidchmil<=`shock'
replace aidshockvarmil`NUM' = 0 if aidshockvarmil`NUM'==.
}
***************************************

***************************************
** Making POSITIVE shocks at every possible value from 50 to 95
foreach NUM of numlist 50/95 {
_pctile smaidchmil if inmysamplemil==1, p(`NUM')
local shock=r(r1)
gen aidshockposvarmil`NUM' = 1 if smaidchmil>=`shock'
replace aidshockposvarmil`NUM' = 0 if aidshockposvarmil`NUM'==.
}
***************************************



************************************
** DISBURSEMENT data
************************************
** makes smoothed aid/gdp vars
tssmooth ma smaidchdisb=aidgdpchangedisb, window(2 0 0)
tssmooth ma sm2aidchdisb=aidgdpchangedisb, window(3 0 0)
tssmooth ma sm3aidchdisb=aidgdpchangedisb, window(4 0 0)
tssmooth ma sm4aidchdisb=aidgdpchangedisb, window(5 0 0)

tssmooth ma smaiddedisb= aidpergdpdisb, window(2 0 0)
tssmooth ma sm4aiddedisb= aidpergdpdisb, window(5 0 0)

gen negaiddisb=smaidchdisb if smaidchdisb<0
replace negaiddisb=0 if negaiddisb==.

gen negaid2disb=sm2aidchdisb if sm2aidchdisb<0
replace negaid2disb = 0 if negaid2disb==.

gen posaiddisb=smaidchdisb if smaidchdisb>0
replace posaiddisb=0 if posaiddisb==.

*******************************************
** To drop the crappy aid data years prior to 1975
drop if year<1975
*******************************************


** A dummy indicating whether an observation is in the sample
*gen inmysample = 1 if warst6!=. &  lln_rgdpc!=. &   ln_population!=. &   oil!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 
*gen inmysample = 1 if lln_rgdpc!=. &   ln_population!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 

*gen inmysample = 1 if prio!=. & lln_rgdpc!=. &   ln_population!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 

** run the model here to specify what the obs. are
relogit prio negaiddisb lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
gen inmysampledisb = e(sample)



_pctile smaidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11disb1 = 1 if smaidchdisb<=`shock'
replace aidshock11disb1 = 0 if aidshock11disb1==.

_pctile smaidchdisb if inmysampledisb==1, p(15)
local shock=r(r1)
gen aidshock11disb2 = 1 if smaidchdisb<=`shock'
replace aidshock11disb2 = 0 if aidshock11disb2==.


_pctile smaidchdisb if inmysampledisb==1, p(25)
local shock=r(r1)
gen aidshock22disb = 1 if smaidchdisb<=`shock'
replace aidshock22disb = 0 if aidshock22disb==.

gen verynegaiddisb = smaidchdisb if smaidchdisb<=`shock'
replace verynegaiddisb = 0 if verynegaiddisb==.


_pctile lagaidgdpchangedisb if inmysampledisb==1, p(15)
local shock=r(r1)
gen aidshock33disb = 1 if lagaidgdpchangedisb <=`shock'
replace aidshock33disb = 0 if aidshock33disb==.
tssmooth ma smaidshdisb = aidshock33disb, window(4 1 0)

_pctile smaidchdisb if inmysampledisb==1, p(10)
local shock=r(r1)
gen aidshock44disb = 1 if smaidchdisb<=`shock'
replace aidshock44disb = 0 if aidshock44disb==.

_pctile smaidchdisb if inmysampledisb==1, p(20)
local shock=r(r1)
gen aidshock55disb = 1 if smaidchdisb<=`shock'
replace aidshock55disb = 0 if aidshock55disb==.

**************************************************
** Adding positive aid shocks -- corresponding to the negative shocks
**************************************************
_pctile smaidchdisb if inmysampledisb==1, p(85)
local shock=r(r1)
gen aidshock11posdisb = 1 if smaidchdisb>=`shock'
replace aidshock11posdisb = 0 if aidshock11posdisb==.

_pctile smaidchdisb if inmysampledisb==1, p(75)
local shock=r(r1)
gen aidshock22posdisb = 1 if smaidchdisb>=`shock'
replace aidshock22posdisb = 0 if aidshock22posdisb==.

_pctile smaidchdisb if inmysampledisb==1, p(90)
local shock=r(r1)
gen aidshock44posdisb = 1 if smaidchdisb>=`shock'
replace aidshock44posdisb = 0 if aidshock44posdisb==.

_pctile smaidchdisb if inmysampledisb==1, p(80)
local shock=r(r1)
gen aidshock55posdisb = 1 if smaidchdisb>=`shock'
replace aidshock55posdisb = 0 if aidshock55posdisb==.



***************************************
** Making shocks at every possible value from 5 to 50
foreach NUM of numlist 5/50 {
_pctile smaidchdisb if inmysampledisb==1, p(`NUM')
local shock=r(r1)
gen aidshockvardisb`NUM' = 1 if smaidchdisb<=`shock'
replace aidshockvardisb`NUM' = 0 if aidshockvardisb`NUM'==.
}
***************************************

***************************************
** Making POSITIVE shocks at every possible value from 50 to 95
foreach NUM of numlist 50/95 {
_pctile smaidchdisb if inmysampledisb==1, p(`NUM')
local shock=r(r1)
gen aidshockposvardisb`NUM' = 1 if smaidchdisb>=`shock'
replace aidshockposvardisb`NUM' = 0 if aidshockposvardisb`NUM'==.
}
***************************************



************************************
** OECD data
************************************
** makes smoothed aid/gdp vars
tssmooth ma smaidchoecd=aidgdpchangeoecd, window(2 0 0)
tssmooth ma sm2aidchoecd=aidgdpchangeoecd, window(3 0 0)
tssmooth ma sm3aidchoecd=aidgdpchangeoecd, window(4 0 0)
tssmooth ma sm4aidchoecd=aidgdpchangeoecd, window(5 0 0)

tssmooth ma smaiddeoecd= aidpergdpoecd, window(2 0 0)
tssmooth ma sm4aiddeoecd= aidpergdpoecd, window(5 0 0)

gen negaidoecd=smaidchoecd if smaidchoecd<0
replace negaidoecd=0 if negaidoecd==.

gen negaid2oecd=sm2aidchoecd if sm2aidchoecd<0
replace negaid2oecd = 0 if negaid2oecd==.

gen posaidoecd=smaidchoecd if smaidchoecd>0
replace posaidoecd=0 if posaidoecd==.

*******************************************
** To drop the crappy aid data years prior to 1975
drop if year<1975
*******************************************


** A dummy indicating whether an observation is in the sample
*gen inmysample = 1 if warst6!=. &  lln_rgdpc!=. &   ln_population!=. &   oil!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 
*gen inmysample = 1 if lln_rgdpc!=. &   ln_population!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 

*gen inmysample = 1 if prio!=. & lln_rgdpc!=. &   ln_population!=. &  nwstate!=. &  instab!=. &  lagaidpergdp!=. &  smaidch!=. &  ethfrac!=. &  relfrac!=. &  ncontig!=. &  logmtn!=. &  lpolity2!=. 

** run the model here to specify what the obs. are
relogit prio negaidoecd lPTSave_filled lassassinbanks lriotsbanks lstrikesbanks ldemonstrationsbanks linfantmort lnciv lpartautocracy lpartdemocracy lfactionaldemoc lfulldemocracy lln_rgdpc  lln_population  loil linstab ethfrac relfrac ncontig logmtn ColdWar , cl(countrynum)
gen inmysampleoecd = e(sample)


  ** NOTE:  for aidshock11oecd1 I used the same cutoff as aidshock11 (smaidch instead of smaidchoecd)
_pctile smaidch if inmysample==1, p(15)
local shock=r(r1)
gen aidshock11oecd1 = 1 if smaidchoecd<=`shock'
replace aidshock11oecd1 = 0 if aidshock11oecd1==.

_pctile smaidchoecd if inmysampleoecd==1, p(15)
local shock=r(r1)
gen aidshock11oecd = 1 if smaidchoecd<=`shock'
replace aidshock11oecd = 0 if aidshock11oecd==.
  ** because I changed my mind elsewhere...
gen aidshock11oecd2 = aidshock11oecd


_pctile smaidchoecd if inmysampleoecd==1, p(25)
local shock=r(r1)
gen aidshock22oecd = 1 if smaidchoecd<=`shock'
replace aidshock22oecd = 0 if aidshock22oecd==.

gen verynegaidoecd = smaidchoecd if smaidch<=`shock'
replace verynegaidoecd = 0 if verynegaidoecd==.


_pctile lagaidgdpchangeoecd if inmysampleoecd==1, p(15)
local shock=r(r1)
gen aidshock33oecd = 1 if lagaidgdpchangeoecd <=`shock'
replace aidshock33oecd = 0 if aidshock33oecd==.
tssmooth ma smaidshoecd = aidshock33oecd, window(4 1 0)

_pctile smaidchoecd if inmysampleoecd==1, p(10)
local shock=r(r1)
gen aidshock44oecd = 1 if smaidchoecd<=`shock'
replace aidshock44oecd = 0 if aidshock44oecd==.

_pctile smaidchoecd if inmysampleoecd==1, p(20)
local shock=r(r1)
gen aidshock55oecd = 1 if smaidchoecd<=`shock'
replace aidshock55oecd = 0 if aidshock55oecd==.

**************************************************
** Adding positive aid shocks -- corresponding to the negative shocks
**************************************************
_pctile smaidchoecd if inmysampleoecd==1, p(85)
local shock=r(r1)
gen aidshock11posoecd = 1 if smaidchoecd>=`shock'
replace aidshock11posoecd = 0 if aidshock11posoecd==.

_pctile smaidchoecd if inmysampleoecd==1, p(75)
local shock=r(r1)
gen aidshock22posoecd = 1 if smaidchoecd>=`shock'
replace aidshock22posoecd = 0 if aidshock22posoecd==.

_pctile smaidchoecd if inmysampleoecd==1, p(90)
local shock=r(r1)
gen aidshock44posoecd = 1 if smaidchoecd>=`shock'
replace aidshock44posoecd = 0 if aidshock44posoecd==.

_pctile smaidchoecd if inmysampleoecd==1, p(80)
local shock=r(r1)
gen aidshock55posoecd = 1 if smaidchoecd>=`shock'
replace aidshock55posoecd = 0 if aidshock55posoecd==.



***************************************
** Making shocks at every possible value from 5 to 50
foreach NUM of numlist 5/50 {
_pctile smaidchoecd if inmysampleoecd==1, p(`NUM')
local shock=r(r1)
gen aidshockvaroecd`NUM' = 1 if smaidchoecd<=`shock'
replace aidshockvaroecd`NUM' = 0 if aidshockvaroecd`NUM'==.
}
***************************************

***************************************
** Making POSITIVE shocks at every possible value from 50 to 95
foreach NUM of numlist 50/95 {
_pctile smaidchoecd if inmysampleoecd==1, p(`NUM')
local shock=r(r1)
gen aidshockposvaroecd`NUM' = 1 if smaidchoecd>=`shock'
replace aidshockposvaroecd`NUM' = 0 if aidshockposvaroecd`NUM'==.
}
***************************************




*drop if inmysample!=1

** I make the splines with only observations in the sample
*btscs warst6 year cowcode, g(peaceyrs) nspline(3)
btscs prio year cowcode if inmysample==1, g(peaceyrs) nspline(3)

save "crap/fulldata.dta", replace



** ADD in the Affinity of Nations data from Gartzke
  ** NOTE: Doesn't have scores after 2002 and Doesn't have scores for Switzerland for some reaso
use "data/affinity_03102006.dta", clear
rename statea ccode1
rename stateb ccode2
gen country=""
gen partner=""
run "data/COW convert.do"
replace country="United States" if country=="United States of America"
replace country="Germany" if country=="German Federal Republic"
rename country donorname
rename partner countryname
keep donorname countryname year s2un s3un s2uni s3uni
drop if donorname=="" | countryname==""
gen OECD=1 if donorname=="United States" | donorname=="Austria" | donorname=="Belgium" | donorname=="Canada" | donorname=="Denmark" | donorname=="France" | donorname=="Germany" | donorname=="Greece" | donorname=="Iceland" | donorname=="Ireland" | donorname=="Italy" | donorname=="Luxembourg" | donorname=="Netherlands" | donorname=="Norway" | donorname=="Portugal" | donorname=="Spain" | donorname=="Sweden" | donorname=="Switzerland" | donorname=="Turkey/Ottoman Empire" | donorname=="United Kingdom"
replace OECD=1 if donorname=="Japan" | donorname=="Finland" | donorname=="Australia" | donorname=="New Zealand"
keep if OECD==1
drop OECD
  ** doing this just for the US first -- if you want OECD, then don't run this
  ** and figure something else out
keep if donorname=="United States" 
run "data/Standardize Country Names.master do file.RHR.08.28.07.do""
save "crap/affinity.dta", replace
  ** merge in the data
use "crap/fulldata.dta", clear
merge countryname year using "crap/affinity.dta", unique sort _merge(_merge_affinity)
move _merge_affinity population
drop if _merge_affinity ==2
drop _merge_affinity 


** save the data
compress
save cwdata, replace



** drop variables that we don't actually end up using

** This section is not essential, but it makes the data
** much smaller.  But if some variable seems to be missing,
** rebuild the data without running the R code below.

/*
## R code

## set the working directory to the location of the data

library(foreign)
dat <- read.dta("cwdata.dta")

varnames <- names(dat)

script1 <- readLines("CW analysis-ajps-part2.do")
script2 <- readLines("CW analysis-ajps-part3.do")
script1[1:10]
script2[1:10]

## Combine the scripts
allscripts <- c(script1,script2)
## make a holder
inscript <- rep(NA,length(varnames))

## Loop through each variable name and see if it appears in the script
for(i in 1:length(varnames)){
  inscript[i] <- as.numeric(length(grep(varnames[i],allscripts))!=0)
    ## this is for some variables that are in "foreach" loops
  if(as.numeric(length(grep("aidshock",varnames[i]))>0)){
    inscript[i] <- 1
  }
    ## This is for some variables that appear only in R code (with "." rather
    ## than "_"
  if(as.numeric(length(grep("aid_usd_2000",varnames[i]))>0)){
    inscript[i] <- 1
  }
}
  ## how many of the variables appear in the script (the "1"s)
table(inscript)
  ## which variables are we keeping?
varnames[inscript==1]
  ## make the new dataset
newdat <- dat[,varnames[inscript==1]]
dim(newdat)
  ## write out the new data
write.dta(newdat,"cwdata.dta")

*/

use cwdata, clear
compress
tsset countrynum year
save cwdata, replace


** It's now safe to delete the "crap" folder.


***********************************************************
** END: Making the data
***********************************************************



