
** Standardizes Country Names to match
** Gleditsch's version 5.0 country list
** Last modified: April, 2009
** by: Rich Nielsen
**	 Harvard University
**	 rnielsen@fas.harvard.edu */

  ** The variable name for the country names should be "countryname"
  ** makes a back-up of the old country names to show what has changed
capture gen countryname_old = countryname
capture replace countryname_old = countryname

  ** In order to generate the variable which specifies
  **  a 1 if the state is in gleditsch's state system in
  **  time t, run these commands:

capture gen stateinyear=.
capture gen microstateinyear=.

  ** If this line is activated, it takes all of the names and
  **  capitalizes them to be proper nouns
*replace countryname = proper(countryname)





*A

replace countryname = "Afghanistan" if countryname == "AFGHANISTAN"
capture replace stateinyear=1 if countryname=="Afghanistan" & year >=1816 & year<=1888
capture replace stateinyear=1 if countryname=="Afghanistan" & year >=1919

replace countryname = "Albania" if countryname == "albania" ///
  | countryname == "ALBANIA"
capture replace stateinyear=1 if countryname=="Albania" & year >=1913

replace countryname = "Algeria" if countryname == "ALGERIA" ///
  | countryname == "algeria"
capture replace stateinyear=1 if countryname=="Algeria" & year >=1816 & year <=1830
capture replace stateinyear=1 if countryname=="Algeria" & year >=1962

  ** American Samoa isn't one of Gleditsch's states or microstates **
replace countryname = "American Samoa" if countryname == "AMERICAN SAMOA"

replace countryname = "Andorra" if countryname == "ANDORRA"
capture replace microstateinyear=1 if countryname=="Andorra" & year >=1816

replace countryname = "Angola" if countryname == "ANGOLA"
capture replace stateinyear=1 if countryname=="Angola" & year >=1975

  **Not in Gleditsch**
replace countryname = "Anguilla" if countryname == "ANGUILLA"

replace countryname = "Antigua & Barbuda" if countryname == "Antigua And Barbuda" ///
  | countryname == "Antigua" ///
  | countryname == "Antiguaandbarbuda" ///
  | countryname == "Antigua and Barbuda" ///
  | countryname == "ANTIGUA AND BARBUDA" ///
  | countryname == "ANTIGUA" ///
  | countryname == "ANTAlgeriaIGUA AND BARBUDA"
capture replace microstateinyear=1 if countryname=="Antigua & Barbuda" & year >=1981

  **Gleditsch doesn't include the Antilles**
replace countryname = "Antilles" if countryname == "Netherlandsantillesandaruba" ///
  | countryname == "Netherlands Antilles" ///
  | countryname == "NETH ANTILS" ///
  | countryname == "NETHERLANDS ANTILLES" 

replace countryname = "Argentina" if countryname == "ARGENTINA" ///
  | countryname == "argentina"
capture replace stateinyear=1 if countryname=="Argentina" & year >=1816

replace countryname = "Armenia" if countryname == "ARMENIA" ///
  | countryname == "armenia"
capture replace stateinyear=1 if countryname=="Armenia" & year >=1991

  **Gleditsch does not include Aruba**
replace countryname = "Aruba" if countryname == "ARUBA"

replace countryname = "Australia" if countryname == "AUSTRALIA"
capture replace stateinyear=1 if countryname=="Australia" & year >=1901

replace countryname = "Austria" if countryname == "AUSTRIA"
capture replace stateinyear=1 if countryname=="Austria" & year >=1918

  ** Austria-Hungary
capture replace stateinyear=1 if countryname=="Austria-Hungary" & year >=1816 & year <=1918

replace countryname = "Azerbaijan" if countryname == "AZERBAIJAN"
capture replace stateinyear=1 if countryname=="Azerbaijan" & year >=1991

*B

*Baden
capture replace stateinyear=1 if countryname=="Baden" & year >=1816 & year <=1871

replace countryname = "Bahamas" if countryname == "Bahamas, The" ///
  | countryname == "The Bahamas" ///
  | countryname == "BAHAMAS" ///
  | countryname == "Bahamas, the" 
capture replace stateinyear=1 if countryname=="Bahamas" & year >=1973

replace countryname = "Bahrain" if countryname == "BAHRAIN"
capture replace stateinyear=1 if countryname=="Bahrain" & year >=1816

replace countryname = "Bangladesh" if countryname == "Bgd" ///
  | countryname == "BANGLADESH" ///
  | countryname == "bangladesh" ///
  | countryname == "BGD"
capture replace stateinyear=1 if countryname=="Bangladesh" & year >=1971

replace countryname = "Barbados" if countryname == "BARBADOS"
capture replace stateinyear=1 if countryname=="Barbados" & year >=1966

replace countryname = "Belarus (Byelorussia)" if countryname == "Belarus" ///
  | countryname == "Byelarus" ///
  | countryname == "BELARUS"
capture replace stateinyear=1 if countryname=="Belarus (Byelorussia)" & year >=1991

replace countryname = "Belgium" if countryname == "BELGIUM"
capture replace stateinyear=1 if countryname=="Belgium" & year >=1830

replace countryname = "Belize" if countryname == "BELIZE" ///
  | countryname == "belize"
capture replace stateinyear=1 if countryname=="Belize" & year >=1981

replace countryname = "Benin" if countryname == "BENIN" ///
  | countryname == "benin" ///
  | countryname == "Benin (Dahomey)"
capture replace stateinyear=1 if countryname=="Benin" & year >=1960

replace countryname = "Bermuda" if countryname == "BERMUDA"
****Gleditsch doesn't include Bermuda****

replace countryname = "Bhutan" if countryname == "Btn" ///
  | countryname == "BHUTAN" ///
  | countryname == "BTN"
capture replace stateinyear=1 if countryname=="Bhutan" & year >=1949

replace countryname = "Bolivia" if countryname == "BOLIVIA" ///
  | countryname == "bolivia"
capture replace stateinyear=1 if countryname=="Bolivia" & year >=1825

replace countryname = "Bosnia-Herzegovina" if countryname == "Bosniaandherzegovina" ///
  | countryname == "Bosnia And Herzegovina" ///
  | countryname == "Bosnia and Herzegovina" ///
  | countryname == "BOSNIA AND HERZEGOVINA" ///
  | countryname == "Bosnia & Herzegovina" ///
  | countryname == "BOSNIA AND HERZEGOVINA" ///
  | countryname == "Bosnia Herzegovenia" ///
  | countryname == "Bosnia Herzegovina" ///
  | countryname == "BosniaHerzegovina" ///
  | countryname == "BOSNIA-HERZEGOVINA" ///
  | countryname == "Bosnia & Herzegovina" ///
  | countryname == "Bosnia Herzegovina" ///
  | countryname == "Bosnia & Herzegovina" ///
  | countryname == "BOSNIA" ///
  | countryname == "Bosnia - Herzegovina" ///
  | countryname == "Bosnia-Herzogovina" ///
  | countryname == "BOSNIA-HERZ" ///
  | countryname == "Bosnia"
capture replace stateinyear=1 if countryname=="Bosnia-Herzegovina" & year >=1992

replace countryname = "Botswana" if countryname == "BOTSWANA" ///
  | countryname == "BOTSWANA" ///
  | countryname == "botswana"
capture replace stateinyear=1 if countryname=="Botswana" & year >=1966

replace countryname = "Brazil" if countryname == "BRAZIL" ///
  | countryname == "brazil"
capture replace stateinyear=1 if countryname=="Brazil" & year >=1822

  **Not in Gleditsch**
replace countryname = "British Virgin Islands" if countryname == "BRITISH VIRGIN ISLANDS" ///
  | countryname == "UK VIRGIN ISLANDS"

replace countryname = "Brunei" if countryname == "Brunei Darus" ///
  | countryname == "Brunei Darussalam" ///
  | countryname == "BRUNEI DARUSSALAM" ///
  | countryname == "BRUNEI"
capture replace stateinyear=1 if countryname=="Brunei" & year >=1984

replace countryname = "Bulgaria" if countryname == "BULGARIA" ///
  | countryname == "bulgaria"
capture replace stateinyear=1 if countryname=="Bulgaria" & year >=1878

replace countryname = "Burkina Faso (Upper Volta)" if countryname == "Upper Volta" ///
  | countryname == "Burkinafaso" ///
  | countryname == "Birkina Faso" ///
  | countryname == "Burkina-Faso" ///
  | countryname == "BURKINA FASO" ///
  | countryname == "BurkinaFaso" ///
  | countryname == "Burkina Faso, Upper Volta" ///
  | countryname == "BURKNA FASO" ///
  | countryname == "UPPER VOLTA" ///
  | countryname == "Burkina Faso"
capture replace stateinyear=1 if countryname=="Burkina Faso (Upper Volta)" & year >=1960

replace countryname = "Burundi" if countryname == "BURUNDI" ///
  | countryname == "burundi"
capture replace stateinyear=1 if countryname=="Burundi" & year >=1962


*C

replace countryname = "Cambodia (Kampuchea)" if countryname == "Cambodia" ///
  | countryname == "Kampochea" ///
  | countryname == "CAMBODIA" ///
  | countryname == "Cambodia, Khmer Republic, Kampuchea" ///
  | countryname == "KAMPUCHEA" ///
  | countryname == "KHMER REP" ///
  | countryname == "cambodia"
capture replace stateinyear=1 if countryname=="Cambodia (Kampuchea)" & year >=1953

replace countryname = "Cameroon" if countryname == "Cameroun" ///
  | countryname == "CAMEROON" ///
  | countryname == "cameroon"
capture replace stateinyear=1 if countryname=="Cameroon" & year >=1960

replace countryname = "Canada" if countryname == "CANADA"
capture replace stateinyear=1 if countryname=="Canada" & year >=1867

replace countryname = "Cape Verde" if countryname == "Capeverde" ///
  | countryname == "Cape-Verde" ///
  | countryname == "CAPE VERDE IS." ///
  | countryname == "CAPE VERDE" ///
  | countryname == "C VERDE IS" ///
  | countryname == "Cape Verde Is."
capture replace stateinyear=1 if countryname=="Cape Verde" & year >=1975

 **Gleditsch doesn't include the Cayman Islands**
replace countryname = "Cayman Islands" if countryname == "CAYMAN ISLANDS"


replace countryname = "Central African Republic" if countryname == "Centralafricanrepublic" ///
  | countryname == "Central African Rep" ///
  | countryname == "Central African Rep." ///
  | countryname == "CENTRAL AFR.R." ///
  | countryname == "CENTRAL AFRICAN REP." ///
  | countryname == "CENTRAL AFRICAN REPUBLIC" ///
  | countryname == "Central African republic" ///
  | countryname == "Central African Repub" ///
  | countryname == "CEN AFR EMP" ///
  | countryname == "CEN AFR REP" ///
  | countryname == "Central Afr.R."
capture replace stateinyear=1 if countryname=="Central African Republic" & year >=1960

replace countryname = "Chad" if countryname == "CHAD" ///
  | countryname == "chad" ///
  | countryname == "Rep. Chad"
capture replace stateinyear=1 if countryname=="Chad" & year >=1960

****Gleditsch does not include the Channel Islands****

replace countryname = "Chile" if countryname == "CHILE" ///
  | countryname == "chile"
capture replace stateinyear=1 if countryname=="Chile" & year >=1818

replace countryname = "China" if countryname == "People'S Republic Of China" ///
  | countryname == "Chn" ///
  | countryname == "China, People'S Republic Of" ///
  | countryname == "CHINA" ///
  | countryname == "china" ///
  | countryname == "People's Republic of China" ///
  | countryname == "CHINA PR" ///
  | countryname == "CHN"
capture replace stateinyear=1 if countryname=="China" & year >=1816

replace countryname = "Colombia" if countryname == "Columbia" ///
  | countryname == "COLOMBIA"
capture replace stateinyear=1 if countryname=="Colombia" & year >=1830

replace countryname = "Comoros" if countryname == "COMOROS" ///
  | countryname == "COMORO IS"
capture replace stateinyear=1 if countryname=="Comoros" & year >=1975

replace countryname = "Congo" if countryname == "Congo, Rep." ///
  | countryname == "Congo/ Rep." ///
  | countryname == "Congo, Republic Of" ///
  | countryname == "Congo, Rep." ///
  | countryname == "Congo-Brazzaville" ///
  | countryname == "Congo (Brazzaville)" ///
  | countryname == "Congo Brazzaville" ///
  | countryname == "CONGO" ///
  | countryname == "CONGO, REP." ///
  | countryname == "Congo, Rep." ///
  | countryname == "Congo, Republic of the" ///
  | countryname == "Congo, Republic of" ///
  | countryname == "Congo/ Rep." ///
  | countryname == "CONGO, REP.                                                                " ///
  | countryname == "Congo, Rep.                                                                " ///
  | countryname == "CONGO, REPUBLIC OF" ///
  | countryname == "Congo, Republic of (Brazzaville)" ///
  | countryname == "REP. OF CONGO" ///
  | countryname == "CONGO (BRA)" ///
  | countryname == "CONGO REP" ///
  | countryname == "Congo, Republic Of The"
capture replace stateinyear=1 if countryname=="Congo" & year >=1960

replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem.Rep." ///
  | countryname == "Congo, Democratic Republic Of / Zaire" ///
  | countryname == "Congo, Dem. Rep." ///
  | countryname == "Zaire" ///
  | countryname == "Congo, Democratic Republic Of" ///
  | countryname == "Congo-Kinshasa" ///
  | countryname == "Congo/ Dem. Rep." ///
  | countryname == "Congo, Democratic Rep." ///
  | countryname == "Congo, Dem Rep" ///
  | countryname == "Democratic Republic Of The Congo" ///
  | countryname == "Democratic Republic Of Congo" ///
  | countryname == "Congo (Kinshasa)" ///
  | countryname == "Congo Kinshasa" ///
  | countryname == "Congo, DR" ///
  | countryname == "Congo, Dr"
  replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem. Rep." ///
  | countryname == "CONGO, DEM.REP." ///
  | countryname == "CONGO, DEM.REP.                                                            " ///
  | countryname == "Congo, Democratic Republic of / Zaire" ///
  | countryname == "Congo/ Dem. Rep." ///
  | countryname == "Democratic Republic of the Congo" ///
  | countryname == "ZAIRE" ///
  | countryname == "Zaire" ///
  | countryname == "Congo, Dem. Rep." ///
  | countryname == "Congo, Democratic Republic of" ///
  | countryname == "Congo, Democratic Republic" ///
  | countryname == "Congo, Dem.Rep.                                                            " ///
  | countryname == "Congo, Dr.                                                           "
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem. Rep." ///
  | countryname == "CONGO, DEMOCRACTIC REPBULIC OF" ///
  | countryname == "DEM. REP. CONGO" ///
  | countryname == "CONGO (KIN)" ///
  | countryname == "CONGO DR" ///
  | countryname == "Democratic Republic Of Congo (Zaire)"
capture replace stateinyear=1 if countryname=="Congo, Democratic Republic of (Zaire)" & year >=1960

replace countryname = "Costa Rica" if countryname == "Costarica" ///
  | countryname == "COSTARICA" ///
  | countryname == "COSTA RICA"
capture replace stateinyear=1 if countryname=="Costa Rica" & year >=1840

replace countryname = "Cote D'Ivoire" if countryname == "C(te d'Ivoire" ///
  | countryname == "C+te D?Ivoire" ///
  | countryname == "Ctte D'Ivoire" ///
  | countryname == "Cote D'Ivore" ///
  | countryname == "Cote-D'Ivoire" ///
  | countryname == "C(te D?ivoire" ///
  | countryname == "Cotedivoire" ///
  | countryname == "Ivory Coast" ///
  | countryname == "Côte D'Ivoire" ///
  | countryname == "CÌ«te D'Ivoire" ///
  | countryname == "Cote D'Ivoire" ///
  | countryname == "c(te d?ivoire" ///
  | countryname == "C(te d'Ivoire" ///
  | countryname == "C+TE D?IVOIRE" ///
  | countryname == "Cote d`Ivoire" ///
  | countryname == "Cote d´Ivoire" ///
  | countryname == "Cote D'Ivoire" ///
  | countryname == "Côte d’Ivoire" ///
  | countryname == "COTE D'IVOIRE" ///
  | countryname == "Cote D'Ivoire" ///
  | countryname == "Cote d'Ivoire" ///
  | countryname == "IVORY COAST" ///
  | countryname == "Ivory Coast" ///
  | countryname == "Cote D’Ivoire" ///
  | countryname == "Cote d'Ivoire (Ivory Coast)" ///
  | countryname == "COTE D'IVOR" ///
  | countryname == "CôTe D'Ivoire"
capture replace stateinyear=1 if countryname=="Cote D'Ivoire" & year >=1960
***This is a departure from Gleditsch, where the name is "Cote D’Ivoire"*** 

replace countryname = "Croatia" if countryname == "CROATIA" ///
  | countryname == "croatia"
capture replace stateinyear=1 if countryname=="Croatia" & year >=1991

replace countryname = "Cuba" if countryname == "CUBA"
capture replace stateinyear=1 if countryname=="Cuba" & year >=1902

replace countryname = "Cyprus" if countryname == "CYPRUS" ///
  | countryname == "Cyprus (Greek)"
capture replace stateinyear=1 if countryname=="Cyprus" & year >=1960

replace countryname = "Czech Republic" if countryname == "Czechrepublic" ///
  | countryname == "Czech Rep" ///
  | countryname == "Czech Rep." ///
  | countryname == "Czech Republic (Czechoslovakia)" ///
  | countryname == "CZECHREP" ///
  | countryname == "CZECH REP" ///
  | countryname == "CZECH REPUBLIC"
capture replace stateinyear=1 if countryname=="Czech Republic" & year >=1993

replace countryname = "Czechoslovakia" if countryname == "Csfr (Czechoslovakia)" ///
  | countryname == "Czechoslovakia (Former)" ///
  | countryname == "CSFR (CZECHOSLOVAKIA)" ///
  | countryname == "CZECHOS'KIA" ///
  | countryname == "CZECHOSLOVAKIA"
capture replace stateinyear=1 if countryname=="Czechoslovakia" & year >=1919 & year <=1992


*D 

replace countryname = "Denmark" if countryname == "DENMARK"
capture replace stateinyear=1 if countryname=="Denmark" & year >=1816

replace countryname = "Djibouti" if countryname == "Dijbouti" ///
  | countryname == "Djibouti, French Somaliland" ///
  | countryname == "DJIBOUTI"
capture replace stateinyear=1 if countryname=="Djibouti" & year >=1977

replace countryname = "Dominica" if countryname == "DOMINICA"
capture replace microstateinyear=1 if countryname=="Dominica" & year >=1978

replace countryname = "Dominican Republic" if countryname == "Dominican Rep." ///
  | countryname == "Dominican Rep" ///
  | countryname == "Dominicanrep" ///
  | countryname == "DOMINICAN REP." ///
  | countryname == "DOMINICAN REPUBLIC" ///
  | countryname == "DOMIN REP" ///
  | countryname == "dominican republic"
capture replace stateinyear=1 if countryname=="Dominican Republic" & year >=1844


*E

replace countryname = "East Timor" if countryname =="Timor-Leste" ///
  | countryname =="Timor Leste" ///
  | countryname =="Timore-Leste" ///
  | countryname == "East Timor LESTE" ///
  | countryname == "EAST TIMOR" ///
  | countryname == "TIMOR" ///
  | countryname == "TIMOR-LESTE" ///
  | countryname == "East Timor (Timor-Leste)" ///
  | countryname == "East Timor Leste"
capture replace stateinyear=1 if countryname=="East Timor" & year >=2002

replace countryname = "Ecuador" if countryname == "ECUADOR" ///
  | countryname == "ecuador"
capture replace stateinyear=1 if countryname=="Ecuador" & year >=1830

replace countryname = "Egypt" if countryname == "Arab Republic Of Egypt" ///
  | countryname == "Egypt, Arab Rep." ///
  | countryname == "Egypt, Arab Republic Of" ///
  | countryname == "Egypt Arab Republic Of" ///
  | countryname == "Arab Republic of Egypt" ///
  | countryname == "Egypt Arab Republic of" ///
  | countryname == "EGYPT" ///
  | countryname == "egypt" ///
  | countryname == "Egypt, Arab Rep."
capture replace stateinyear=1 if countryname=="Egypt" & year >=1827 & year<=1855
capture replace stateinyear=1 if countryname=="Egypt" & year >=1922

replace countryname = "El Salvador" if countryname == "Elsalvador" ///
  | countryname == "EL SALVADOR" ///
  | countryname == "el salvador" 
capture replace stateinyear=1 if countryname=="El Salvador" & year >=1840

replace countryname = "Equatorial Guinea" if countryname == "EQUATORIAL GUINEA" ///
  | countryname == "Equitorial Guinea" ///
  | countryname == "EQUA GUINEA"
capture replace stateinyear=1 if countryname=="Equatorial Guinea" & year >=1968

replace countryname = "Eritrea" if countryname == "ERITREA"
capture replace stateinyear=1 if countryname=="Eritrea" & year >=1993

replace countryname = "Estonia" if countryname == "ESTONIA"
capture replace stateinyear=1 if countryname=="Estonia" & year >=1918 & year <=1940
capture replace stateinyear=1 if countryname=="Estonia" & year >=1991

replace countryname = "Ethiopia" if countryname == "ETHIOPIA" ///
  | countryname == "ETH'PIA FDR" ///
  | countryname == "ETH'PIA PDR" 
capture replace stateinyear=1 if countryname=="Ethiopia" & year >=1855


*F

  **Gleditsch does not include the Faeroe Islands**
replace countryname = "Faeroe Islands" if countryname == "Faeroe_Is" ///
  | countryname == "FAEROE ISLANDS"

  **Not in Gledtisch**
replace countryname = "Falkland Islands" if countryname == "Falkland Islands (Malvinas)" ///
  | countryname == "Falkland Is.(Malvinas)" ///
  | countryname == "Falkland Is(Malvinas)" ///
  | countryname == "FALKLAND ISLANDS"


replace countryname = "Federated States of Micronesia" if countryname == "Federated States Of Micronesia" ///
  | countryname == "Micronesia" ///
  | countryname == "Micronesia, Fed States Of" ///
  | countryname == "Micronesia, Fed States" ///
  | countryname == "Micronesia, Fed. States Of" ///
  | countryname == "Micronesia, Fed. States" ///
  | countryname == "Micronesia, Federated States Of" ///
  | countryname == "Federal States Of Micronesia" ///
  | countryname == "Micronesia, Fed. Sts." ///
  | countryname == "Micronesia, Fed Sts" ///
  | countryname == "Micronesia (Federated States of)" ///
  | countryname == "MICRONESIA" ///
  | countryname == "MICRONESIA,FED. STATES                                                     " ///
  | countryname == "MICRONESIA, FED. STATES"
  replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed. Sts." ///
  | countryname == "Micronesia, Fed Stat" ///
  | countryname == "Micronesia, Federated States of" ///
  | countryname == "Micronesia,Fed. States                                                     "
capture replace microstateinyear=1 if countryname=="Federated States of Micronesia" & year >=1986

replace countryname = "Fiji" if countryname == "Fiji Islands" ///
  | countryname == "FIJI" 
capture replace stateinyear=1 if countryname=="Fiji" & year >=1970

replace countryname = "Finland" if countryname == "FINLAND"
capture replace stateinyear=1 if countryname=="Finland" & year >=1917

replace countryname = "France" if countryname == "FRANCE"
capture replace stateinyear=1 if countryname=="France" & year >=1816

replace countryname = "French Guiana" if countryname == "FRENCH GUIANA" ///
  | countryname == "Guiana, French"
****Not in Gleditsch****

replace countryname = "French Polynesia" if countryname == "FRENCH POLYNESIA"
****Not in Gleditsch****

*G 

replace countryname = "Gabon" if countryname == "GABON" ///
  | countryname == "gabon"
capture replace stateinyear=1 if countryname=="Gabon" & year >=1960

replace countryname = "Gambia" if countryname == "The Gambia" ///
  | countryname == "Gambia, The" ///
  | countryname == "GAMBIA" ///
  | countryname == "Gambia, The      " ///
  | countryname == "Gambia, The" ///
  | countryname == "The Gambia"
capture replace stateinyear=1 if countryname=="Gambia" & year >=1965

replace countryname = "Georgia" if countryname == "GEORGIA" ///
  | countryname == "georgia"
capture replace stateinyear=1 if countryname=="Georgia" & year >=1991

replace countryname = "German Democratic Republic" if countryname == "GDR (Ex)" ///
  | countryname == "East Germany" ///
  | countryname == "Germany Dem. Rep." ///
  | countryname == "Germany Dem Rep" ///
  | countryname == "German Dem. Rep." ///
  | countryname == "German Dem Rep" ///
  | countryname == "GDR (ex)" ///
  | countryname == "GERMANY, EAST" ///
  | countryname == "Germany East" ///
  | countryname == "Germany, E." ///
  | countryname == "GERMAN DEM. REP." ///
  | countryname == "Germany, E. " ///
  | countryname == "East Germany (GDR)" ///
  | countryname == "German Democratic Republic (East Germany)" ///
  | countryname == "EAST GERMANY" ///
  | countryname == "GERMAN DR" ///
  | countryname == "Germany, East"
capture replace stateinyear=1 if countryname=="German Democratic Republic" & year >=1949 & year <=1990

replace countryname = "German Federal Republic" if countryname == "Germany" ///
  | countryname == "Germany, West" ///
  | countryname == "West Germany" ///
  | countryname == "Germany_Unified" ///
  | countryname == "Germany Fed. Rep." ///
  | countryname == "Germany Fed Rep" ///
  | countryname == "German Fed. Rep." ///
  | countryname == "German Fed Rep" ///
  | countryname == "GERMANY" ///
  | countryname == "GERMANY, WEST" ///
  | countryname == "WEST GERMANY" ///
  | countryname == "West Germany" ///
  | countryname == "Germany West" ///
  | countryname == "Germany, W." ///
  | countryname == "German Federal Republic" /// 
  | countryname == "GERMANYFED. REP." /// 
  | countryname == "Germany, W. " /// 
  | countryname == "West Germany (FRG)" /// 
  | countryname == "Federal Republic of Germany (West Germany)/Germany" /// 
  | countryname == "Federal Republic of Germany" /// 
  | countryname == "GERMAN FR" /// 
  | countryname == "Federal Republic Of Germany"
capture replace stateinyear=1 if countryname=="German Federal Republic" & year >=1949

*Germany (Prussia)
replace countryname="Germany (Prussia)" if countryname=="Prussia"
capture replace stateinyear=1 if countryname=="Germany (Prussia)" & year >=1816 & year <=1945

replace countryname = "Ghana" if countryname == "GHANA" ///
  | countryname == "ghana"
capture replace stateinyear=1 if countryname=="Ghana" & year >=1957

replace countryname = "Gibralter" if countryname == "GIBRALTAR"
****Not in Gleditsch****

replace countryname = "Greece" if countryname == "GREECE"
capture replace stateinyear=1 if countryname=="Greece" & year >=1827

replace countryname = "Grenada" if countryname == "GRENADA"
capture replace microstateinyear=1 if countryname=="Grenada" & year >=1974

****Greenland (Denmark)--Not in Gleditsch****

replace countryname = "Guadeloupe" if countryname == "GUADELOUPE"
****Not in Gleditsch****

replace countryname = "Guatemala" if countryname == "GUATEMALA" ///
  | countryname == "guatemala"
capture replace stateinyear=1 if countryname=="Guatemala" & year >=1840

****Guam (US)--Not in Gleditsch****

replace countryname = "Guiana" if countryname == "GUIANA"
****Not in Gleditsch****

replace countryname = "Guinea" if countryname == "GUINEA" ///
  | countryname == "guinea"
capture replace stateinyear=1 if countryname=="Guinea" & year >=1958

replace countryname = "Guinea-Bissau" if countryname == "Guineabissau" ///
  | countryname == "Guinea Bissau" ///
  | countryname == "Guinea Bissau" ///
  | countryname == "GUINEA-BISS" ///
  | countryname == "GuineaBissau" ///
  | countryname == "GUINEA-BISSAU" ///
  | countryname == "GUINEA BISSAU" ///
  | countryname == "GUINEA-B'AU" ///
  | countryname == "Guinea-Biss"
capture replace stateinyear=1 if countryname=="Guinea-Bissau" & year >=1974

replace countryname = "Guyana" if countryname == "GUYANA" ///
  | countryname == "Guyana, British Guiana"
capture replace stateinyear=1 if countryname=="Guyana" & year >=1966


*H 

replace countryname = "Haiti" if countryname == "HAITI"
capture replace stateinyear=1 if countryname=="Haiti" & year >=1816 & year <=1915
capture replace stateinyear=1 if countryname=="Haiti" & year >=1934

*Hanover
capture replace stateinyear=1 if countryname=="Hanover" & year >=1816 & year <=1871

*Hesse-Darmstadt (Ducal)
capture replace stateinyear=1 if countryname=="Hesse-Darmstadt (Ducal)" & year >=1816 & year <=1871

*Hesse-Kassel (Electoral)
capture replace stateinyear=1 if countryname=="Hesse-Kassel (Electoral)" & year >=1816 & year <=1871

replace countryname = "Honduras" if countryname == "HONDURAS" ///
  | countryname == "honduras"
capture replace stateinyear=1 if countryname=="Honduras" & year >=1840

replace countryname = "Hong Kong" if countryname == "Hongkong" ///
  | countryname == "Hong Kong, China" ///
  | countryname == "Hong Kong China" ///
  | countryname == "Hong Kong Sar" ///
  | countryname == "CHINA, HONG KONG SAR" ///
  | countryname == "HONG KONG" ///
  | countryname == "HONG KONG, CHINA                                                           " ///
  | countryname == "HONG KONG, CHINA" ///
  | countryname == "Hong Kong, China" ///
  | countryname == "Hong Kong, China                                                           " ///
  | countryname == "China,P.R.:Hong Kong"
****Not in Gleditsch****

replace countryname = "Hungary" if countryname == "HUNGARY"
capture replace stateinyear=1 if countryname=="Hungary" & year >=1918

*I 

replace countryname = "Iceland" if countryname == "ICELAND"
capture replace stateinyear=1 if countryname=="Iceland" & year >=1944

replace countryname = "India" if countryname == "Ind" ///
  | countryname == "Indiapost1947" ///
  | countryname == "Indiapre1948" ///
  | countryname == "IND" ///
  | countryname == "INDIA" ///
  | countryname == "india"
capture replace stateinyear=1 if countryname=="India" & year >=1947

replace countryname = "Indonesia" if countryname == "IDN" ///
  | countryname == "INDONESIA" ///
  | countryname == "indonesia" ///
  | countryname == "Idn"
capture replace stateinyear=1 if countryname=="Indonesia" & year >=1945

replace countryname = "Iran (Persia)" if countryname == "Iran" ///
  | countryname == "Iran (Islamic Republic Of)" ///
  | countryname == "Iran, Islamic Rep" ///
  | countryname == "Iran, Islamic Rep." ///
  | countryname == "Iran, Islamic Republic Of" ///
  | countryname == "Iran (Islamic Rep. Of)" ///
  | countryname == "Iran (Islamic Republic of)" ///
  | countryname == "IRAN ISLAMIC REP. OF" ///
  | countryname == "IRAN" ///
  | countryname == "Iran, Islamic Rep." 
capture replace stateinyear=1 if countryname=="Iran (Persia)" & year >=1816

replace countryname = "Iraq" if countryname == "IRAQ"
capture replace stateinyear=1 if countryname=="Iraq" & year >=1932

replace countryname = "Ireland" if countryname == "IRELAND"
capture replace stateinyear=1 if countryname=="Ireland" & year >=1921

replace countryname = "Isle of Man" if countryname == "ISLE OF MAN"
****Not in Gleditsch****

replace countryname = "Israel" if countryname == "ISRAEL"
capture replace stateinyear=1 if countryname=="Israel" & year >=1948

replace countryname = "Italy/Sardinia" if countryname == "ITALY" ///
  | countryname == "Italy" ///
  | countryname=="Sardinia"
capture replace stateinyear=1 if countryname=="Italy/Sardinia" & year >=1816


*J

replace countryname = "Jamaica" if countryname == "JAMAICA"
capture replace stateinyear=1 if countryname=="Jamaica" & year >=1962

replace countryname = "Japan" if countryname == "JAPAN"
capture replace stateinyear=1 if countryname=="Japan" & year >=1816

replace countryname = "Jordan" if countryname == "JORDAN" ///
  | countryname == "jordan"
capture replace stateinyear=1 if countryname=="Jordan" & year >=1946


*K

replace countryname = "Kazakhstan" if countryname == "Khazakhstan" ///
  | countryname == "Kazakstan" ///
  | countryname == "KAZAKHSTAN" ///
  | countryname == "kazakhstan" ///
  | countryname == "Kazakstan" ///
  | countryname == "Khazakhstan"
capture replace stateinyear=1 if countryname=="Kazakhstan" & year >=1991

replace countryname = "Kenya" if countryname == "KENYA" ///
  | countryname == "kenya"
capture replace stateinyear=1 if countryname=="Kenya" & year >=1963

replace countryname = "Kiribati" if countryname == "KIRIBATI"
capture replace microstateinyear=1 if countryname=="Kiribati" & year >=1979

replace countryname = "Korea, People's Republic of" if countryname == "Korea, People'S Republic Of" ///
  | countryname == "Korea, Dem." ///
  | countryname == "Prk" ///
  | countryname == "North Korea" ///
  | countryname == "Northkorea" ///
  | countryname == "Korea, Dem People'S Rep" ///
  | countryname == "Korea, Democratic People'S Republic Of" ///
  | countryname == "Korea, Dem. Rep." ///
  | countryname == "Korea Dpr" ///
  | countryname == "Korea Dem.People's Rep." ///
  | countryname == "Korea, North" ///
  | countryname == "KOREA, DEM. REP." ///
  | countryname == "Korea, Dem. Rep." ///
  | countryname == "KOREA, DEM." ///
  | countryname == "Korea, Democratic People's Republic of" ///
  | countryname == "Korea, DPR" ///
  | countryname == "Korea, N."
  replace countryname = "Korea, People's Republic of" if countryname == "NORTH KOREA" ///
  | countryname == "North Korea" ///
  | countryname == "PRK" ///
  | countryname == "Korea, Dem." ///
  | countryname == "KOREA, DEM.                                                                 " ///
  | countryname == "Korea North" ///
  | countryname == "Korea, Democratic People'S Republic" ///
  | countryname == "Korea, Dem.                                                                " ///
  | countryname == "Korea, Democratic People'S Republic Of " ///
  | countryname == "Korea, Dpr" ///
  | countryname == "Korea Dem.People'S Rep." ///
  | countryname == "People's Republic of Korea" ///
  | countryname == "KOREA, NORTH" ///
  | countryname == "Korea, Democratic People's Republic" ///
  | countryname == "N. KOREA" ///
  | countryname == "KOREA PR" ///
  | countryname == "People'S Republic Of Korea"
capture replace stateinyear=1 if countryname=="Korea, People's Republic of" & year >=1948

replace countryname = "Korea, Republic of" if countryname == "Korea, Republic Of" ///
  | countryname == "Korea" ///
  | countryname == "Korea, Rep." ///
  | countryname == "Southkorea" ///
  | countryname == "South Korea" ///
  | countryname == "Republic Of Korea" ///
  | countryname == "Korea, Rep" ///
  | countryname == "Korea Republic of" ///
  | countryname == "KOREA, REP." ///
  | countryname == "Korea, Rep." ///
  | countryname == "Korea, Republic Of" ///
  | countryname == "KOREA, REPUBLIC OF" ///
  | countryname == "korea, republic of" ///
  | countryname == "Korea, Republic of" ///
  | countryname == "Korea, Republic of" ///
  | countryname == "Korea, S." ///
  | countryname == "Korea,Republic of" ///
  | countryname == "REPUBLIC OF KOREA" ///
  | countryname == "Republic of Korea" ///
  | countryname == "SOUTH KOREA" ///
  | countryname == "Korea" ///
  | countryname == "Korea South" ///
  | countryname == "South Korea" ///
  | countryname == "KOREA, REP.                                                                " ///
  | countryname == "Korea, Rep.                                                                " ///
  | countryname == "KOREA, SOUTH" ///
  | countryname == "KOREA, S." ///
  | countryname == "S. KOREA" ///
  | countryname == "KOREA REP" ///
  | countryname == "Korea,Republic Of"
capture replace stateinyear=1 if countryname=="Korea, Republic of" & year >=1948

*Korea
capture replace stateinyear=1 if countryname=="Korea" & year >=1816 & year <=1910


replace countryname = "Kosovo" if countryname == "KOSOVO"
capture replace stateinyear=1 if countryname=="Kosovo" & year >=2008


replace countryname = "Kuwait" if countryname == "KUWAIT"
capture replace stateinyear=1 if countryname=="Kuwait" & year >=1961

replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyz Rep." ///
  | countryname == "Kyrgyz Rep" ///
  | countryname == "Kyrghz Republic" ///
  | countryname == "Kyrgyz" ///
  | countryname == "Kyrgyzstan" ///
  | countryname == "Kyrghz republic" ///
  | countryname == "KYRGYZ REP." ///
  | countryname == "Kyrgyz Rep." ///
  | countryname == "KYRGYZ REPUBLIC" ///
  | countryname == "kyrgyz republic" ///
  | countryname == "Kyrgyz" ///
  | countryname == "KYRGYZSTAN" ///
  | countryname == "Kyrgyzstan" ///
  | countryname == "Kyrgyz Rep."
capture replace stateinyear=1 if countryname=="Kyrgyz Republic" & year >=1991


*L

replace countryname = "Laos" if countryname == "Lao" ///
  | countryname == "Lao People?S Democratic Republic" ///
  | countryname == "Lao People'S Dem Rep" ///
  | countryname == "Lao People'S Dem. Rep." ///
  | countryname == "Lao People'S Democratic Republic" ///
  | countryname == "Lao Pdr" ///
  | countryname == "Lao PDR" ///
  | countryname == "LAO PEOPLE?S DEMOCRATIC REPUBLIC" ///
  | countryname == "Lao People?s Democratic Republic" ///
  | countryname == "lao people?s democratic republic" ///
  | countryname == "Lao People´s Democratic Republic" ///
  | countryname == "LAO PEOPLE'S DEM. REPUBLIC" ///
  | countryname == "LAO" ///
  | countryname == "LAOS"
capture replace stateinyear=1 if countryname=="Laos" & year >=1954

replace countryname = "Latvia" if countryname == "LATVIA"
capture replace stateinyear=1 if countryname=="Latvia" & year >=1918 & year <=1940
capture replace stateinyear=1 if countryname=="Latvia" & year >=1991

replace countryname = "Lebanon" if countryname == "LEBANON" ///
  | countryname == "lebanon"
capture replace stateinyear=1 if countryname=="Lebanon" & year >=1944

replace countryname = "Lesotho" if countryname == "LESOTHO"
capture replace stateinyear=1 if countryname=="Lesotho" & year >=1966

replace countryname = "Liberia" if countryname == "LIBERIA"
capture replace stateinyear=1 if countryname=="Liberia" & year >=1847

replace countryname = "Libya" if countryname == "Libyan Arab Jamahiriya" ///
  | countryname == "Libya Arab Jamahiriy" ///
  | countryname == "LIBYA" ///
  | countryname == "Libyan Arab Jamahiriya" ///
  | countryname == "LIBYAN ARAB JAMAHIRIYA"
capture replace stateinyear=1 if countryname=="Libya" & year >=1816 & year <=1834
capture replace stateinyear=1 if countryname=="Libya" & year >=1951

replace countryname = "Liechtenstein" if countryname == "LIECHTENSTEIN" ///
  | countryname == "LIECHTSTEIN"
capture replace microstateinyear=1 if countryname=="Liechtenstein" & year >=1816

replace countryname = "Lithuania" if countryname == "LITHUANIA" ///
  | countryname == "lithuania"
capture replace stateinyear=1 if countryname=="Lithuania" & year >=1918 & year <=1940
capture replace stateinyear=1 if countryname=="Lithuania" & year >=1991

replace countryname = "Luxembourg" if countryname == "LUXEMBOURG"  /// 
  | countryname == "Luxemburg"
capture replace stateinyear=1 if countryname=="Luxembourg" & year >=1867

*M

replace countryname = "Macao" if countryname == "Macao, China" ///
  | countryname == "Macau" ///
  | countryname == "MACAO" ///
  | countryname == "China,P.R.:Macao" ///
  | countryname == "Macao, China"
****Not in Gleditsch****

replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia (Former Yugoslav Republic Of)" ///
  | countryname == "Macedonia" ///
  | countryname == "Fyrmacedonia" ///
  | countryname == "Fyrom-Macedonia" ///
  | countryname == "Macedonia, The Fmr Yug Rp" ///
  | countryname == "Macedonia, Fyr" ///
  | countryname == "The Former Yugoslav Republic Of Macedonia" ///
  | countryname == "Fyr Macedonia" ///
  | countryname == "Macedonia, Former Yugoslav Republic Of" ///
  | countryname == "FYROM-MACEDONIA" ///
  | countryname == "MACEDONIA" ///
  | countryname == "MACEDONIA" ///
  | countryname == "macedonia" ///
  | countryname == "MACEDONIA, FYR" ///
  | countryname == "Macedonia, FYR" ///
  | countryname == "The fmr Yug Rep Macedonia" ///
  | countryname == "The Former Yugoslav Republic of Macedonia" ///
  | countryname == "Macedonia (FYRM)" ///
  | countryname == "The Fmr Yug Rep Macedonia"
capture replace stateinyear=1 if countryname=="Macedonia (Former Yugoslav Republic of)" & year >=1991

replace countryname = "Madagascar (Malagasy)" if countryname == "Madagascar" ///
  | countryname == "MADAGASCAR" ///
  | countryname == "MALAGASY R" ///
  | countryname == "madagascar"
capture replace stateinyear=1 if countryname=="Madagascar (Malagasy)" & year >=1816 & year <=1896
capture replace stateinyear=1 if countryname=="Madagascar (Malagasy)" & year >=1960

replace countryname = "Malawi" if countryname == "MALAWI" ///
  | countryname == "malawi"
capture replace stateinyear=1 if countryname=="Malawi" & year >=1964

replace countryname = "Malaysia" if countryname == "MALAYSIA" ///
  | countryname == "malaysia"
capture replace stateinyear=1 if countryname=="Malaysia" & year >=1957

replace countryname = "Maldives" if countryname == "MALDIVES" ///
  | countryname == "MALDIVE IS"
capture replace stateinyear=1 if countryname=="Maldives" & year >=1965

replace countryname = "Mali" if countryname == "MALI" ///
  | countryname == "mali"
capture replace stateinyear=1 if countryname=="Mali" & year >=1960

replace countryname = "Malta" if countryname == "MALTA"
capture replace stateinyear=1 if countryname=="Malta" & year >=1964

replace countryname = "Marshall Islands" if countryname == "MARSHALL ISLANDS" ///
  | countryname == "MARSHALL IS"
capture replace microstateinyear=1 if countryname=="Marshall Islands" & year >=1986

replace countryname = "Martinique" if countryname == "MARTINIQUE"
****Not in Gleditsch****

replace countryname = "Mauritania" if countryname == "Mauritiana" ///
  | countryname == "mauritania" ///
  | countryname == "MAURITANIA" 
capture replace stateinyear=1 if countryname=="Mauritania" & year >=1960

replace countryname = "Mauritius" if countryname == "Mauritus" ///
  | countryname == "Maurituis" ///
  | countryname == "MAURITIUS"
capture replace stateinyear=1 if countryname=="Mauritius" & year >=1968

replace countryname = "Mayotte" if countryname == "MAYOTTE" 
****Not in Gleditsch****

*Mecklenburg-Schwerin
capture replace stateinyear=1 if countryname=="Mecklenburg-Schwerin" & year >=1816 & year <=1871

replace countryname = "Mexico" if countryname == "mexico" ///
  | countryname == "MEXICO"
capture replace stateinyear=1 if countryname=="Mexico" & year >=1821

*Modena
capture replace stateinyear=1 if countryname=="Modena" & year >=1816 & year <=1861

replace countryname = "Moldova" if countryname == "Moldova, Republic Of" ///
  | countryname == "Moldova, Rep" ///
  | countryname == "Moldova, Rep." ///
  | countryname == "Republic Of Moldova" ///
  | countryname == "moldova" ///
  | countryname == "MOLDOVA" ///
  | countryname == "Moldova, Republic of" ///
  | countryname == "Moldova, Republic Of" ///
  | countryname == "Republic of Moldova"
capture replace stateinyear=1 if countryname=="Moldova" & year >=1991

*Monaco
replace countryname = "Monaco" if countryname=="MONACO"
capture replace microstateinyear=1 if countryname=="Monaco" & year >=1816


replace countryname = "Mongolia" if countryname == "MONGOLIA"
capture replace stateinyear=1 if countryname=="Mongolia" & year >=1921

*Montenegro
replace countryname = "Montenegro" if countryname == "MONTENEGRO"
capture replace stateinyear=1 if countryname=="Montenegro" & year >=1868 & year <= 1915
capture replace stateinyear=1 if countryname=="Montenegro" & year >=2006

replace countryname = "Montserrat" if countryname == "MONTSERRAT"
****Not in Gleditsch****

replace countryname = "Morocco" if countryname == "morocco" ///
  | countryname == "MOROCCO"
capture replace stateinyear=1 if countryname=="Morocco" & year >=1816 & year <=1904
capture replace stateinyear=1 if countryname=="Morocco" & year >=1956

replace countryname = "Mozambique" if countryname == "mozambique" ///
  | countryname == "MOZAMBIQUE"
capture replace stateinyear=1 if countryname=="Mozambique" & year >=1975

replace countryname = "Myanmar (Burma)" if countryname == "Myanmar" ///
  | countryname == "Mayanmar" ///
  | countryname == "MYANMAR" ///
  | countryname == "Burma (Myanmar)" ///
  | countryname == "BURMA" ///
  | countryname == "Burma/Myanmar" ///
  | countryname == "Burma"
capture replace stateinyear=1 if countryname=="Myanmar (Burma)" & year >=1816 & year <= 1885
capture replace stateinyear=1 if countryname=="Myanmar (Burma)" & year >=1948


*N 

replace countryname = "Namibia" if countryname == "NAMIBIA" ///
  | countryname == "Namibia, South West Africa" /// 
  | countryname == "namibia"
capture replace stateinyear=1 if countryname=="Namibia" & year >=1990

replace countryname = "Nauru" if countryname == "NAURU"
capture replace microstateinyear=1 if countryname=="Nauru" & year >=1968

replace countryname = "Nepal" if countryname == "NEPAL"
capture replace stateinyear=1 if countryname=="Nepal" & year >=1816

replace countryname = "Netherlands" if countryname == "NETHERLANDS"
capture replace stateinyear=1 if countryname=="Netherlands" & year >=1816

replace countryname = "New Caledonia" if countryname == "Newcaledonia" ///
  | countryname == "NEW CALEDONIA"
****Not in Gleditsch****

replace countryname = "New Zealand" if countryname == "NEW ZEALAND"
capture replace stateinyear=1 if countryname=="New Zealand" & year >=1907

replace countryname = "Nicaragua" if countryname == "NICARAGUA" ///
  | countryname == "nicaragua"
capture replace stateinyear=1 if countryname=="Nicaragua" & year >=1840

replace countryname = "Niger" if countryname == "NIGER"
capture replace stateinyear=1 if countryname=="Niger" & year >=1960

replace countryname = "Nigeria" if countryname == "NIGERIA" ///
  | countryname == "nigeria"
capture replace stateinyear=1 if countryname=="Nigeria" & year >=1960

replace countryname = "Niue" if countryname == "NIUE"
****Not in Gleditsch****

replace countryname = "Northern Marianas" if countryname == "N. Mariana Islands" ///
  | countryname == "N Mariana Islands" ///
  | countryname == "Northern Mariana Islands" ///
  | countryname == "Northern Mariana Is"  ///
  | countryname == "Northern Mariana Is."  ///
  | countryname == "NORTHERN MARIANAS"
****Not in Gleditsch****

replace countryname = "Norway" if countryname == "NORWAY"
capture replace stateinyear=1 if countryname=="Norway" & year >=1905


*O 

replace countryname = "Oman" if countryname == "OMAN" ///
  | countryname == "Muscat and Oman" /// 
  | countryname == "oman"
capture replace stateinyear=1 if countryname=="Oman" & year >=1816

*Orange Free State
capture replace stateinyear=1 if countryname=="Orange Free State" & year >=1854 & year <=1910


*P

replace countryname = "Pakistan" if countryname == "PAKISTAN" ///
  | countryname == "pakistan"
capture replace stateinyear=1 if countryname=="Pakistan" & year >=1947

replace countryname = "Palau" if countryname == "PALAU"
capture replace microstateinyear=1 if countryname=="Palau" & year >=1994

replace countryname = "Palestine" if countryname == "West Bank And Gaza" ///
  | countryname == "Gaza Strip (Palestine)" ///
  | countryname == "Gaza/West Bank" ///
  | countryname == "Gaza Strip" ///
  | countryname == "Palestinian Admin. Areas" ///
  | countryname == "Palestinian Admin Areas" ///
  | countryname == "Gaza/West Bank" ///
  | countryname == "PALESTINE" ///
  | countryname == "PALESTINIAN ADMIN. AREAS" ///
  | countryname == "WEST BANK AND GAZA" ///
  | countryname == "west bank and gaza" ///
  | countryname == "West Bank and Gaza"
****Not in Gleditsch****

replace countryname = "Panama" if countryname == "PANAMA" ///
  | countryname == "panama"
capture replace stateinyear=1 if countryname=="Panama" & year >=1903

*Papal state: not the same as Holy See
capture replace stateinyear=1 if countryname=="Papal States" & year >=1816 & year <=1870

replace countryname = "Papua New Guinea" if countryname == "Papau New Guinea" ///
  | countryname == "Png" ///
  | countryname == "Papau New Guinea" ///
  | countryname == "PAPUA N.GUINEA" ///
  | countryname == "PAPUA NEW GUINEA" ///
  | countryname == "PAPUA N.G." ///
  | countryname == "Papua-New Guinea" ///
  | countryname == "PAPUA NEW G" ///
  | countryname == "Papua N.Guinea"
capture replace stateinyear=1 if countryname=="Papua New Guinea" & year >=1975

replace countryname = "Paraguay" if countryname == "PARAGUAY"
capture replace stateinyear=1 if countryname=="Paraguay" & year >=1816

*Parma
capture replace stateinyear=1 if countryname=="Parma" & year >=1816 & year<=1861

replace countryname = "Peru" if countryname == "PERU" ///
  | countryname == "peru"
capture replace stateinyear=1 if countryname=="Peru" & year >=1824

replace countryname = "Philippines" if countryname == "PHILIPPINES" ///
  | countryname == "philippines" ///
  | countryname == "Philipines"
capture replace stateinyear=1 if countryname=="Philippines" & year >=1946

replace countryname = "Poland" if countryname == "POLAND" ///
  | countryname == "poland"
capture replace stateinyear=1 if countryname=="Poland" & year >=1918

replace countryname = "Portugal" if countryname == "PORTUGAL"
capture replace stateinyear=1 if countryname=="Portugal" & year >=1816

replace countryname = "Puerto Rico" if countryname == "Puertorico" ///
  | countryname == "PUERTO RICO"
****Not in Gleditsch****


*Q 

replace countryname = "Qatar" if countryname == "QATAR"
capture replace stateinyear=1 if countryname=="Qatar" & year >=1971


*R

replace countryname = "Reunion" if countryname=="RŽUnion" ///
  | countryname=="RéUnion" ///
  | countryname == "RÌ©Union" ///
  | countryname == "REUNION" ///
  | countryname == "Réunion"
****Not in Gleditsch****

replace countryname = "Rumania" if countryname == "Romania" ///
  | countryname == "ROMANIA" ///
  | countryname == "romania"
capture replace stateinyear=1 if countryname=="Rumania" & year >=1878

replace countryname = "Russia (Soviet Union)" if countryname == "Russia" ///
  | countryname == "Europeanrussia" ///
  | countryname == "Russian Federation" ///
  | countryname == "RUSSIA" ///
  | countryname == "RUSSIAN FEDERATION" ///
  | countryname == "Russian Federation" ///
  | countryname == "russian federation" ///
  | countryname == "Russian Federation" ///
  | countryname == "SOVIET UNION" ///
  | countryname == "U.S.S.R." ///
  | countryname == "USSR" ///
  | countryname == "Ussr" ///
  | countryname == "USSR (Russia)" ///
  | countryname == "USSR (Soviet Union)" ///
  | countryname == "RUSSIAN FED" ///
  | countryname == "Soviet Union"
capture replace stateinyear=1 if countryname=="Russia (Soviet Union)" & year >=1816

replace countryname = "Rwanda" if countryname == "rwanda" ///
  | countryname == "RWANDA"
capture replace stateinyear=1 if countryname=="Rwanda" & year >=1962


*S

replace countryname = "Saint Helena" if countryname == "St. Helena" ///
  | countryname == "ST. HELENA"
****Not in Gleditsch****

replace countryname = "Saint Kitts and Nevis" if countryname == "Saint Kitts And Nevis" ///
  | countryname == "St. Kitts & Nevis" ///
  | countryname == "St Kitts & Nevis" ///
  | countryname == "Saint Kitts And Nevis" ///
  | countryname == "St. Kitts And Nevis" ///
  | countryname == "St. Kitts-Nevis" ///
  | countryname == "St Kitts And Nevis" ///
  | countryname == "St Kitts-Nevis" ///
  | countryname == "St. Kitts/Nevis"  ///
  | countryname == "Saint Kitts and Nevis" ///
  | countryname == "Saints Kitts and Nevis" ///
  | countryname == "ST. KITTS AND NEVIS" ///
  | countryname == "St. Kitts and Nevis" ///
  | countryname == "St. Kitts and Nevis" ///
  | countryname == "St. Kitts/Nevis" ///
  | countryname == "ST. KITTS-NEVIS" ///
  | countryname == "ST.KITTS&NEVIS" ///
  | countryname == "St. Kitts & Nevis" ///
  | countryname == "ST. KITTS & NEVIS" ///
  | countryname == "ST KITT/NEV" ///
  | countryname == "Saint Kitts & Nevis"
capture replace microstateinyear=1 if countryname=="Saint Kitts and Nevis" & year >=1983

replace countryname = "Saint Lucia" if countryname == "St. Lucia" ///
  | countryname == "St Lucia" ///
  | countryname == "Saint Lucia" ///
  | countryname == "ST. LUCIA" ///
  | countryname == "ST.LUCIA" ///
  | countryname == "SAINT LUCIA" ///
  | countryname == "St. Lucia"
capture replace microstateinyear=1 if countryname=="Saint Lucia" & year >=1979

replace countryname = "Saint Maurice" if countryname == "St. Maurice" ///
  | countryname == "St Maurice" ///
  | countryname == "ST. MAURICE" ///
  | countryname == "St. Maurice"
****Not in Gleditsch****

replace countryname = "Saint Pierre and Miquelon" if countryname == "Saint Pierre And Miquelon" ///
  | countryname == "Saint Pierre & Miquelon" ///
  | countryname == "St. Pierre & Miquelon" ///
  | countryname == "St Pierre & Miquelon" ///
  | countryname == "Stpierreetmiquelon" ///
  | countryname == "St. Pierre Et Miquelon" ///
  | countryname == "St Pierre Et Miquelon" ///
  | countryname == "ST. PIERRE & MIQUELON" ///
  | countryname == "St. Pierre & Miquelon" ///
  | countryname == "St. Pierre-Miquelon"
****Not in Gleditsch****

replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent And The Grenadines" ///
  | countryname == "Saint Vincent And Grenadines" ///
  | countryname == "Saint Vincent & The Grenadines" ///
  | countryname == "Saint Vincent & Grenadines" ///
  | countryname == "St. Vincent & Grenadines" ///
  | countryname == "St. Vincent And Gr." ///
  | countryname == "St Vincent & Grenadines" ///
  | countryname == "St Vincent & The Grenadines" ///
  | countryname == "St. Vincent And Grenadines" ///
  | countryname == "St Vincent And Grenadines" ///
  | countryname == "St. Vincent And The Grenadines" ///
  | countryname == "St Vincent And The Grenadines" ///
  | countryname == "St. Vincent And Gr." ///
  | countryname == "St Vincent And Gr" ///
  | countryname == "Saint Vincent/Grenadines" ///
  | countryname == "St. Vincent" ///
  | countryname == "ST. VINCENT AND GR." ///
  | countryname == "St. Vincent & Grenadine"
  replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent and the Grenadines" ///
  | countryname == "St. Vincent" ///
  | countryname == "Saint Vincent & Grenadines" ///
  | countryname == "Saint Vincent and Grenadines" ///
  | countryname == "Saint Vincent and the Grenadines" ///
  | countryname == "St. Vincent & Gren." ///
  | countryname == "St. Vincent & Grenadines" ///
  | countryname == "St. Vincent and the Grenadines" ///
  | countryname == "St.Vincent & Grenadines" ///
  | countryname == "ST.VINCENT&GRE" ///
  | countryname == "St. Vincent & the Grenadines" ///
  | countryname == "ST. VINCENT AND THE GRENADINES" ///
  | countryname == "ST VINCENT" ///
  | countryname == "St. Vincent & The Grenadines"
capture replace microstateinyear=1 if countryname=="Saint Vincent and the Grenadines" & year >=1979

replace countryname = "Samoa/Western Samoa" if countryname == "Samoa" ///
  | countryname == "Western Samoa" ///
  | countryname == "SAMOA" ///
  | countryname == "samoa" ///
  | countryname == "Samoa (Western Samoa)" ///
  | countryname == "WESTN SAMOA" ///
  | countryname == "WESTERN SAMOA"
capture replace microstateinyear=1 if countryname=="Samoa/Western Samoa" & year >=1962
****Likely to be problems here if this is used with PLAID data******

replace countryname = "San Marino" if countryname == "SAN MARINO"
capture replace microstateinyear=1 if countryname=="San Marino" & year >=1816

replace countryname = "Sao Tome and Principe" if countryname == "Sao Tome & Principe" ///
  | countryname == "Sao Tome And Principe" ///
  | countryname == "Sao Tome" ///
  | countryname == "SãO Tomé And Principe" ///
  | countryname == " Sao Tome & Principe" ///
  | countryname == "S Tome  Prin." ///
  | countryname == "SAO TOME & PRINCIPE" ///
  | countryname == "Sao Tome and Principe" ///
  | countryname == "Sao Tome and Principe" ///
  | countryname == "SAO TOME AND PRINCIPE" ///
  | countryname == "São Tomé and Principe" ///
  | countryname == "Sao Tome & Principe" ///
  | countryname == "SAO TOME/PR" ///
  | countryname == "São Tomé & Príncipe"
capture replace microstateinyear=1 if countryname=="Sao Tome and Principe" & year >=1975
***This is a departure from Gleditsch, where the name is "São Tomé and Principe"***

replace countryname = "Saudi Arabia" if countryname == "Saudiarabia" ///
  | countryname == "saudi arabia" ///
  | countryname == "SAUDI ARABIA" ///
  | countryname == "SA'U ARABIA" ///
  | countryname == "Saudi Arabia"
capture replace stateinyear=1 if countryname=="Saudi Arabia" & year >=1932

*Saxony
capture replace stateinyear=1 if countryname=="Saxony" & year >=1816 & year <=1871

replace countryname = "Senegal" if countryname == "SENEGAL" ///
  | countryname == "senegal"
capture replace stateinyear=1 if countryname=="Senegal" & year >=1960

*Serbia
capture replace stateinyear=1 if countryname=="Serbia" & year >=1878 & year <=1915

replace countryname = "Seychelles" if countryname == "SEYCHELLES"
capture replace microstateinyear=1 if countryname=="Seychelles" & year >=1976

replace countryname = "Sierra Leone" if countryname == "Sierraleone" ///
  | countryname == "SIERRA LEONE" ///
  | countryname == "SIERRA LEO"
capture replace stateinyear=1 if countryname=="Sierra Leone" & year >=1961

replace countryname = "Singapore" if countryname == "SINGAPORE"
capture replace stateinyear=1 if countryname=="Singapore" & year >=1965

replace countryname = "Slovakia" if countryname == "Slovak Republic" ///
  | countryname == "SLOVAK REPUBLIC" ///
  | countryname == "slovak republic" ///
  | countryname == "SLOVAK REP" ///
  | countryname == "SLOVAKIA"
capture replace stateinyear=1 if countryname=="Slovakia" & year >=1993

replace countryname = "Slovenia" if countryname == "SLOVENIA"
capture replace stateinyear=1 if countryname=="Slovenia" & year >=1991

replace countryname = "Solomon Islands" if countryname == "SOLOMON IS." ///
  | countryname == "SOLOMON ISLANDS" ///
  | countryname == "SOLOMON IS"
capture replace stateinyear=1 if countryname=="Solomon Islands" & year >=1978

replace countryname = "Somalia" if countryname == "SOMALIA"
capture replace stateinyear=1 if countryname=="Somalia" & year >=1960

replace countryname = "South Africa" if countryname == "Southafrica" ///
  | countryname == "SOUTH AFRICA" ///
  | countryname == "south africa" ///
  | countryname == "Republic of South Africa" ///
  | countryname == "SO AFRICA" ///
  | countryname == "South Africa0"
capture replace stateinyear=1 if countryname=="South Africa" & year >=1910

replace countryname = "Spain" if countryname == "SPAIN"
capture replace stateinyear=1 if countryname=="Spain" & year >=1816

replace countryname = "Sri Lanka (Ceylon)" if countryname == "Sri Lanka" ///
  | countryname == "Srilanka" ///
  | countryname == "SRI LANKA" ///
  | countryname == "Sri Lanka, Ceylon" ///
  | countryname == "CEYLON" ///
  | countryname == "sri lanka"
capture replace stateinyear=1 if countryname=="Sri Lanka (Ceylon)" & year >=1948

replace countryname = "Sudan" if countryname == "SUDAN"
capture replace stateinyear=1 if countryname=="Sudan" & year >=1956

replace countryname = "Surinam" if countryname == "Suriname" ///
  | countryname == "SURINAME" ///
  | countryname == "Suriname, Dutch Guiana"
capture replace stateinyear=1 if countryname=="Surinam" & year >=1975

replace countryname = "Swaziland" if countryname == "SWAZILAND" ///
  | countryname == "swaziland"
capture replace stateinyear=1 if countryname=="Swaziland" & year >=1968

replace countryname = "Sweden" if countryname == "SWEDEN"
capture replace stateinyear=1 if countryname=="Sweden" & year >=1816

replace countryname = "Switzerland" if countryname == "SWITZERLAND"
capture replace stateinyear=1 if countryname=="Switzerland" & year >=1816

replace countryname = "Syria" if countryname == "Syrian Arab Republic" ///
  | countryname == "Syrian Arab Rep." ///
  | countryname == "Syrian Arab Rep" ///
  | countryname == "SYRIA" ///
  | countryname == "syria" ///
  | countryname == "Syrian Arab Rep." ///
  | countryname == "Syrian Arab Republic" ///
  | countryname == "Syrian Arab Republic" ///
  | countryname == "SYRIAN ARAB REPUBLIC"
capture replace stateinyear=1 if countryname=="Syria" & year >=1946


*T

replace countryname = "Taiwan" if countryname == "Taiwan, China" ///
  | countryname == "Chinese Taipei" ///
  | countryname == "Taiwan Province Of China" ///
  | countryname == "CHINESE TAIPEI" ///
  | countryname == "CHINA REP" ///
  | countryname == "TAIWAN"
capture replace stateinyear=1 if countryname=="Taiwan" & year >=1949

replace countryname = "Tajikistan" if countryname == "TAJIKISTAN" ///
  | countryname == "tajikistan" ///
  | countryname == "Tadzhikistan"
capture replace stateinyear=1 if countryname=="Tajikistan" & year >=1991

replace countryname = "Tanzania/Tanganyika" if countryname == "Tanzania" ///
  | countryname == "Tanzania United Republic Of" ///
  | countryname == "Tanzinia" ///
  | countryname == "United Republic Of Tanzania" ///
  | countryname == "Tanzania United Republic of" ///
  | countryname == "TANZANIA" ///
  | countryname == "tanzania" ///
  | countryname == "United Republic of Tanzania" ///
  | countryname == "UNITED REPUBLIC OF TANZANIA"
capture replace stateinyear=1 if countryname=="Tanzania/Tanganyika" & year >=1961

replace countryname = "Thailand" if countryname == "THAILAND" ///
  | countryname == "thailand"
capture replace stateinyear=1 if countryname=="Thailand" & year >=1816

  **Tibet -- independent until 1951
capture replace stateinyear=1 if countryname=="Tibet" & year >=1913 & year <=1950

replace countryname = "Togo" if countryname == "TOGO" ///
  | countryname == "togo"
capture replace stateinyear=1 if countryname=="Togo" & year >=1960

replace countryname = "Tokelau" if countryname == "TOKELAU" ///
  | countryname == "Tokelau Islands"
****Not in Gleditsch****

replace countryname = "Tonga" if countryname == "TONGA"
capture replace microstateinyear=1 if countryname=="Tonga" & year >=1970

*Transvaal
capture replace stateinyear=1 if countryname=="Transvaal" & year >=1852 & year <=1910

replace countryname = "Trinidad and Tobago" if countryname == "Trinidad And Tobago" ///
  | countryname == "Trinidad & Tobago" ///
  | countryname == "Trinidad" ///
  | countryname == "Trinidad/Tobago" ///
  | countryname == "TRINIDAD & TOBAGO" ///
  | countryname == "TRINIDAD & TOBAGO" ///
  | countryname == "Trinidad &Tobago" ///
  | countryname == "TRINIDAD AND TOBAGO" ///
  | countryname == "Trinidad and Tobago" ///
  | countryname == "Trinidad and Tobago" ///
  | countryname == "TRINIDAD&TOBAGO" ///
  | countryname == "Trinidad & Tobago" ///
  | countryname == "Trinidad" ///
  | countryname == "TRINIDAD" ///
  | countryname == "Trinidad&Tobago"
capture replace stateinyear=1 if countryname=="Trinidad and Tobago" & year >=1962

replace countryname = "Tunisia" if countryname == "Tunisa" ///
  | countryname == "TUNISIA" ///
  | countryname == "tunisia"
capture replace stateinyear=1 if countryname=="Tunisia" & year >=1816 & year <=1881
capture replace stateinyear=1 if countryname=="Tunisia" & year >=1956

replace countryname = "Turkey/Ottoman Empire" if countryname == "Turkey" ///
  | countryname == "Tukey" ///
  | countryname == "Turkey" ///
  | countryname == "turkey" ///
  | countryname == "TURKEY"
capture replace stateinyear=1 if countryname=="Turkey/Ottoman Empire" & year >=1816

replace countryname = "Turkmenistan" if countryname == "turkmenistan" ///
  | countryname == "TURKMENISTAN" ///
  | countryname == "Turkmenia" ///
  | countryname == "TURKMENSTAN" ///
  | countryname == "Turkemenistan"
capture replace stateinyear=1 if countryname=="Turkmenistan" & year >=1991

replace countryname = "Turks and Caicos Islands" if countryname == "Turks And Caicos Islands" ///
  | countryname == "Turks & Caicos Islands" ///
  | countryname == "Turks/Caicos Islands" ///
  | countryname == "Turks & Caicos Islands" ///
  | countryname == "TURKS & CAICOS ISLANDS" ///
  | countryname == "Turks and Caicos Islands" ///
  | countryname == "TURKS AND CAICOS ISLANDS" ///
  | countryname == "Turks & Caicos Islands"
****Not in Gleditsch****

*Tuscany
capture replace stateinyear=1 if countryname=="Tuscany" & year >=1816 & year<=1861

*Two Sicilies
capture replace stateinyear=1 if countryname=="Two Sicilies" & year >=1816 & year <=1861

replace countryname = "Tuvalu" if countryname == "TUVALU"
capture replace microstateinyear=1 if countryname=="Tuvalu" & year >=1978


*U 

replace countryname = "Uganda" if countryname == "UGANDA" ///
  | countryname == "uganda"
capture replace stateinyear=1 if countryname=="Uganda" & year >=1962

replace countryname = "Ukraine" if countryname == "UKRAINE" ///
  | countryname == "ukraine"
capture replace stateinyear=1 if countryname=="Ukraine" & year >=1991

replace countryname = "United Arab Emirates" if countryname == "Uae" ///
  | countryname == "UNITED ARAB E." ///
  | countryname == "United Arab Emir." ///
  | countryname == "UNITED ARAB EMIRATES" ///
  | countryname == "UAE" ///
  | countryname == "U. ARAB EMIRATES" ///
  | countryname == "UA EMIRATES" ///
  | countryname == "United Arab E."
capture replace stateinyear=1 if countryname=="United Arab Emirates" & year >=1971

replace countryname = "United Kingdom" if countryname == "U.K." ///
  | countryname == "United Kingdom of Great Britain and Northern Ireland" ///
  | countryname == "UK" ///
  | countryname == "UNITED KINGDOM"
capture replace stateinyear=1 if countryname=="United Kingdom" & year >=1816

*United Provinces of Central America
capture replace stateinyear=1 if countryname=="United Provinces of Central America" & year >=1823 & year<=1839

replace countryname = "United States of America" if countryname == "United States Of America" ///
  | countryname == "United States" ///
  | countryname == "Usa" ///
  | countryname == "U.S.A." ///
  | countryname == "United States of America" ///
  | countryname == "USA" ///
  | countryname == "US" ///
  | countryname == "UNITED STATES"
capture replace stateinyear=1 if countryname=="United States of America" & year >=1816

replace countryname = "Uruguay" if countryname == "URUGUAY"
capture replace stateinyear=1 if countryname=="Uruguay" & year >=1830

replace countryname = "Uzbekistan" if countryname == "UZBEKISTAN" ///
  | countryname == "uzbekistan"
capture replace stateinyear=1 if countryname=="Uzbekistan" & year >=1991


*V

replace countryname = "Vanuatu" if countryname == "Vanutatu" ///
  | countryname == "VANUATU" ///
  | countryname == "Vanuatau"
capture replace microstateinyear=1 if countryname=="Vanuatu" & year >=1980

replace countryname = "Venezuela" if countryname == "Venezuela Rep?blica Bolivariana de" ///
  | countryname == "Venezuela, Rb" ///
  | countryname == "Venezuela Rep?blica Bolivariana de" ///
  | countryname == "VENEZUELA" ///
  | countryname == "venezuela" ///
  | countryname == "Venezuela, RB" ///
  | countryname == "Venezuela,"
capture replace stateinyear=1 if countryname=="Venezuela" & year >=1829

*Vietnam (Annam/Cochin China/Tonkin)
capture replace stateinyear=1 if countryname=="Vietnam (Annam/Cochin China/Tonkin)" & year >=1816 & year<=1893

replace countryname = "Vietnam, Democratic Republic of" if countryname == "Vietnam" ///
  | countryname == "Vietnam, Democratic Republic Of" ///
  | countryname == "Viet nam" ///
  | countryname == "VIET NAM" ///
  | countryname == "VIETNAM" ///
  | countryname == "vietnam" ///
  | countryname =="Viet Nam" ///
  | countryname =="Vietnam North" ///
  | countryname =="Vietnam, North" ///
  | countryname =="Democratic Republic of Vietnam" ///
  | countryname =="Vietnam, N." ///
  | countryname =="Vietnam, Socialist Republic of" ///
  | countryname =="North Vietnam" ///
  | countryname =="VIETNAM DR" ///
  | countryname =="Democratic Republic Of Vietnam"
capture replace stateinyear=1 if countryname=="Vietnam, Democratic Republic of" & year >=1954

*Vietnam, Republic of
replace countryname = "Vietnam, Republic of" if countryname =="Vietnam, South" ///
  | countryname =="Vietnam South" ///
  | countryname =="Vietnam, S." ///
  | countryname =="Vietnam, Republic of (South Vietnam)" ///
  | countryname =="VIETNAM, S." ///
  | countryname =="South Vietnam" ///
  | countryname =="SOUTH VIETNAM" ///
  | countryname =="VIETNAM REP" ///
  | countryname =="Republic Of Vietnam"
capture replace stateinyear=1 if countryname=="Vietnam, Republic of" & year >=1954 & year <=1975

replace countryname = "Virgin Islands" if countryname == "Virginislands" ///
  | countryname == "United States Virgin Islands" ///
  | countryname == "Virgin Islands (U.S.)" ///
  | countryname == "Virgin Islands (Usa)" ///
  | countryname == "US VIRGIN ISLANDS" ///
  | countryname == "VIRGIN ISLANDS"
****Not in Gleditsch****


*W

replace countryname = "Wallis and Futuna" if countryname == "Wallis And Futuna" ///
  | countryname == "Wallis & Futuna" ///
  | countryname == "WALLIS & FUTUNA" ///
  | countryname == "Wallis and Futuna Islands" ///
  | countryname == "Wallis and Futuna Is." ///
  | countryname == "WALLIS & FUTUNA" ///
  | countryname == "Wallis & Futuna" ///
  | countryname == "Wallis-Futuna"
****Not in Gleditsch****

*Württemberg
replace countryname="Württemberg" if countryname=="Wuerttemburg"
capture replace stateinyear=1 if countryname=="Württemberg" & year >=1816 & year <=1871


*X


*Y

replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen (Arab Republic Of Yemen)" ///
  | countryname == "Yemen North" ///
  | countryname == "Yemen (North)" ///
  | countryname == "Yemen, North" ///
  | countryname == "Yemen (Arabl Republic Of Yemen)" ///
  | countryname == "North Yemen" ///
  | countryname == "Yemen, Arab Rep." ///
  | countryname == "Yemen" ///
  | countryname == "Yemen, Republic Of" ///
  | countryname == "Yemen Republic Of" ///
  | countryname == "Republic Of Yemen" ///
  | countryname == "Yemenunited" ///
  | countryname == "Yemen, Rep." ///
  | countryname == "Yemen, Rep" ///
  | countryname == "REPUBLIC OF YEMEN" ///
  | countryname == "Republic of Yemen" ///
  | countryname == "NORTH YEMEN" ///
  | countryname == "Yemen Arab Republic (North Yemen)" ///
  | countryname == "Yemen, N." ///
  | countryname == "YEMEN" ///
  | countryname == "Yemen, Rep." ///
  | countryname == "Yemen Arab Republic" ///
  | countryname == "North Yemen" ///
  | countryname == "Yemen,(Arab Republic of Yemen)" ///
  | countryname == "YEMEN ARAB REP." ///
  | countryname == "YEMEN AR" ///
  | countryname == "YEMEN REP" ///
  | countryname == "Yemen,(Arab Republic Of Yemen)"
capture replace stateinyear=1 if countryname=="Yemen (Arab Republic of Yemen)" & year >=1918

replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, People's Republic Of" ///
  | countryname == "Yemen South" ///
  | countryname == "Yemen, Dem." ///
  | countryname == "Yemen, Dem" ///
  | countryname == "South Yemen" ///
  | countryname == "Yemen PDR" ///
  | countryname == "Yemen PDR." ///
  | countryname == "Yemen P.D.R." ///
  | countryname == "Yemen Dem" ///
  | countryname == "Yemen Dem." ///
  | countryname == "SOUTH YEMEN" ///
  | countryname == "Yemen PDR" ///
  | countryname == "Yemen People's Republic" ///
  | countryname == "South Yemen" ///
  | countryname == "YEMEN, DEM.                                                                " ///
  | countryname == "YEMEN, DEM." ///
  | countryname == "Yemen, South" ///
  | countryname == "Yemen, Dem.                                                                " ///
  | countryname == "Yemen, S." ///
  | countryname == "Democratic Yemen" ///
  | countryname == "SouthYemen" ///
  | countryname == "Yemen, People's Democratic Republic of (South)" ///
  | countryname == "YEMEN PDR" ///
  | countryname == "YEMEN PEOP. REP."
replace countryname = "Yemen, People's Republic of" if countryname == "South Yemen, PDRY" ///
  | countryname == "People'S Republic Of Yemen"
capture replace stateinyear=1 if countryname=="Yemen, People's Republic of" & year >=1967 & year <= 1990
*From WDI-"Footnote: Data for the Republic of Yemen refer to that countryname from 1990 onward; data for previous years refer to aggregated data of the former People's Democratic Republic of Yemen and the former Yemen Arab Republic unless otherwise noted."

  ** Note, as of Aug 3, 2009, the countryname is "Serbia (Yugoslavia)" rather than "Yugoslavia (Serbia)"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Sts Ex-Yugoslavia Unsp." ///
  | countryname == "Yugoslavia Sfr" ///
  | countryname == "Yugoslavia Sfr." ///
  | countryname == "Yugoslavia S.f.r." ///
  | countryname == "Yugoslavia" ///
  | countryname == "Yogoslavia" ///
  | countryname == "Yugoslavia Fed. Rep. Of" ///
  | countryname == "Yugoslavia Fed Rep Of" ///
  | countryname == "Yugoslavia, Fed. Rep." ///
  | countryname == "Yugoslavia, Fed Rep" ///
  | countryname == "Yugoslavia, Former" ///
  | countryname == "Yugoslavia (Former)" ///
  | countryname == "Serbia & Montenegro" ///
  | countryname == "Serbia And Montenegro" ///
  | countryname == "Serbia And Montenegro, Former" ///
  | countryname == "Serbia" ///
  | countryname == "Yugoslavia, Fr (Serbia/Montenegro)" ///
  | countryname == "Serbiamontenegro" ///
  | countryname == "Serbia Montenegro" ///
  | countryname == "Fry-Serbia & Montenegro"
  replace countryname = "Yugoslavia (Serbia)" if countryname == "Fry-Serbia And Montenegro" ///
  | countryname == "FRY-SERBIA & MONTENEGRO" ///
  | countryname == "SERBIA & MONTENEGRO" ///
  | countryname == "Serbia & Montenegro" ///
  | countryname == "STS EX-YUGOSLAVIA UNSP." ///
  | countryname == "Yogoslavia" ///
  | countryname == "Yugoslavia Fed. Rep. of" ///
  | countryname == "Yugoslavia post" ///
  | countryname == "YUGOSLAVIA" ///
  | countryname == "Yugoslavia†" ///
  | countryname == "Yugoslavia, Federal Republic of" ///
  | countryname == "Serbia and Montenegro" ///
  | countryname == "SERBIA" ///
  | countryname == "Yugoslavia (Serbia and Montenegro)" ///
  | countryname == "Serbia-Montenegro" ///
  | countryname == "Yugoslavia/Serbia/Montenegro" ///
  | countryname == "Yugoslavia (Serbia)" ///
  | countryname == "Yugoslavia, Federal Republic Of" ///
  | countryname == "Yugoslavia/Serbia" ///
  | countryname == "SERBIA/MONT" ///
  | countryname == "Yugoslavia Post"
capture replace stateinyear=1 if countryname=="Yugoslavia (Serbia)" & year >=1918


*Z

replace countryname = "Zambia" if countryname == "ZAMBIA" ///
  | countryname == "zambia"
capture replace stateinyear=1 if countryname=="Zambia" & year >=1964

*Zanzibar
capture replace stateinyear=1 if countryname=="Zanzibar" & year >=1963 & year <=1964

replace countryname = "Zimbabwe (Rhodesia)" if countryname == "Zimbabwe" ///
  | countryname == "Zimbabwe (Rhodesia)" ///
  | countryname == "ZIMBABWE" ///
  | countryname == "Rhodesia" ///
  | countryname == "Zimbabwe, Rhodesia" ///
  | countryname == "RHODESIA" ///
  | countryname == "zimbabwe"
capture replace stateinyear=1 if countryname=="Zimbabwe (Rhodesia)" & year >=1965




/* This section adds Gleditsch's country codes (3 letter and numeric)
   Added on August 27, 2007 by Rich Nielsen, rnielsen@fas.harvard.edu */

capture gen countrycode = "missing"
replace countrycode = "USA" if countryname=="United States of America"
replace countrycode = "CAN" if countryname=="Canada"
replace countrycode = "BHM" if countryname=="Bahamas"
replace countrycode = "CUB" if countryname=="Cuba"
replace countrycode = "HAI" if countryname=="Haiti"
replace countrycode = "HAI" if countryname=="Haiti"
replace countrycode = "DOM" if countryname=="Dominican Republic"
replace countrycode = "JAM" if countryname=="Jamaica"
replace countrycode = "TRI" if countryname=="Trinidad and Tobago"
replace countrycode = "BAR" if countryname=="Barbados"
replace countrycode = "MEX" if countryname=="Mexico"
replace countrycode = "BLZ" if countryname=="Belize"
replace countrycode = "UPC" if countryname=="United Provinces of Central America"
replace countrycode = "GUA" if countryname=="Guatemala"
replace countrycode = "HON" if countryname=="Honduras"
replace countrycode = "SAL" if countryname=="El Salvador"
replace countrycode = "NIC" if countryname=="Nicaragua"
replace countrycode = "COS" if countryname=="Costa Rica"
replace countrycode = "PAN" if countryname=="Panama"
replace countrycode = "GCL" if countryname=="Great Colombia"
replace countrycode = "COL" if countryname=="Colombia"
replace countrycode = "VEN" if countryname=="Venezuela"
replace countrycode = "GUY" if countryname=="Guyana"
replace countrycode = "SUR" if countryname=="Surinam"
replace countrycode = "ECU" if countryname=="Ecuador"
replace countrycode = "PER" if countryname=="Peru"
replace countrycode = "BRA" if countryname=="Brazil"
replace countrycode = "BOL" if countryname=="Bolivia"
replace countrycode = "PAR" if countryname=="Paraguay"
replace countrycode = "CHL" if countryname=="Chile"
replace countrycode = "ARG" if countryname=="Argentina"
replace countrycode = "URU" if countryname=="Uruguay"
replace countrycode = "UKG" if countryname=="United Kingdom"
replace countrycode = "IRE" if countryname=="Ireland"
replace countrycode = "NTH" if countryname=="Netherlands"
replace countrycode = "BEL" if countryname=="Belgium"
replace countrycode = "LUX" if countryname=="Luxembourg"
replace countrycode = "FRN" if countryname=="France"
replace countrycode = "SWZ" if countryname=="Switzerland"
replace countrycode = "SPN" if countryname=="Spain"
replace countrycode = "POR" if countryname=="Portugal"
replace countrycode = "HAN" if countryname=="Hanover"
replace countrycode = "BAV" if countryname=="Bavaria"
replace countrycode = "GMY" if countryname=="Germany (Prussia)"
replace countrycode = "GFR" if countryname=="German Federal Republic"
replace countrycode = "GDR" if countryname=="German Democratic Republic"
replace countrycode = "BAD" if countryname=="Baden"
replace countrycode = "SAX" if countryname=="Saxony"
replace countrycode = "WRT" if countryname=="Württemberg"
replace countrycode = "HSE" if countryname=="Hesse-Kassel (Electoral)"
replace countrycode = "HSD" if countryname=="Hesse-Darmstadt (Ducal)"
replace countrycode = "MEC" if countryname=="Mecklenburg-Schwerin"
replace countrycode = "POL" if countryname=="Poland"
replace countrycode = "AUH" if countryname=="Austria-Hungary"
replace countrycode = "AUS" if countryname=="Austria"
replace countrycode = "HUN" if countryname=="Hungary"
replace countrycode = "CZE" if countryname=="Czechoslovakia"
replace countrycode = "CZR" if countryname=="Czech Republic"
replace countrycode = "SLO" if countryname=="Slovakia"
replace countrycode = "ITA" if countryname=="Italy/Sardinia"
replace countrycode = "PAP" if countryname=="Papal States"
replace countrycode = "SIC" if countryname=="Two Sicilies"
replace countrycode = "MOD" if countryname=="Modena"
replace countrycode = "PMA" if countryname=="Parma"
replace countrycode = "TUS" if countryname=="Tuscany"
replace countrycode = "MLT" if countryname=="Malta"
replace countrycode = "ALB" if countryname=="Albania"
replace countrycode = "SER" if countryname=="Serbia"
replace countrycode = "MNG" if countryname=="Montenegro"
replace countrycode = "MNG" if countryname=="Montenegro"
replace countrycode = "MAC" if countryname=="Macedonia (Former Yugoslav Republic of)"
replace countrycode = "CRO" if countryname=="Croatia"
replace countrycode = "SER" if countryname=="Serbia (Yugoslavia)"
*replace countrycode = "YUG" if countryname=="Yugoslavia (Serbia)"  ** Changed on Aug 3, 2009, to match Gleditsch's updated list
replace countrycode = "BOS" if countryname=="Bosnia-Herzegovina"
replace countrycode = "KOS" if countryname=="Kosovo"
replace countrycode = "SLV" if countryname=="Slovenia"
replace countrycode = "GRC" if countryname=="Greece"
replace countrycode = "CYP" if countryname=="Cyprus"
replace countrycode = "BUL" if countryname=="Bulgaria"
replace countrycode = "MLD" if countryname=="Moldova"
replace countrycode = "RUM" if countryname=="Rumania"
replace countrycode = "RUS" if countryname=="Russia (Soviet Union)"
replace countrycode = "EST" if countryname=="Estonia"
replace countrycode = "EST" if countryname=="Estonia"
replace countrycode = "LAT" if countryname=="Latvia"
replace countrycode = "LAT" if countryname=="Latvia"
replace countrycode = "LIT" if countryname=="Lithuania"
replace countrycode = "LIT" if countryname=="Lithuania"
replace countrycode = "UKR" if countryname=="Ukraine"
replace countrycode = "BLR" if countryname=="Belarus (Byelorussia)"
replace countrycode = "ARM" if countryname=="Armenia"
replace countrycode = "GRG" if countryname=="Georgia"
replace countrycode = "AZE" if countryname=="Azerbaijan"
replace countrycode = "FIN" if countryname=="Finland"
replace countrycode = "SWD" if countryname=="Sweden"
replace countrycode = "NOR" if countryname=="Norway"
replace countrycode = "DEN" if countryname=="Denmark"
replace countrycode = "ICE" if countryname=="Iceland"
replace countrycode = "CAP" if countryname=="Cape Verde"
replace countrycode = "GNB" if countryname=="Guinea-Bissau"
replace countrycode = "EQG" if countryname=="Equatorial Guinea"
replace countrycode = "GAM" if countryname=="Gambia"
replace countrycode = "MLI" if countryname=="Mali"
replace countrycode = "SEN" if countryname=="Senegal"
replace countrycode = "BEN" if countryname=="Benin"
replace countrycode = "MAA" if countryname=="Mauritania"
replace countrycode = "NIR" if countryname=="Niger"
replace countrycode = "CDI" if countryname=="Cote D'Ivoire"
replace countrycode = "GUI" if countryname=="Guinea"
replace countrycode = "BFO" if countryname=="Burkina Faso (Upper Volta)"
replace countrycode = "LBR" if countryname=="Liberia"
replace countrycode = "SIE" if countryname=="Sierra Leone"
replace countrycode = "GHA" if countryname=="Ghana"
replace countrycode = "TOG" if countryname=="Togo"
replace countrycode = "CAO" if countryname=="Cameroon"
replace countrycode = "NIG" if countryname=="Nigeria"
replace countrycode = "GAB" if countryname=="Gabon"
replace countrycode = "CEN" if countryname=="Central African Republic"
replace countrycode = "CHA" if countryname=="Chad"
replace countrycode = "CON" if countryname=="Congo"
replace countrycode = "DRC" if countryname=="Congo, Democratic Republic of (Zaire)"
replace countrycode = "UGA" if countryname=="Uganda"
replace countrycode = "KEN" if countryname=="Kenya"
replace countrycode = "TAZ" if countryname=="Tanzania/Tanganyika"
replace countrycode = "ZAN" if countryname=="Zanzibar"
replace countrycode = "BUI" if countryname=="Burundi"
replace countrycode = "RWA" if countryname=="Rwanda"
replace countrycode = "SOM" if countryname=="Somalia"
replace countrycode = "DJI" if countryname=="Djibouti"
replace countrycode = "ETH" if countryname=="Ethiopia"
replace countrycode = "ERI" if countryname=="Eritrea"
replace countrycode = "ANG" if countryname=="Angola"
replace countrycode = "MZM" if countryname=="Mozambique"
replace countrycode = "ZAM" if countryname=="Zambia"
replace countrycode = "ZIM" if countryname=="Zimbabwe (Rhodesia)"
replace countrycode = "MAW" if countryname=="Malawi"
replace countrycode = "SAF" if countryname=="South Africa"
replace countrycode = "TRA" if countryname=="Transvaal"
replace countrycode = "OFS" if countryname=="Orange Free State"
replace countrycode = "NAM" if countryname=="Namibia"
replace countrycode = "LES" if countryname=="Lesotho"
replace countrycode = "BOT" if countryname=="Botswana"
replace countrycode = "SWA" if countryname=="Swaziland"
replace countrycode = "MAG" if countryname=="Madagascar (Malagasy)"
replace countrycode = "MAG" if countryname=="Madagascar (Malagasy)"
replace countrycode = "COM" if countryname=="Comoros"
replace countrycode = "MAS" if countryname=="Mauritius"
replace countrycode = "MOR" if countryname=="Morocco"
replace countrycode = "MOR" if countryname=="Morocco"
replace countrycode = "ALG" if countryname=="Algeria"
replace countrycode = "ALG" if countryname=="Algeria"
replace countrycode = "TUN" if countryname=="Tunisia"
replace countrycode = "TUN" if countryname=="Tunisia"
replace countrycode = "LIB" if countryname=="Libya"
replace countrycode = "LIB" if countryname=="Libya"
replace countrycode = "SUD" if countryname=="Sudan"
replace countrycode = "IRN" if countryname=="Iran (Persia)"
replace countrycode = "TUR" if countryname=="Turkey/Ottoman Empire"
replace countrycode = "IRQ" if countryname=="Iraq"
replace countrycode = "EGY" if countryname=="Egypt"
replace countrycode = "EGY" if countryname=="Egypt"
replace countrycode = "SYR" if countryname=="Syria"
replace countrycode = "LEB" if countryname=="Lebanon"
replace countrycode = "JOR" if countryname=="Jordan"
replace countrycode = "ISR" if countryname=="Israel"
replace countrycode = "SAU" if countryname=="Saudi Arabia"
replace countrycode = "YEM" if countryname=="Yemen (Arab Republic of Yemen)"
replace countrycode = "YPR" if countryname=="Yemen, People's Republic of"
replace countrycode = "KUW" if countryname=="Kuwait"
replace countrycode = "BAH" if countryname=="Bahrain"
replace countrycode = "QAT" if countryname=="Qatar"
replace countrycode = "UAE" if countryname=="United Arab Emirates"
replace countrycode = "OMA" if countryname=="Oman"
replace countrycode = "AFG" if countryname=="Afghanistan"
replace countrycode = "AFG" if countryname=="Afghanistan"
replace countrycode = "TKM" if countryname=="Turkmenistan"
replace countrycode = "TAJ" if countryname=="Tajikistan"
replace countrycode = "KYR" if countryname=="Kyrgyz Republic"
replace countrycode = "UZB" if countryname=="Uzbekistan"
replace countrycode = "KZK" if countryname=="Kazakhstan"
replace countrycode = "CHN" if countryname=="China"
replace countrycode = "TBT" if countryname=="Tibet"
replace countrycode = "MON" if countryname=="Mongolia"
replace countrycode = "TAW" if countryname=="Taiwan"
replace countrycode = "KOR" if countryname=="Korea"
replace countrycode = "PRK" if countryname=="Korea, People's Republic of"
replace countrycode = "ROK" if countryname=="Korea, Republic of"
replace countrycode = "JPN" if countryname=="Japan"
replace countrycode = "IND" if countryname=="India"
replace countrycode = "BHU" if countryname=="Bhutan"
replace countrycode = "PAK" if countryname=="Pakistan"
replace countrycode = "BNG" if countryname=="Bangladesh"
replace countrycode = "MYA" if countryname=="Myanmar (Burma)"
replace countrycode = "MYA" if countryname=="Myanmar (Burma)"
replace countrycode = "SRI" if countryname=="Sri Lanka (Ceylon)"
replace countrycode = "MAD" if countryname=="Maldives"
replace countrycode = "NEP" if countryname=="Nepal"
replace countrycode = "THI" if countryname=="Thailand"
replace countrycode = "CAM" if countryname=="Cambodia (Kampuchea)"
replace countrycode = "LAO" if countryname=="Laos"
replace countrycode = "VNM" if countryname=="Vietnam (Annam/Cochin China/Tonkin)"
replace countrycode = "DRV" if countryname=="Vietnam, Democratic Republic of"
replace countrycode = "RVN" if countryname=="Vietnam, Republic of"
replace countrycode = "MAL" if countryname=="Malaysia"
replace countrycode = "SIN" if countryname=="Singapore"
replace countrycode = "BRU" if countryname=="Brunei"
replace countrycode = "PHI" if countryname=="Philippines"
replace countrycode = "INS" if countryname=="Indonesia"
replace countrycode = "ETM" if countryname=="East Timor"
replace countrycode = "AUL" if countryname=="Australia"
replace countrycode = "PNG" if countryname=="Papua New Guinea"
replace countrycode = "NEW" if countryname=="New Zealand"
replace countrycode = "SOL" if countryname=="Solomon Islands"
replace countrycode = "FJI" if countryname=="Fiji"
 
/* Gleditsch's numeric country codes */
capture gen countrynumcode =.
replace countrynumcode =2 if countryname=="United States of America"
replace countrynumcode =20 if countryname=="Canada"
replace countrynumcode =31 if countryname=="Bahamas"
replace countrynumcode =40 if countryname=="Cuba"
replace countrynumcode =41 if countryname=="Haiti"
replace countrynumcode =41 if countryname=="Haiti"
replace countrynumcode =42 if countryname=="Dominican Republic"
replace countrynumcode =51 if countryname=="Jamaica"
replace countrynumcode =52 if countryname=="Trinidad and Tobago"
replace countrynumcode =53 if countryname=="Barbados"
replace countrynumcode =70 if countryname=="Mexico"
replace countrynumcode =80 if countryname=="Belize"
replace countrynumcode =89 if countryname=="United Provinces of Central America"
replace countrynumcode =90 if countryname=="Guatemala"
replace countrynumcode =91 if countryname=="Honduras"
replace countrynumcode =92 if countryname=="El Salvador"
replace countrynumcode =93 if countryname=="Nicaragua"
replace countrynumcode =94 if countryname=="Costa Rica"
replace countrynumcode =95 if countryname=="Panama"
replace countrynumcode =99 if countryname=="Great Colombia"
replace countrynumcode =100 if countryname=="Colombia"
replace countrynumcode =101 if countryname=="Venezuela"
replace countrynumcode =110 if countryname=="Guyana"
replace countrynumcode =115 if countryname=="Surinam"
replace countrynumcode =130 if countryname=="Ecuador"
replace countrynumcode =135 if countryname=="Peru"
replace countrynumcode =140 if countryname=="Brazil"
replace countrynumcode =145 if countryname=="Bolivia"
replace countrynumcode =150 if countryname=="Paraguay"
replace countrynumcode =155 if countryname=="Chile"
replace countrynumcode =160 if countryname=="Argentina"
replace countrynumcode =165 if countryname=="Uruguay"
replace countrynumcode =200 if countryname=="United Kingdom"
replace countrynumcode =205 if countryname=="Ireland"
replace countrynumcode =210 if countryname=="Netherlands"
replace countrynumcode =211 if countryname=="Belgium"
replace countrynumcode =212 if countryname=="Luxembourg"
replace countrynumcode =220 if countryname=="France"
replace countrynumcode =225 if countryname=="Switzerland"
replace countrynumcode =230 if countryname=="Spain"
replace countrynumcode =235 if countryname=="Portugal"
replace countrynumcode =240 if countryname=="Hanover"
replace countrynumcode =245 if countryname=="Bavaria"
replace countrynumcode =255 if countryname=="Germany (Prussia)"
replace countrynumcode =260 if countryname=="German Federal Republic"
replace countrynumcode =265 if countryname=="German Democratic Republic"
replace countrynumcode =267 if countryname=="Baden"
replace countrynumcode =269 if countryname=="Saxony"
replace countrynumcode =271 if countryname=="Württemberg"
replace countrynumcode =273 if countryname=="Hesse-Kassel (Electoral)"
replace countrynumcode =275 if countryname=="Hesse-Darmstadt (Ducal)"
replace countrynumcode =280 if countryname=="Mecklenburg-Schwerin"
replace countrynumcode =290 if countryname=="Poland"
replace countrynumcode =300 if countryname=="Austria-Hungary"
replace countrynumcode =305 if countryname=="Austria"
replace countrynumcode =310 if countryname=="Hungary"
replace countrynumcode =315 if countryname=="Czechoslovakia"
replace countrynumcode =316 if countryname=="Czech Republic"
replace countrynumcode =317 if countryname=="Slovakia"
replace countrynumcode =325 if countryname=="Italy/Sardinia"
replace countrynumcode =327 if countryname=="Papal States"
replace countrynumcode =329 if countryname=="Two Sicilies"
replace countrynumcode =332 if countryname=="Modena"
replace countrynumcode =335 if countryname=="Parma"
replace countrynumcode =337 if countryname=="Tuscany"
replace countrynumcode =338 if countryname=="Malta"
replace countrynumcode =339 if countryname=="Albania"
replace countrynumcode =340 if countryname=="Serbia"
replace countrynumcode =341 if countryname=="Montenegro"
replace countrynumcode =341 if countryname=="Montenegro"
replace countrynumcode =343 if countryname=="Macedonia (Former Yugoslav Republic of)"
replace countrynumcode =344 if countryname=="Croatia"
replace countrynumcode =345 if countryname=="Serbia (Yugoslavia)"
*replace countrynumcode =345 if countryname=="Yugoslavia (Serbia)"  ** Changed on Aug 3, 2009, to match Gleditsch's updates
replace countrynumcode =346 if countryname=="Bosnia-Herzegovina"
replace countrynumcode =347 if countryname=="Kosovo"
replace countrynumcode =349 if countryname=="Slovenia"
replace countrynumcode =350 if countryname=="Greece"
replace countrynumcode =352 if countryname=="Cyprus"
replace countrynumcode =355 if countryname=="Bulgaria"
replace countrynumcode =359 if countryname=="Moldova"
replace countrynumcode =360 if countryname=="Rumania"
replace countrynumcode =365 if countryname=="Russia (Soviet Union)"
replace countrynumcode =366 if countryname=="Estonia"
replace countrynumcode =366 if countryname=="Estonia"
replace countrynumcode =367 if countryname=="Latvia"
replace countrynumcode =367 if countryname=="Latvia"
replace countrynumcode =368 if countryname=="Lithuania"
replace countrynumcode =368 if countryname=="Lithuania"
replace countrynumcode =369 if countryname=="Ukraine"
replace countrynumcode =370 if countryname=="Belarus (Byelorussia)"
replace countrynumcode =371 if countryname=="Armenia"
replace countrynumcode =372 if countryname=="Georgia"
replace countrynumcode =373 if countryname=="Azerbaijan"
replace countrynumcode =375 if countryname=="Finland"
replace countrynumcode =380 if countryname=="Sweden"
replace countrynumcode =385 if countryname=="Norway"
replace countrynumcode =390 if countryname=="Denmark"
replace countrynumcode =395 if countryname=="Iceland"
replace countrynumcode =402 if countryname=="Cape Verde"
replace countrynumcode =404 if countryname=="Guinea-Bissau"
replace countrynumcode =411 if countryname=="Equatorial Guinea"
replace countrynumcode =420 if countryname=="Gambia"
replace countrynumcode =432 if countryname=="Mali"
replace countrynumcode =433 if countryname=="Senegal"
replace countrynumcode =434 if countryname=="Benin"
replace countrynumcode =435 if countryname=="Mauritania"
replace countrynumcode =436 if countryname=="Niger"
replace countrynumcode =437 if countryname=="Cote D'Ivoire"
replace countrynumcode =438 if countryname=="Guinea"
replace countrynumcode =439 if countryname=="Burkina Faso (Upper Volta)"
replace countrynumcode =450 if countryname=="Liberia"
replace countrynumcode =451 if countryname=="Sierra Leone"
replace countrynumcode =452 if countryname=="Ghana"
replace countrynumcode =461 if countryname=="Togo"
replace countrynumcode =471 if countryname=="Cameroon"
replace countrynumcode =475 if countryname=="Nigeria"
replace countrynumcode =481 if countryname=="Gabon"
replace countrynumcode =482 if countryname=="Central African Republic"
replace countrynumcode =483 if countryname=="Chad"
replace countrynumcode =484 if countryname=="Congo"
replace countrynumcode =490 if countryname=="Congo, Democratic Republic of (Zaire)"
replace countrynumcode =500 if countryname=="Uganda"
replace countrynumcode =501 if countryname=="Kenya"
replace countrynumcode =510 if countryname=="Tanzania/Tanganyika"
replace countrynumcode =511 if countryname=="Zanzibar"
replace countrynumcode =516 if countryname=="Burundi"
replace countrynumcode =517 if countryname=="Rwanda"
replace countrynumcode =520 if countryname=="Somalia"
replace countrynumcode =522 if countryname=="Djibouti"
replace countrynumcode =530 if countryname=="Ethiopia"
replace countrynumcode =531 if countryname=="Eritrea"
replace countrynumcode =540 if countryname=="Angola"
replace countrynumcode =541 if countryname=="Mozambique"
replace countrynumcode =551 if countryname=="Zambia"
replace countrynumcode =552 if countryname=="Zimbabwe (Rhodesia)"
replace countrynumcode =553 if countryname=="Malawi"
replace countrynumcode =560 if countryname=="South Africa"
replace countrynumcode =563 if countryname=="Transvaal"
replace countrynumcode =564 if countryname=="Orange Free State"
replace countrynumcode =565 if countryname=="Namibia"
replace countrynumcode =570 if countryname=="Lesotho"
replace countrynumcode =571 if countryname=="Botswana"
replace countrynumcode =572 if countryname=="Swaziland"
replace countrynumcode =580 if countryname=="Madagascar (Malagasy)"
replace countrynumcode =580 if countryname=="Madagascar (Malagasy)"
replace countrynumcode =581 if countryname=="Comoros"
replace countrynumcode =590 if countryname=="Mauritius"
replace countrynumcode =600 if countryname=="Morocco"
replace countrynumcode =600 if countryname=="Morocco"
replace countrynumcode =615 if countryname=="Algeria"
replace countrynumcode =615 if countryname=="Algeria"
replace countrynumcode =616 if countryname=="Tunisia"
replace countrynumcode =616 if countryname=="Tunisia"
replace countrynumcode =620 if countryname=="Libya"
replace countrynumcode =620 if countryname=="Libya"
replace countrynumcode =625 if countryname=="Sudan"
replace countrynumcode =630 if countryname=="Iran (Persia)"
replace countrynumcode =640 if countryname=="Turkey/Ottoman Empire"
replace countrynumcode =645 if countryname=="Iraq"
replace countrynumcode =651 if countryname=="Egypt"
replace countrynumcode =651 if countryname=="Egypt"
replace countrynumcode =652 if countryname=="Syria"
replace countrynumcode =660 if countryname=="Lebanon"
replace countrynumcode =663 if countryname=="Jordan"
replace countrynumcode =666 if countryname=="Israel"
replace countrynumcode =670 if countryname=="Saudi Arabia"
replace countrynumcode =678 if countryname=="Yemen (Arab Republic of Yemen)"
replace countrynumcode =680 if countryname=="Yemen, People's Republic of"
replace countrynumcode =690 if countryname=="Kuwait"
replace countrynumcode =692 if countryname=="Bahrain"
replace countrynumcode =694 if countryname=="Qatar"
replace countrynumcode =696 if countryname=="United Arab Emirates"
replace countrynumcode =698 if countryname=="Oman"
replace countrynumcode =700 if countryname=="Afghanistan"
replace countrynumcode =700 if countryname=="Afghanistan"
replace countrynumcode =701 if countryname=="Turkmenistan"
replace countrynumcode =702 if countryname=="Tajikistan"
replace countrynumcode =703 if countryname=="Kyrgyz Republic"
replace countrynumcode =704 if countryname=="Uzbekistan"
replace countrynumcode =705 if countryname=="Kazakhstan"
replace countrynumcode =710 if countryname=="China"
replace countrynumcode =711 if countryname=="Tibet"
replace countrynumcode =712 if countryname=="Mongolia"
replace countrynumcode =713 if countryname=="Taiwan"
replace countrynumcode =730 if countryname=="Korea"
replace countrynumcode =731 if countryname=="Korea, People's Republic of"
replace countrynumcode =732 if countryname=="Korea, Republic of"
replace countrynumcode =740 if countryname=="Japan"
replace countrynumcode =750 if countryname=="India"
replace countrynumcode =760 if countryname=="Bhutan"
replace countrynumcode =770 if countryname=="Pakistan"
replace countrynumcode =771 if countryname=="Bangladesh"
replace countrynumcode =775 if countryname=="Myanmar (Burma)"
replace countrynumcode =775 if countryname=="Myanmar (Burma)"
replace countrynumcode =780 if countryname=="Sri Lanka (Ceylon)"
replace countrynumcode =781 if countryname=="Maldives"
replace countrynumcode =790 if countryname=="Nepal"
replace countrynumcode =800 if countryname=="Thailand"
replace countrynumcode =811 if countryname=="Cambodia (Kampuchea)"
replace countrynumcode =812 if countryname=="Laos"
replace countrynumcode =815 if countryname=="Vietnam (Annam/Cochin China/Tonkin)"
replace countrynumcode =816 if countryname=="Vietnam, Democratic Republic of"
replace countrynumcode =817 if countryname=="Vietnam, Republic of"
replace countrynumcode =820 if countryname=="Malaysia"
replace countrynumcode =830 if countryname=="Singapore"
replace countrynumcode =835 if countryname=="Brunei"
replace countrynumcode =840 if countryname=="Philippines"
replace countrynumcode =850 if countryname=="Indonesia"
replace countrynumcode =860 if countryname=="East Timor"
replace countrynumcode =900 if countryname=="Australia"
replace countrynumcode =910 if countryname=="Papua New Guinea"
replace countrynumcode =920 if countryname=="New Zealand"
replace countrynumcode =940 if countryname=="Solomon Islands"
replace countrynumcode =950 if countryname=="Fiji"


/*  This does creates the same codes for Gleditsch's list of micro-states */
replace countrycode = "DMA" if countryname=="Dominica"
replace countrycode = "GRN" if countryname=="Grenada"
replace countrycode = "SLU" if countryname=="Saint Lucia"
replace countrycode = "SVG" if countryname=="Saint Vincent and the Grenadines"
replace countrycode = "AAB" if countryname=="Antigua & Barbuda"
replace countrycode = "SKN" if countryname=="Saint Kitts and Nevis"
replace countrycode = "MNC" if countryname=="Monaco"
replace countrycode = "LIE" if countryname=="Liechtenstein"
replace countrycode = "SNM" if countryname=="San Marino"
replace countrycode = "AND" if countryname=="Andorra"
replace countrycode = "STP" if countryname=="Sao Tome and Principe"
replace countrycode = "SEY" if countryname=="Seychelles"
replace countrycode = "VAN" if countryname=="Vanuatu"
replace countrycode = "KBI" if countryname=="Kiribati"
replace countrycode = "NAU" if countryname=="Nauru"
replace countrycode = "TON" if countryname=="Tonga"
replace countrycode = "TUV" if countryname=="Tuvalu"
replace countrycode = "MSI" if countryname=="Marshall Islands"
replace countrycode = "PAL" if countryname=="Palau"
replace countrycode = "FSM" if countryname=="Federated States of Micronesia"
replace countrycode = "WSM" if countryname=="Samoa/Western Samoa"

replace countrynumcode =54 if countryname=="Dominica"
replace countrynumcode =55 if countryname=="Grenada"
replace countrynumcode =56 if countryname=="Saint Lucia"
replace countrynumcode =57 if countryname=="Saint Vincent and the Grenadines"
replace countrynumcode =58 if countryname=="Antigua & Barbuda"
replace countrynumcode =60 if countryname=="Saint Kitts and Nevis"
replace countrynumcode =221 if countryname=="Monaco"
replace countrynumcode =223 if countryname=="Liechtenstein"
replace countrynumcode =331 if countryname=="San Marino"
replace countrynumcode =232 if countryname=="Andorra"
replace countrynumcode =403 if countryname=="Sao Tome and Principe"
replace countrynumcode =591 if countryname=="Seychelles"
replace countrynumcode =935 if countryname=="Vanuatu"
replace countrynumcode =970 if countryname=="Kiribati"
replace countrynumcode =971 if countryname=="Nauru"
replace countrynumcode =972 if countryname=="Tonga"
replace countrynumcode =973 if countryname=="Tuvalu"
replace countrynumcode =983 if countryname=="Marshall Islands"
replace countrynumcode =986 if countryname=="Palau"
replace countrynumcode =987 if countryname=="Federated States of Micronesia"
replace countrynumcode =990 if countryname=="Samoa/Western Samoa"



/*	This section will convert things that only have Gleditsch's 
	3-character codes(like his GDP dataset) so that they also 
	have the countrynames. 

	In order to work, this section requires that you run:
	gen countryname="proper(countryname)"  */

capture replace countryname = "Afghanistan" if countrycode=="AFG"
capture replace countryname = "Afghanistan" if countrycode=="AFG"
capture replace countryname = "Albania" if countrycode=="ALB"
capture replace countryname = "Algeria" if countrycode=="ALG"
capture replace countryname = "Algeria" if countrycode=="ALG"
capture replace countryname = "Angola" if countrycode=="ANG"
capture replace countryname = "Argentina" if countrycode=="ARG"
capture replace countryname = "Armenia" if countrycode=="ARM"
capture replace countryname = "Australia" if countrycode=="AUL"
capture replace countryname = "Austria" if countrycode=="AUS"
capture replace countryname = "Austria-Hungary" if countrycode=="AUH"
capture replace countryname = "Azerbaijan" if countrycode=="AZE"
capture replace countryname = "Baden" if countrycode=="BAD"
capture replace countryname = "Bahamas" if countrycode=="BHM"
capture replace countryname = "Bahrain" if countrycode=="BAH"
capture replace countryname = "Bangladesh" if countrycode=="BNG"
capture replace countryname = "Barbados" if countrycode=="BAR"
capture replace countryname = "Bavaria" if countrycode=="BAV"
capture replace countryname = "Belarus (Byelorussia)" if countrycode=="BLR"
capture replace countryname = "Belgium" if countrycode=="BEL"
capture replace countryname = "Belize" if countrycode=="BLZ"
capture replace countryname = "Benin" if countrycode=="BEN"
capture replace countryname = "Bhutan" if countrycode=="BHU"
capture replace countryname = "Bolivia" if countrycode=="BOL"
capture replace countryname = "Bosnia-Herzegovina" if countrycode=="BOS"
capture replace countryname = "Botswana" if countrycode=="BOT"
capture replace countryname = "Brazil" if countrycode=="BRA"
capture replace countryname = "Brunei" if countrycode=="BRU"
capture replace countryname = "Bulgaria" if countrycode=="BUL"
capture replace countryname = "Burkina Faso (Upper Volta)" if countrycode=="BFO"
capture replace countryname = "Burundi" if countrycode=="BUI"
capture replace countryname = "Cambodia (Kampuchea)" if countrycode=="CAM"
capture replace countryname = "Cameroon" if countrycode=="CAO"
capture replace countryname = "Canada" if countrycode=="CAN"
capture replace countryname = "Cape Verde" if countrycode=="CAP"
capture replace countryname = "Central African Republic" if countrycode=="CEN"
capture replace countryname = "Chad" if countrycode=="CHA"
capture replace countryname = "Chile" if countrycode=="CHL"
capture replace countryname = "China" if countrycode=="CHN"
capture replace countryname = "Colombia" if countrycode=="COL"
capture replace countryname = "Comoros" if countrycode=="COM"
capture replace countryname = "Congo" if countrycode=="CON"
capture replace countryname = "Congo, Democratic Republic of (Zaire)" if countrycode=="DRC"
capture replace countryname = "Costa Rica" if countrycode=="COS"
capture replace countryname = "Cote D'Ivoire" if countrycode=="CDI"
****This is a departure from Gleditsch where the name is Cote D’Ivoire****

capture replace countryname = "Croatia" if countrycode=="CRO"
capture replace countryname = "Cuba" if countrycode=="CUB"
capture replace countryname = "Cyprus" if countrycode=="CYP"
capture replace countryname = "Czech Republic" if countrycode=="CZR"
capture replace countryname = "Czechoslovakia" if countrycode=="CZE"
capture replace countryname = "Denmark" if countrycode=="DEN"
capture replace countryname = "Djibouti" if countrycode=="DJI"
capture replace countryname = "Dominican Republic" if countrycode=="DOM"
capture replace countryname = "East Timor" if countrycode=="ETM"
capture replace countryname = "Ecuador" if countrycode=="ECU"
capture replace countryname = "Egypt" if countrycode=="EGY"
capture replace countryname = "Egypt" if countrycode=="EGY"
capture replace countryname = "El Salvador" if countrycode=="SAL"
capture replace countryname = "Equatorial Guinea" if countrycode=="EQG"
capture replace countryname = "Eritrea" if countrycode=="ERI"
capture replace countryname = "Estonia" if countrycode=="EST"
capture replace countryname = "Estonia" if countrycode=="EST"
capture replace countryname = "Ethiopia" if countrycode=="ETH"
capture replace countryname = "Fiji" if countrycode=="FJI"
capture replace countryname = "Finland" if countrycode=="FIN"
capture replace countryname = "France" if countrycode=="FRN"
capture replace countryname = "Gabon" if countrycode=="GAB"
capture replace countryname = "Gambia" if countrycode=="GAM"
capture replace countryname = "Georgia" if countrycode=="GRG"
capture replace countryname = "German Democratic Republic" if countrycode=="GDR"
capture replace countryname = "German Federal Republic" if countrycode=="GFR"
capture replace countryname = "Germany (Prussia)" if countrycode=="GMY"
capture replace countryname = "Ghana" if countrycode=="GHA"
capture replace countryname = "Great Colombia" if countrycode=="GCL"
capture replace countryname = "Greece" if countrycode=="GRC"
capture replace countryname = "Guatemala" if countrycode=="GUA"
capture replace countryname = "Guinea" if countrycode=="GUI"
capture replace countryname = "Guinea-Bissau" if countrycode=="GNB"
capture replace countryname = "Guyana" if countrycode=="GUY"
capture replace countryname = "Haiti" if countrycode=="HAI"
capture replace countryname = "Haiti" if countrycode=="HAI"
capture replace countryname = "Hanover" if countrycode=="HAN"
capture replace countryname = "Hesse-Darmstadt (Ducal)" if countrycode=="HSD"
capture replace countryname = "Hesse-Kassel (Electoral)" if countrycode=="HSE"
capture replace countryname = "Honduras" if countrycode=="HON"
capture replace countryname = "Hungary" if countrycode=="HUN"
capture replace countryname = "Iceland" if countrycode=="ICE"
capture replace countryname = "India" if countrycode=="IND"
capture replace countryname = "Indonesia" if countrycode=="INS"
capture replace countryname = "Iran (Persia)" if countrycode=="IRN"
capture replace countryname = "Iraq" if countrycode=="IRQ"
capture replace countryname = "Ireland" if countrycode=="IRE"
capture replace countryname = "Israel" if countrycode=="ISR"
capture replace countryname = "Italy/Sardinia" if countrycode=="ITA"
capture replace countryname = "Jamaica" if countrycode=="JAM"
capture replace countryname = "Japan" if countrycode=="JPN"
capture replace countryname = "Jordan" if countrycode=="JOR"
capture replace countryname = "Kazakhstan" if countrycode=="KZK"
capture replace countryname = "Kenya" if countrycode=="KEN"
capture replace countryname = "Korea" if countrycode=="KOR"
capture replace countryname = "Korea, People's Republic of" if countrycode=="PRK"
capture replace countryname = "Korea, Republic of" if countrycode=="ROK"
capture replace countryname = "Kuwait" if countrycode=="KUW"
capture replace countryname = "Kyrgyz Republic" if countrycode=="KYR"
capture replace countryname = "Laos" if countrycode=="LAO"
capture replace countryname = "Latvia" if countrycode=="LAT"
capture replace countryname = "Latvia" if countrycode=="LAT"
capture replace countryname = "Lebanon" if countrycode=="LEB"
capture replace countryname = "Lesotho" if countrycode=="LES"
capture replace countryname = "Liberia" if countrycode=="LBR"
capture replace countryname = "Libya" if countrycode=="LIB"
capture replace countryname = "Libya" if countrycode=="LIB"
capture replace countryname = "Lithuania" if countrycode=="LIT"
capture replace countryname = "Lithuania" if countrycode=="LIT"
capture replace countryname = "Luxembourg" if countrycode=="LUX"
capture replace countryname = "Macedonia (Former Yugoslav Republic of)" if countrycode=="MAC"
capture replace countryname = "Madagascar (Malagasy)" if countrycode=="MAG"
capture replace countryname = "Madagascar (Malagasy)" if countrycode=="MAG"
capture replace countryname = "Malawi" if countrycode=="MAW"
capture replace countryname = "Malaysia" if countrycode=="MAL"
capture replace countryname = "Maldives" if countrycode=="MAD"
capture replace countryname = "Mali" if countrycode=="MLI"
capture replace countryname = "Malta" if countrycode=="MLT"
capture replace countryname = "Mauritania" if countrycode=="MAA"
capture replace countryname = "Mauritius" if countrycode=="MAS"
capture replace countryname = "Mecklenburg-Schwerin" if countrycode=="MEC"
capture replace countryname = "Mexico" if countrycode=="MEX"
capture replace countryname = "Modena" if countrycode=="MOD"
capture replace countryname = "Moldova" if countrycode=="MLD"
capture replace countryname = "Mongolia" if countrycode=="MON"
capture replace countryname = "Montenegro" if countrycode=="MNG"
capture replace countryname = "Montenegro" if countrycode=="MNG"
capture replace countryname = "Morocco" if countrycode=="MOR"
capture replace countryname = "Morocco" if countrycode=="MOR"
capture replace countryname = "Mozambique" if countrycode=="MZM"
capture replace countryname = "Myanmar (Burma)" if countrycode=="MYA"
capture replace countryname = "Myanmar (Burma)" if countrycode=="MYA"
capture replace countryname = "Namibia" if countrycode=="NAM"
capture replace countryname = "Nepal" if countrycode=="NEP"
capture replace countryname = "Netherlands" if countrycode=="NTH"
capture replace countryname = "New Zealand" if countrycode=="NEW"
capture replace countryname = "Nicaragua" if countrycode=="NIC"
capture replace countryname = "Niger" if countrycode=="NIR"
capture replace countryname = "Nigeria" if countrycode=="NIG"
capture replace countryname = "Norway" if countrycode=="NOR"
capture replace countryname = "Oman" if countrycode=="OMA"
capture replace countryname = "Orange Free State" if countrycode=="OFS"
capture replace countryname = "Pakistan" if countrycode=="PAK"
capture replace countryname = "Panama" if countrycode=="PAN"
capture replace countryname = "Papal States" if countrycode=="PAP"
capture replace countryname = "Papua New Guinea" if countrycode=="PNG"
capture replace countryname = "Paraguay" if countrycode=="PAR"
capture replace countryname = "Parma" if countrycode=="PMA"
capture replace countryname = "Peru" if countrycode=="PER"
capture replace countryname = "Philippines" if countrycode=="PHI"
capture replace countryname = "Poland" if countrycode=="POL"
capture replace countryname = "Portugal" if countrycode=="POR"
capture replace countryname = "Qatar" if countrycode=="QAT"
capture replace countryname = "Rumania" if countrycode=="RUM"
capture replace countryname = "Russia (Soviet Union)" if countrycode=="RUS"
capture replace countryname = "Rwanda" if countrycode=="RWA"
capture replace countryname = "Saudi Arabia" if countrycode=="SAU"
capture replace countryname = "Saxony" if countrycode=="SAX"
capture replace countryname = "Senegal" if countrycode=="SEN"
capture replace countryname = "Serbia" if countrycode=="SER"
capture replace countryname = "Sierra Leone" if countrycode=="SIE"
capture replace countryname = "Singapore" if countrycode=="SIN"
capture replace countryname = "Slovakia" if countrycode=="SLO"
capture replace countryname = "Slovenia" if countrycode=="SLV"
capture replace countryname = "Solomon Islands" if countrycode=="SOL"
capture replace countryname = "Somalia" if countrycode=="SOM"
capture replace countryname = "South Africa" if countrycode=="SAF"
capture replace countryname = "Spain" if countrycode=="SPN"
capture replace countryname = "Sri Lanka (Ceylon)" if countrycode=="SRI"
capture replace countryname = "Sudan" if countrycode=="SUD"
capture replace countryname = "Surinam" if countrycode=="SUR"
capture replace countryname = "Swaziland" if countrycode=="SWA"
capture replace countryname = "Sweden" if countrycode=="SWD"
capture replace countryname = "Switzerland" if countrycode=="SWZ"
capture replace countryname = "Syria" if countrycode=="SYR"
capture replace countryname = "Taiwan" if countrycode=="TAW"
capture replace countryname = "Tajikistan" if countrycode=="TAJ"
capture replace countryname = "Tanzania/Tanganyika" if countrycode=="TAZ"
capture replace countryname = "Thailand" if countrycode=="THI"
capture replace countryname = "Tibet" if countrycode=="TBT"
capture replace countryname = "Togo" if countrycode=="TOG"
capture replace countryname = "Transvaal" if countrycode=="TRA"
capture replace countryname = "Trinidad and Tobago" if countrycode=="TRI"
capture replace countryname = "Tunisia" if countrycode=="TUN"
capture replace countryname = "Tunisia" if countrycode=="TUN"
capture replace countryname = "Turkey/Ottoman Empire" if countrycode=="TUR"
capture replace countryname = "Turkmenistan" if countrycode=="TKM"
capture replace countryname = "Tuscany" if countrycode=="TUS"
capture replace countryname = "Two Sicilies" if countrycode=="SIC"
capture replace countryname = "Uganda" if countrycode=="UGA"
capture replace countryname = "Ukraine" if countrycode=="UKR"
capture replace countryname = "United Arab Emirates" if countrycode=="UAE"
capture replace countryname = "United Kingdom" if countrycode=="UKG"
capture replace countryname = "United Provinces of Central America" if countrycode=="UPC"
capture replace countryname = "United States of America" if countrycode=="USA"
capture replace countryname = "Uruguay" if countrycode=="URU"
capture replace countryname = "Uzbekistan" if countrycode=="UZB"
capture replace countryname = "Venezuela" if countrycode=="VEN"
capture replace countryname = "Vietnam (Annam/Cochin China/Tonkin)" if countrycode=="VNM"
capture replace countryname = "Vietnam, Democratic Republic of" if countrycode=="DRV"
capture replace countryname = "Vietnam, Republic of" if countrycode=="RVN"
capture replace countryname = "Württemberg" if countrycode=="WRT"
capture replace countryname = "Yemen (Arab Republic of Yemen)" if countrycode=="YEM"
capture replace countryname = "Yemen, People's Republic of" if countrycode=="YPR"
capture replace countryname = "Serbia (Yugoslavia)" if countrycode=="SER"
*capture replace countryname = "Yugoslavia (Serbia)" if countrycode=="YUG" ** Changed on Aug 3, 2009, to match gleditsch's updates
capture replace countryname = "Zambia" if countrycode=="ZAM"
capture replace countryname = "Zanzibar" if countrycode=="ZAN"
capture replace countryname = "Zimbabwe (Rhodesia)" if countrycode=="ZIM"

* micro-states included by Gleditsch
capture replace countryname = "Andorra" if countrycode=="AND"
capture replace countryname = "Antigua & Barbuda" if countrycode=="AAB"
capture replace countryname = "Dominica" if countrycode=="DMA"
capture replace countryname = "Federated States of Micronesia" if countrycode=="FSM"
capture replace countryname = "Grenada" if countrycode=="GRN"
capture replace countryname = "Kiribati" if countrycode=="KBI"
capture replace countryname = "Liechtenstein" if countrycode=="LIE"
capture replace countryname = "Marshall Islands" if countrycode=="MSI"
capture replace countryname = "Monaco" if countrycode=="MNC"
capture replace countryname = "Nauru" if countrycode=="NAU"
capture replace countryname = "Palau" if countrycode=="PAL"
capture replace countryname = "Saint Kitts and Nevis" if countrycode=="SKN"
capture replace countryname = "Saint Lucia" if countrycode=="SLU"
capture replace countryname = "Saint Vincent and the Grenadines" if countrycode=="SVG"
capture replace countryname = "Samoa/Western Samoa" if countrycode=="WSM"
capture replace countryname = "San Marino" if countrycode=="SNM"
capture replace countryname = "Sao Tome and Principe" if countrycode=="STP"
****This is a departure from Gleditsch where the name is São Tomé and Principe****

capture replace countryname = "Seychelles" if countrycode=="SEY"
capture replace countryname = "Tonga" if countrycode=="TON"
capture replace countryname = "Tuvalu" if countrycode=="TUV"
capture replace countryname = "Vanuatu" if countrycode=="VAN"




