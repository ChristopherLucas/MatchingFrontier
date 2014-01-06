/* Joshua Loud
   Brigham Young University
   785 SWKT
   Provo, UT 84602

Last modified: 25 August, 07
   by: Rich Nielsen
	 Harvard University
	 rnielsen@fas.harvard.edu */

capture rename country countryname
replace countryname = proper(countryname)

/* 	In order to generate the variable which specifies
	a 1 if the state is in gleditsch's state system in
	time t, run these commands: */

capture gen stateinyeart_g=.
capture gen microstateinyeart_g=.

*A

replace countryname = "Afghanistan" if countryname == "AFGHANISTAN"
capture replace stateinyeart_g=1 if countryname=="Afghanistan" & year >=1816 & year<=1888
capture replace stateinyeart_g=1 if countryname=="Afghanistan" & year >=1919

replace countryname = "Albania" if countryname == "albania"
replace countryname = "Albania" if countryname == "ALBANIA"
capture replace stateinyeart_g=1 if countryname=="Albania" & year >=1913

replace countryname = "Algeria" if countryname == "ALGERIA"
replace countryname = "Algeria" if countryname == "algeria"
capture replace stateinyeart_g=1 if countryname=="Algeria" & year >=1816 & year <=1830
capture replace stateinyeart_g=1 if countryname=="Algeria" & year >=1962

replace countryname = "American Samoa" if countryname == "AMERICAN SAMOA"
****This isn't one of Gleditsch's countries******

replace countryname = "Andorra" if countryname == "ANDORRA"
capture replace microstateinyeart_g=1 if countryname=="Andorra" & year >=1816

replace countryname = "Angola" if countryname == "ANGOLA"
capture replace stateinyeart_g=1 if countryname=="Angola" & year >=1975

replace countryname = "Anguilla" if countryname == "ANGUILLA"
****Not in Gleditsch****

replace countryname = "Antigua & Barbuda" if countryname == "Antigua And Barbuda"
replace countryname = "Antigua & Barbuda" if countryname == "Antigua"
replace countryname = "Antigua & Barbuda" if countryname == "Antiguaandbarbuda"
replace countryname = "Antigua & Barbuda" if countryname == "ANTAlgeriaIGUA AND BARBUDA"
capture replace microstateinyeart_g=1 if countryname=="Antigua & Barbuda" & year >=1981

replace countryname = "Antilles" if countryname == "Netherlandsantillesandaruba"
replace countryname = "Antilles" if countryname == "Netherlands Antilles"
replace countryname = "Antilles" if countryname == "NETHERLANDS ANTILLES" 
replace countryname = "Antilles" if countryname == "Antilles" 
****Gleditsch doesn't include the Antilles****

replace countryname = "Argentina" if countryname == "ARGENTINA"
replace countryname = "Argentina" if countryname == "argentina"
capture replace stateinyeart_g=1 if countryname=="Argentina" & year >=1816

replace countryname = "Armenia" if countryname == "ARMENIA"
replace countryname = "Armenia" if countryname == "armenia"
capture replace stateinyeart_g=1 if countryname=="Armenia" & year >=1991

replace countryname = "Aruba" if countryname == "ARUBA"
****Gleditsch does not include Aruba****

replace countryname = "Australia" if countryname == "AUSTRALIA"
capture replace stateinyeart_g=1 if countryname=="Australia" & year >=1901

replace countryname = "Austria" if countryname == "AUSTRIA"
capture replace stateinyeart_g=1 if countryname=="Austria" & year >=1918

*Austria-Hungary
capture replace stateinyeart_g=1 if countryname=="Austria-Hungary" & year >=1816 & year <=1918

replace countryname = "Azerbaijan" if countryname == "AZERBAIJAN"
capture replace stateinyeart_g=1 if countryname=="Azerbaijan" & year >=1991

*B

*Baden
capture replace stateinyeart_g=1 if countryname=="Baden" & year >=1816 & year <=1871

replace countryname = "Bahamas" if countryname == "Bahamas, The"
replace countryname = "Bahamas" if countryname == "The Bahamas"
replace countryname = "Bahamas" if countryname == "BAHAMAS"
replace countryname = "Bahamas" if countryname == "Bahamas, the" 
capture replace stateinyeart_g=1 if countryname=="Bahamas" & year >=1973

replace countryname = "Bahrain" if countryname == "BAHRAIN"
capture replace stateinyeart_g=1 if countryname=="Bahrain" & year >=1816

replace countryname = "Bangladesh" if countryname == "Bgd"
replace countryname = "Bangladesh" if countryname == "BANGLADESH"
replace countryname = "Bangladesh" if countryname == "bangladesh"
replace countryname = "Bangladesh" if countryname == "BGD"
capture replace stateinyeart_g=1 if countryname=="Bangladesh" & year >=1971

replace countryname = "Barbados" if countryname == "BARBADOS"
capture replace stateinyeart_g=1 if countryname=="Barbados" & year >=1966

replace countryname = "Belarus (Byelorussia)" if countryname == "Belarus"
replace countryname = "Belarus (Byelorussia)" if countryname == "BELARUS"
capture replace stateinyeart_g=1 if countryname=="Belarus (Byelorussia)" & year >=1991

replace countryname = "Belgium" if countryname == "BELGIUM"
capture replace stateinyeart_g=1 if countryname=="Belgium" & year >=1830

replace countryname = "Belize" if countryname == "BELIZE"
replace countryname = "Belize" if countryname == "belize"
capture replace stateinyeart_g=1 if countryname=="Belize" & year >=1981

replace countryname = "Benin" if countryname == "BENIN"
replace countryname = "Benin" if countryname == "benin"
capture replace stateinyeart_g=1 if countryname=="Benin" & year >=1960

replace countryname = "Bermuda" if countryname == "BERMUDA"
****Gleditsch doesn't include Bermuda****

replace countryname = "Bhutan" if countryname == "Btn"
replace countryname = "Bhutan" if countryname == "BHUTAN"
replace countryname = "Bhutan" if countryname == "BTN"
capture replace stateinyeart_g=1 if countryname=="Bhutan" & year >=1949

replace countryname = "Bolivia" if countryname == "BOLIVIA"
replace countryname = "Bolivia" if countryname == "bolivia"
capture replace stateinyeart_g=1 if countryname=="Bolivia" & year >=1825

replace countryname = "Bosnia-Herzegovina" if countryname == "Bosniaandherzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia And Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia and Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "BOSNIA AND HERZEGOVINA"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia & Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "BOSNIA AND HERZEGOVINA"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia Herzegovenia"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "BosniaHerzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "BOSNIA-HERZEGOVINA"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia & Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia-Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia & Herzegovina"
replace countryname = "Bosnia-Herzegovina" if countryname == "Bosnia"
capture replace stateinyeart_g=1 if countryname=="Bosnia-Herzegovina" & year >=1992

replace countryname = "Botswana" if countryname == "BOTSWANA"
replace countryname = "Botswana" if countryname == "BOTSWANA"
replace countryname = "Botswana" if countryname == "botswana"
capture replace stateinyeart_g=1 if countryname=="Botswana" & year >=1966

replace countryname = "Brazil" if countryname == "BRAZIL"
replace countryname = "Brazil" if countryname == "brazil"
capture replace stateinyeart_g=1 if countryname=="Brazil" & year >=1822

replace countryname = "British Virgin Islands" if countryname == "BRITISH VIRGIN ISLANDS"
****Not in Gleditsch****

replace countryname = "Brunei" if countryname == "Brunei Darus"
replace countryname = "Brunei" if countryname == "Brunei Darussalam"
replace countryname = "Brunei" if countryname == "BRUNEI DARUSSALAM"
replace countryname = "Brunei" if countryname == "BRUNEI"
capture replace stateinyeart_g=1 if countryname=="Brunei" & year >=1984

replace countryname = "Bulgaria" if countryname == "BULGARIA"
replace countryname = "Bulgaria" if countryname == "bulgaria"
capture replace stateinyeart_g=1 if countryname=="Bulgaria" & year >=1878

replace countryname = "Burkina Faso (Upper Volta)" if countryname == "Upper Volta"
replace countryname = "Burkina Faso (Upper Volta)" if countryname == "Burkinafaso"
replace countryname = "Burkina Faso (Upper Volta)" if countryname == "Birkina Faso"
replace countryname = "Burkina Faso (Upper Volta)" if countryname == "Burkina-Faso"
replace countryname = "Burkina Faso (Upper Volta)" if countryname == "BURKINA FASO"
replace countryname = "Burkina Faso (Upper Volta)" if countryname == "BurkinaFaso"
replace countryname = "Burkina Faso (Upper Volta)" if countryname == "Burkina Faso"
capture replace stateinyeart_g=1 if countryname=="Burkina Faso (Upper Volta)" & year >=1960

replace countryname = "Burundi" if countryname == "BURUNDI"
replace countryname = "Burundi" if countryname == "burundi"
capture replace stateinyeart_g=1 if countryname=="Burundi" & year >=1962


*C

replace countryname = "Cambodia (Kampuchea)" if countryname == "Cambodia"
replace countryname = "Cambodia (Kampuchea)" if countryname == "Kampochea"
replace countryname = "Cambodia (Kampuchea)" if countryname == "CAMBODIA"
replace countryname = "Cambodia (Kampuchea)" if countryname == "cambodia"
capture replace stateinyeart_g=1 if countryname=="Cambodia (Kampuchea)" & year >=1953

replace countryname = "Cameroon" if countryname == "Cameroun"
replace countryname = "Cameroon" if countryname == "CAMEROON"
replace countryname = "Cameroon" if countryname == "cameroon"
capture replace stateinyeart_g=1 if countryname=="Cameroon" & year >=1960

replace countryname = "Canada" if countryname == "CANADA"
capture replace stateinyeart_g=1 if countryname=="Canada" & year >=1867

replace countryname = "Cape Verde" if countryname == "Capeverde"
replace countryname = "Cape Verde" if countryname == "Cape-Verde"
replace countryname = "Cape Verde" if countryname == "CAPE VERDE IS."
replace countryname = "Cape Verde" if countryname == "CAPE VERDE"
replace countryname = "Cape Verde" if countryname == "Cape Verde Is."
capture replace stateinyeart_g=1 if countryname=="Cape Verde" & year >=1975

replace countryname = "Cayman Islands" if countryname == "CAYMAN ISLANDS"
****Gleditsch doesn't include the Cayman Islands****

replace countryname = "Central African Republic" if countryname == "Centralafricanrepublic"
replace countryname = "Central African Republic" if countryname == "Central African Rep"
replace countryname = "Central African Republic" if countryname == "Central African Rep."
replace countryname = "Central African Republic" if countryname == "CENTRAL AFR.R."
replace countryname = "Central African Republic" if countryname == "CENTRAL AFRICAN REP."
replace countryname = "Central African Republic" if countryname == "CENTRAL AFRICAN REPUBLIC"
replace countryname = "Central African Republic" if countryname == "Central Afr.R."
replace countryname = "Central African Republic" if countryname == "Central Africa"
replace countryname = "Central African Republic" if countryname == "Central Afr. R."
capture replace stateinyeart_g=1 if countryname=="Central African Republic" & year >=1960

replace countryname = "Chad" if countryname == "CHAD"
replace countryname = "Chad" if countryname == "chad"
replace countryname = "Chad" if countryname == "Rep. Chad"
capture replace stateinyeart_g=1 if countryname=="Chad" & year >=1960

****Gleditsch does not include the Channel Islands****

replace countryname = "Chile" if countryname == "CHILE"
replace countryname = "Chile" if countryname == "chile"
capture replace stateinyeart_g=1 if countryname=="Chile" & year >=1818

replace countryname = "China" if countryname == "People'S Republic Of China"
replace countryname = "China" if countryname == "Chn"
replace countryname = "China" if countryname == "China, People'S Republic Of"
replace countryname = "China" if countryname == "CHINA"
replace countryname = "China" if countryname == "china"
replace countryname = "China" if countryname == "CHN"
capture replace stateinyeart_g=1 if countryname=="China" & year >=1816

replace countryname = "Colombia" if countryname == "Columbia"
replace countryname = "Colombia" if countryname == "COLOMBIA"
capture replace stateinyeart_g=1 if countryname=="Colombia" & year >=1830

replace countryname = "Comoros" if countryname == "COMOROS"
capture replace stateinyeart_g=1 if countryname=="Comoros" & year >=1975

replace countryname = "Congo" if countryname == "Congo, Rep."
replace countryname = "Congo" if countryname == "Congo/ Rep."
replace countryname = "Congo" if countryname == "Congo, Republic Of"
replace countryname = "Congo" if countryname == "Congo, Rep."
replace countryname = "Congo" if countryname == "Congo-Brazzaville"
replace countryname = "Congo" if countryname == "Congo (Brazzaville)"
replace countryname = "Congo" if countryname == "Congo Brazzaville"
replace countryname = "Congo" if countryname == "CONGO"
replace countryname = "Congo" if countryname == "CONGO, REP."
replace countryname = "Congo" if countryname == "Congo, Rep."
replace countryname = "Congo" if countryname == "Congo, Republic of the"
replace countryname = "Congo" if countryname == "Congo, Republic of"
replace countryname = "Congo" if countryname == "Congo/ Rep."
replace countryname = "Congo" if countryname == "CONGO, REP.                                                                "
replace countryname = "Congo" if countryname == "Congo, Rep.                                                                "
replace countryname = "Congo" if countryname == "Congo, Republic Of The"
replace countryname = "Congo" if countryname == "Congo (Bra)"
replace countryname = "Congo" if countryname == "Congo (Brazzaville,Rep. Of Congo)"
capture replace stateinyeart_g=1 if countryname=="Congo" & year >=1960

replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem.Rep."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Democratic Republic Of / Zaire"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem. Rep."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Zaire"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Democratic Republic Of"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo-Kinshasa"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo/ Dem. Rep."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Democratic Rep."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem Rep"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Democratic Republic Of The Congo"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Democratic Republic Of Congo"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo (Kinshasa)"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo Kinshasa"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, DR"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dr"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem. Rep."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "CONGO, DEM.REP."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "CONGO, DEM.REP.                                                            "
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Democratic Republic of / Zaire"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo/ Dem. Rep."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Democratic Republic of the Congo"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "ZAIRE"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Zaire"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem. Rep."
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Democratic Republic of"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Democratic Republic"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dem.Rep.                                                            "
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Dr.                                                            "
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo, Democratic Republic Of (Zaire)"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Congo (Kin)"
replace countryname = "Congo, Democratic Republic of (Zaire)" if countryname == "Zaire (Congo-Kinshasha,Dem. Rep. Of Congo)"
capture replace stateinyeart_g=1 if countryname=="Congo, Democratic Republic of (Zaire)" & year >=1960

replace countryname = "Costa Rica" if countryname == "Costarica"
replace countryname = "Costa Rica" if countryname == "COSTA RICA"
capture replace stateinyeart_g=1 if countryname=="Costa Rica" & year >=1840

replace countryname = "Cote D'Ivoire" if countryname == "C(te d'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "C+te D?Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Ctte D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Cote D'Ivore"
replace countryname = "Cote D'Ivoire" if countryname == "Cote-D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "C(te D?ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Cotedivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Ivory Coast"
replace countryname = "Cote D'Ivoire" if countryname == "Côte D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "CÌ«te D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Cote D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "c(te d?ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "C(te d'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "C+TE D?IVOIRE"
replace countryname = "Cote D'Ivoire" if countryname == "Cote d`Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Cote d´Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Cote D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Côte d’Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "COTE D'IVOIRE"
replace countryname = "Cote d'Ivoire" if countryname == "Cote D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Cote d'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "IVORY COAST"
replace countryname = "Cote D'Ivoire" if countryname == "Ivory Coast"
replace countryname = "Cote D'Ivoire" if countryname == "Cote D’Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "CôTe D'Ivoire"
replace countryname = "Cote D'Ivoire" if countryname == "Cote D'Ivoire (Ivorycoast)"
capture replace stateinyeart_g=1 if countryname=="Cote D'Ivoire" & year >=1960
***This is a departure from Gleditsch, where the name is "Cote D’Ivoire"*** 

replace countryname = "Croatia" if countryname == "CROATIA"
replace countryname = "Croatia" if countryname == "croatia"
capture replace stateinyeart_g=1 if countryname=="Croatia" & year >=1991

replace countryname = "Cuba" if countryname == "CUBA"
capture replace stateinyeart_g=1 if countryname=="Cuba" & year >=1902

replace countryname = "Cyprus" if countryname == "CYPRUS"
capture replace stateinyeart_g=1 if countryname=="Cyprus" & year >=1960

replace countryname = "Czech Republic" if countryname == "Czechrepublic"
replace countryname = "Czech Republic" if countryname == "Czech Rep"
replace countryname = "Czech Republic" if countryname == "Czech Rep."
replace countryname = "Czech Republic" if countryname == "CZECH REPUBLIC"
capture replace stateinyeart_g=1 if countryname=="Czech Republic" & year >=1993

replace countryname = "Czechoslovakia" if countryname == "Csfr (Czechoslovakia)"
replace countryname = "Czechoslovakia" if countryname == "Czechoslovakia (Former)"
replace countryname = "Czechoslovakia" if countryname == "CSFR (CZECHOSLOVAKIA)"
replace countryname = "Czechoslovakia" if countryname == "CZECHOSLOVAKIA"
replace countryname = "Czechoslovakia" if countryname == "Czech"
capture replace stateinyeart_g=1 if countryname=="Czechoslovakia" & year >=1919 & year <=1992


*D 

replace countryname = "Denmark" if countryname == "DENMARK"
capture replace stateinyeart_g=1 if countryname=="Denmark" & year >=1816

replace countryname = "Djibouti" if countryname == "Dijbouti" 
capture replace stateinyeart_g=1 if countryname=="Djibouti" & year >=1977

replace countryname = "Dominica" if countryname == "DOMINICA"
capture replace microstateinyeart_g=1 if countryname=="Dominica" & year >=1978

replace countryname = "Dominican Republic" if countryname == "Dominican Rep."
replace countryname = "Dominican Republic" if countryname == "Dominican Rep"
replace countryname = "Dominican Republic" if countryname == "Dominicanrep"
replace countryname = "Dominican Republic" if countryname == "DOMINICAN REP."
replace countryname = "Dominican Republic" if countryname == "DOMINICAN REPUBLIC"
replace countryname = "Dominican Republic" if countryname == "dominican republic"
capture replace stateinyeart_g=1 if countryname=="Dominican Republic" & year >=1844


*E

replace countryname = "East Timor" if countryname =="Timor-Leste"
replace countryname = "East Timor" if countryname =="Timor Leste"
replace countryname = "East Timor" if countryname =="Timore-Leste"
replace countryname = "East Timor" if countryname == "East Timor LESTE"
replace countryname = "East Timor" if countryname == "EAST TIMOR"
replace countryname = "East Timor" if countryname == "TIMOR"
replace countryname = "East Timor" if countryname == "East Timor Leste"
capture replace stateinyeart_g=1 if countryname=="East Timor" & year >=2002

replace countryname = "Ecuador" if countryname == "ECUADOR"
replace countryname = "Ecuador" if countryname == "ecuador"
capture replace stateinyeart_g=1 if countryname=="Ecuador" & year >=1830

 replace countryname = "Egypt" if countryname == "Arab Republic Of Egypt"
replace countryname = "Egypt" if countryname == "Egypt, Arab Rep."
replace countryname = "Egypt" if countryname == "Egypt, Arab Republic Of"
replace countryname = "Egypt" if countryname == "Egypt Arab Republic Of"
replace countryname = "Egypt" if countryname == "Arab Republic of Egypt"
replace countryname = "Egypt" if countryname == "Egypt Arab Republic of"
replace countryname = "Egypt" if countryname == "EGYPT"
replace countryname = "Egypt" if countryname == "egypt"
replace countryname = "Egypt" if countryname == "Egypt, Arab Rep."
replace countryname = "Egypt" if countryname == "Egypt, Arab Republic"
capture replace stateinyeart_g=1 if countryname=="Egypt" & year >=1827 & year<=1855
capture replace stateinyeart_g=1 if countryname=="Egypt" & year >=1922

replace countryname = "El Salvador" if countryname == "Elsalvador"
replace countryname = "El Salvador" if countryname == "EL SALVADOR"
replace countryname = "El Salvador" if countryname == "el salvador" 
capture replace stateinyeart_g=1 if countryname=="El Salvador" & year >=1840

replace countryname = "Equatorial Guinea" if countryname == "EQUATORIAL GUINEA"
replace countryname = "Equatorial Guinea" if countryname == "Equitorial Guinea"
capture replace stateinyeart_g=1 if countryname=="Equatorial Guinea" & year >=1968

replace countryname = "Eritrea" if countryname == "ERITREA"
replace countryname = "Eritrea" if countryname == "Eritera"
capture replace stateinyeart_g=1 if countryname=="Eritrea" & year >=1993

replace countryname = "Estonia" if countryname == "ESTONIA"
capture replace stateinyeart_g=1 if countryname=="Estonia" & year >=1918 & year <=1940
capture replace stateinyeart_g=1 if countryname=="Estonia" & year >=1991

replace countryname = "Ethiopia" if countryname == "ETHIOPIA"
replace countryname = "Ethiopia" if countryname == "Ethiopia2"
capture replace stateinyeart_g=1 if countryname=="Ethiopia" & year >=1855


*F

replace countryname = "Faeroe Islands" if countryname == "Faeroe_Is"
replace countryname = "Faeroe Islands" if countryname == "FAEROE ISLANDS"
****Gleditsch does not include the Faeroe Islands****

replace countryname = "Falkland Islands" if countryname == "Falkland Islands (Malvinas)"
replace countryname = "Falkland Islands" if countryname == "Falkland Is.(Malvinas)"
replace countryname = "Falkland Islands" if countryname == "Falkland Is(Malvinas)"
replace countryname = "Falkland Islands" if countryname == "FALKLAND ISLANDS"
****Not in Gledtisch****

replace countryname = "Federated States of Micronesia" if countryname == "Federated States Of Micronesia"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed States Of"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed States"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed. States Of"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed. States"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Federated States Of"
replace countryname = "Federated States of Micronesia" if countryname == "Federal States Of Micronesia"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed. Sts."
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed Sts"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia (Federated States of)"
replace countryname = "Federated States of Micronesia" if countryname == "MICRONESIA"
replace countryname = "Federated States of Micronesia" if countryname == "MICRONESIA,FED. STATES                                                     "
replace countryname = "Federated States of Micronesia" if countryname == "MICRONESIA, FED. STATES"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed. Sts."
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Fed Stat"
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia,Fed. States                                                     "
replace countryname = "Federated States of Micronesia" if countryname == "Micronesia, Federal States"
replace microstateinyeart_g=1 if countryname=="Federated States of Micronesia" & year >=1986

replace countryname = "Fiji" if countryname == "Fiji Islands"
replace countryname = "Fiji" if countryname == "FIJI" 
capture replace stateinyeart_g=1 if countryname=="Fiji" & year >=1970

replace countryname = "Finland" if countryname == "FINLAND"
capture replace stateinyeart_g=1 if countryname=="Finland" & year >=1917

replace countryname = "France" if countryname == "FRANCE"
capture replace stateinyeart_g=1 if countryname=="France" & year >=1816

replace countryname = "French Guiana" if countryname == "FRENCH GUIANA"
replace countryname = "French Guiana" if countryname == "Guiana, French"
****Not in Gleditsch****

replace countryname = "French Polynesia" if countryname == "FRENCH POLYNESIA"
****Not in Gleditsch****

*G 

replace countryname = "Gabon" if countryname == "GABON"
replace countryname = "Gabon" if countryname == "gabon"
capture replace stateinyeart_g=1 if countryname=="Gabon" & year >=1960

replace countryname = "Gambia" if countryname == "The Gambia"
replace countryname = "Gambia" if countryname == "Gambia, The"
replace countryname = "Gambia" if countryname == "GAMBIA"
replace countryname = "Gambia" if countryname == "Gambia, The      "
replace countryname = "Gambia" if countryname == "Gambia, The"
replace countryname = "Gambia" if countryname == "The Gambia"
capture replace stateinyeart_g=1 if countryname=="Gambia" & year >=1965

replace countryname = "Georgia" if countryname == "GEORGIA"
replace countryname = "Georgia" if countryname == "georgia"
capture replace stateinyeart_g=1 if countryname=="Georgia" & year >=1991

replace countryname = "German Democratic Republic" if countryname == "GDR (Ex)"
replace countryname = "German Democratic Republic" if countryname == "East Germany"
replace countryname = "German Democratic Republic" if countryname == "Germany Dem. Rep."
replace countryname = "German Democratic Republic" if countryname == "Germany Dem Rep"
replace countryname = "German Democratic Republic" if countryname == "German Dem. Rep."
replace countryname = "German Democratic Republic" if countryname == "German Dem Rep"
replace countryname = "German Democratic Republic" if countryname == "GDR (ex)"
replace countryname = "German Democratic Republic" if countryname == "GERMANY, EAST"
replace countryname = "German Democratic Republic" if countryname == "Germany East"
replace countryname = "German Democratic Republic" if countryname == "Germany, E."
replace countryname = "German Democratic Republic" if countryname == "Germany, East"
capture replace stateinyeart_g=1 if countryname=="German Democratic Republic" & year >=1949 & year <=1990

replace countryname = "German Federal Republic" if countryname == "Germany"
replace countryname = "German Federal Republic" if countryname == "Germany, West"
replace countryname = "German Federal Republic" if countryname == "West Germany"
replace countryname = "German Federal Republic" if countryname == "Germany_Unified"
replace countryname = "German Federal Republic" if countryname == "Germany Fed. Rep."
replace countryname = "German Federal Republic" if countryname == "Germany Fed Rep"
replace countryname = "German Federal Republic" if countryname == "German Fed. Rep."
replace countryname = "German Federal Republic" if countryname == "German Fed Rep"
replace countryname = "German Federal Republic" if countryname == "GERMANY"
replace countryname = "German Federal Republic" if countryname == "GERMANY, WEST"
replace countryname = "German Federal Republic" if countryname == "WEST GERMANY"
replace countryname = "German Federal Republic" if countryname == "West Germany"
replace countryname = "German Federal Republic" if countryname == "Germany West"
replace countryname = "German Federal Republic" if countryname == "Germany, W."
replace countryname = "German Federal Republic" if countryname == "German Federal Republic" 
replace countryname = "German Federal Republic" if countryname == "Federal Republic Of Germany"
capture replace stateinyeart_g=1 if countryname=="German Federal Republic" & year >=1949

*Germany (Prussia)
capture replace stateinyeart_g=1 if countryname=="Germany (Prussia)" & year >=1816 & year <=1945

replace countryname = "Ghana" if countryname == "GHANA"
replace countryname = "Ghana" if countryname == "ghana"
capture replace stateinyeart_g=1 if countryname=="Ghana" & year >=1957

replace countryname = "Gibralter" if countryname == "GIBRALTAR"
****Not in Gleditsch****

replace countryname = "Greece" if countryname == "GREECE"
capture replace stateinyeart_g=1 if countryname=="Greece" & year >=1827

replace countryname = "Grenada" if countryname == "GRENADA"
capture replace microstateinyeart_g=1 if countryname=="Grenada" & year >=1974

****Greenland (Denmark)--Not in Gleditsch****

replace countryname = "Guadeloupe" if countryname == "GUADELOUPE"
****Not in Gleditsch****

replace countryname = "Guatemala" if countryname == "GUATEMALA"
replace countryname = "Guatemala" if countryname == "guatemala"
capture replace stateinyeart_g=1 if countryname=="Guatemala" & year >=1840

****Guam (US)--Not in Gleditsch****

replace countryname = "Guiana" if countryname == "GUIANA"
****Not in Gleditsch****

replace countryname = "Guinea" if countryname == "GUINEA"
replace countryname = "Guinea" if countryname == "guinea"
capture replace stateinyeart_g=1 if countryname=="Guinea" & year >=1958

replace countryname = "Guinea-Bissau" if countryname == "Guineabissau"
replace countryname = "Guinea-Bissau" if countryname == "Guinea Bissau"
replace countryname = "Guinea-Bissau" if countryname == "Guinea Bissau"
replace countryname = "Guinea-Bissau" if countryname == "GUINEA-BISS"
replace countryname = "Guinea-Bissau" if countryname == "GuineaBissau"
replace countryname = "Guinea-Bissau" if countryname == "GUINEA-BISSAU"
replace countryname = "Guinea-Bissau" if countryname == "Guinea-Biss"
capture replace stateinyeart_g=1 if countryname=="Guinea-Bissau" & year >=1974

replace countryname = "Guyana" if countryname == "GUYANA"
capture replace stateinyeart_g=1 if countryname=="Guyana" & year >=1966


*H 

replace countryname = "Haiti" if countryname == "HAITI"
capture replace stateinyeart_g=1 if countryname=="Haiti" & year >=1816 & year <=1915
capture replace stateinyeart_g=1 if countryname=="Haiti" & year >=1934

*Hanover
capture replace stateinyeart_g=1 if countryname=="Hanover" & year >=1816 & year <=1871

*Hesse-Darmstadt (Ducal)
capture replace stateinyeart_g=1 if countryname=="Hesse-Darmstadt (Ducal)" & year >=1816 & year <=1871

*Hesse-Kassel (Electoral)
capture replace stateinyeart_g=1 if countryname=="Hesse-Kassel (Electoral)" & year >=1816 & year <=1871

replace countryname = "Honduras" if countryname == "HONDURAS"
replace countryname = "Honduras" if countryname == "honduras"
capture replace stateinyeart_g=1 if countryname=="Honduras" & year >=1840

replace countryname = "Hong Kong" if countryname == "Hongkong"
replace countryname = "Hong Kong" if countryname == "Hong Kong, China"
replace countryname = "Hong Kong" if countryname == "Hong Kong China"
replace countryname = "Hong Kong" if countryname == "Hong Kong Sar"
replace countryname = "Hong Kong" if countryname == "CHINA, HONG KONG SAR"
replace countryname = "Hong Kong" if countryname == "HONG KONG"
replace countryname = "Hong Kong" if countryname == "HONG KONG, CHINA                                                           "
replace countryname = "Hong Kong" if countryname == "HONG KONG, CHINA"
replace countryname = "Hong Kong" if countryname == "Hong Kong, China"
replace countryname = "Hong Kong" if countryname == "Hong Kong, China                                                           "
replace countryname = "Hong Kong" if countryname == "China,P.R.:Hong Kong"
****Not in Gleditsch****

replace countryname = "Hungary" if countryname == "HUNGARY"
capture replace stateinyeart_g=1 if countryname=="Hungary" & year >=1918

*I 

replace countryname = "Iceland" if countryname == "ICELAND"
capture replace stateinyeart_g=1 if countryname=="Iceland" & year >=1944

replace countryname = "India" if countryname == "Ind"
replace countryname = "India" if countryname == "Indiapost1947"
replace countryname = "India" if countryname == "Indiapre1948"
replace countryname = "India" if countryname == "IND"
replace countryname = "India" if countryname == "INDIA"
replace countryname = "India" if countryname == "india"
capture replace stateinyeart_g=1 if countryname=="India" & year >=1947

replace countryname = "Indonesia" if countryname == "IDN"
replace countryname = "Indonesia" if countryname == "INDONESIA"
replace countryname = "Indonesia" if countryname == "indonesia"
replace countryname = "Indonesia" if countryname == "Idn"
capture replace stateinyeart_g=1 if countryname=="Indonesia" & year >=1945

replace countryname = "Iran (Persia)" if countryname == "Iran"
replace countryname = "Iran (Persia)" if countryname == "Iran (Islamic Republic Of)"
replace countryname = "Iran (Persia)" if countryname == "Iran, Islamic Rep"
replace countryname = "Iran (Persia)" if countryname == "Iran, Islamic Rep."
replace countryname = "Iran (Persia)" if countryname == "Iran, Islamic Republic Of"
replace countryname = "Iran (Persia)" if countryname == "Iran (Islamic Rep. Of)"
replace countryname = "Iran (Persia)" if countryname == "Iran (Islamic Republic of)"
replace countryname = "Iran (Persia)" if countryname == "IRAN ISLAMIC REP. OF"
replace countryname = "Iran (Persia)" if countryname == "IRAN"
replace countryname = "Iran (Persia)" if countryname == "Iran, Islamic Rep." 
replace countryname = "Iran (Persia)" if countryname == "Iran, Islamic"
replace countryname = "Iran (Persia)" if countryname == "Iran, I.R. Of"
capture replace stateinyeart_g=1 if countryname=="Iran (Persia)" & year >=1816

replace countryname = "Iraq" if countryname == "IRAQ"
capture replace stateinyeart_g=1 if countryname=="Iraq" & year >=1932

replace countryname = "Ireland" if countryname == "IRELAND"
capture replace stateinyeart_g=1 if countryname=="Ireland" & year >=1921

replace countryname = "Isle of Man" if countryname == "ISLE OF MAN"
****Not in Gleditsch****

replace countryname = "Israel" if countryname == "ISRAEL"
capture replace stateinyeart_g=1 if countryname=="Israel" & year >=1948

replace countryname = "Italy/Sardinia" if countryname == "ITALY"
replace countryname = "Italy/Sardinia" if countryname == "Italy"
capture replace stateinyeart_g=1 if countryname=="Italy/Sardinia" & year >=1816


*J

replace countryname = "Jamaica" if countryname == "JAMAICA"
capture replace stateinyeart_g=1 if countryname=="Jamaica" & year >=1962

replace countryname = "Japan" if countryname == "JAPAN"
capture replace stateinyeart_g=1 if countryname=="Japan" & year >=1816

replace countryname = "Jordan" if countryname == "JORDAN"
replace countryname = "Jordan" if countryname == "jordan"
capture replace stateinyeart_g=1 if countryname=="Jordan" & year >=1946


*K

replace countryname = "Kazakhstan" if countryname == "Khazakhstan"
replace countryname = "Kazakhstan" if countryname == "Kazakstan"
replace countryname = "Kazakhstan" if countryname == "KAZAKHSTAN"
replace countryname = "Kazakhstan" if countryname == "kazakhstan"
replace countryname = "Kazakhstan" if countryname == "Kazakstan"
replace countryname = "Kazakhstan" if countryname == "Khazakhstan"
capture replace stateinyeart_g=1 if countryname=="Kazakhstan" & year >=1991

replace countryname = "Kenya" if countryname == "KENYA"
replace countryname = "Kenya" if countryname == "kenya"
capture replace stateinyeart_g=1 if countryname=="Kenya" & year >=1963

replace countryname = "Kiribati" if countryname == "KIRIBATI"
capture replace microstateinyeart_g=1 if countryname=="Kiribati" & year >=1979

replace countryname = "Korea, People's Republic of" if countryname == "Korea, People'S Republic Of"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dem."
replace countryname = "Korea, People's Republic of" if countryname == "Prk"
replace countryname = "Korea, People's Republic of" if countryname == "North Korea"
replace countryname = "Korea, People's Republic of" if countryname == "Northkorea"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dem People'S Rep"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Democratic People'S Republic Of"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dem. Rep."
replace countryname = "Korea, People's Republic of" if countryname == "Korea Dpr"
replace countryname = "Korea, People's Republic of" if countryname == "Korea Dem.People's Rep."
replace countryname = "Korea, People's Republic of" if countryname == "Korea, North"
replace countryname = "Korea, People's Republic of" if countryname == "KOREA, DEM. REP."
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dem. Rep."
replace countryname = "Korea, People's Republic of" if countryname == "KOREA, DEM."
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Democratic People's Republic of"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, DPR"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, N."
replace countryname = "Korea, People's Republic of" if countryname == "NORTH KOREA"
replace countryname = "Korea, People's Republic of" if countryname == "North Korea"
replace countryname = "Korea, People's Republic of" if countryname == "PRK"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dem."
replace countryname = "Korea, People's Republic of" if countryname == "KOREA, DEM.                                                                "
replace countryname = "Korea, People's Republic of" if countryname == "Korea North"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Democratic People'S Republic"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dem.                                                                "
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Democratic People'S Republic Of "
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dpr"
replace countryname = "Korea, People's Republic of" if countryname == "Korea Dem.People'S Rep."
replace countryname = "Korea, People's Republic of" if countryname == "Korea, Dem. Re"
replace countryname = "Korea, People's Republic of" if countryname == "Korea, North (Dem. Rep.)"
capture replace stateinyeart_g=1 if countryname=="Korea, People's Republic of" & year >=1948

replace countryname = "Korea, Republic of" if countryname == "Korea, Republic Of"
replace countryname = "Korea, Republic of" if countryname == "Korea"
replace countryname = "Korea, Republic of" if countryname == "Korea, Rep."
replace countryname = "Korea, Republic of" if countryname == "Southkorea"
replace countryname = "Korea, Republic of" if countryname == "South Korea"
replace countryname = "Korea, Republic of" if countryname == "Republic Of Korea"
replace countryname = "Korea, Republic of" if countryname == "Korea, Rep"
replace countryname = "Korea, Republic of" if countryname == "Korea Republic of"
replace countryname = "Korea, Republic of" if countryname == "KOREA, REP."
replace countryname = "Korea, Republic of" if countryname == "Korea, Rep."
replace countryname = "Korea, Republic of" if countryname == "Korea, Republic Of"
replace countryname = "Korea, Republic of" if countryname == "KOREA, REPUBLIC OF"
replace countryname = "Korea, Republic of" if countryname == "korea, republic of"
replace countryname = "Korea, Republic of" if countryname == "Korea, Republic of"
replace countryname = "Korea, Republic of" if countryname == "Korea, Republic of"
replace countryname = "Korea, Republic of" if countryname == "Korea, S."
replace countryname = "Korea, Republic of" if countryname == "Korea,Republic of"
replace countryname = "Korea, Republic of" if countryname == "REPUBLIC OF KOREA"
replace countryname = "Korea, Republic of" if countryname == "Republic of Korea"
replace countryname = "Korea, Republic of" if countryname == "SOUTH KOREA"
replace countryname = "Korea, Republic of" if countryname == "Korea"
replace countryname = "Korea, Republic of" if countryname == "Korea South"
replace countryname = "Korea, Republic of" if countryname == "South Korea"
replace countryname = "Korea, Republic of" if countryname == "KOREA, REP.                                                                "
replace countryname = "Korea, Republic of" if countryname == "Korea, Rep.                                                                "
replace countryname = "Korea, Republic of" if countryname == "Korea,Republic Of"
replace countryname = "Korea, Republic of" if countryname == "Korea, South"
replace countryname = "Korea, Republic of" if countryname == "Korea, South (Rep.)"
capture replace stateinyeart_g=1 if countryname=="Korea, Republic of" & year >=1948

*Korea
capture replace stateinyeart_g=1 if countryname=="Korea" & year >=1816 & year <=1910


replace countryname = "Kosovo" if countryname == "KOSOVO"
****Not in Gleditsch****

replace countryname = "Kuwait" if countryname == "KUWAIT"
capture replace stateinyeart_g=1 if countryname=="Kuwait" & year >=1961

replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyz Rep."
replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyz Rep"
replace countryname = "Kyrgyz Republic" if countryname == "Kyrghz Republic"
replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyz"
replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyzstan"
replace countryname = "Kyrgyz Republic" if countryname == "Kyrghz republic"
replace countryname = "Kyrgyz Republic" if countryname == "KYRGYZ REP."
replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyz Rep."
replace countryname = "Kyrgyz Republic" if countryname == "KYRGYZ REPUBLIC"
replace countryname = "Kyrgyz Republic" if countryname == "kyrgyz republic"
replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyz"
replace countryname = "Kyrgyz Republic" if countryname == "KYRGYZSTAN"
replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyzstan"
replace countryname = "Kyrgyz Republic" if countryname == "Kyrgyz Rep."
capture replace stateinyeart_g=1 if countryname=="Kyrgyz Republic" & year >=1991


*L

replace countryname = "Laos" if countryname == "Lao"
replace countryname = "Laos" if countryname == "Lao People?S Democratic Republic"
replace countryname = "Laos" if countryname == "Lao People'S Dem Rep"
replace countryname = "Laos" if countryname == "Lao People'S Dem. Rep."
replace countryname = "Laos" if countryname == "Lao People'S Democratic Republic"
replace countryname = "Laos" if countryname == "Lao Pdr"
replace countryname = "Laos" if countryname == "Lao PDR"
replace countryname = "Laos" if countryname == "LAO PEOPLE?S DEMOCRATIC REPUBLIC"
replace countryname = "Laos" if countryname == "Lao People?s Democratic Republic"
replace countryname = "Laos" if countryname == "lao people?s democratic republic"
replace countryname = "Laos" if countryname == "Lao People´s Democratic Republic"
replace countryname = "Laos" if countryname == "LAO PEOPLE'S DEM. REPUBLIC"
replace countryname = "Laos" if countryname == "LAO"
replace countryname = "Laos" if countryname == "LAOS"
replace countryname = "Laos" if countryname == "Lao, People'S Dem Rep."
replace countryname = "Laos" if countryname == "Laos Pdr"
capture replace stateinyeart_g=1 if countryname=="Laos" & year >=1954

replace countryname = "Latvia" if countryname == "LATVIA"
capture replace stateinyeart_g=1 if countryname=="Latvia" & year >=1918 & year <=1940
capture replace stateinyeart_g=1 if countryname=="Latvia" & year >=1991

replace countryname = "Lebanon" if countryname == "LEBANON"
replace countryname = "Lebanon" if countryname == "lebanon"
capture replace stateinyeart_g=1 if countryname=="Lebanon" & year >=1944

replace countryname = "Lesotho" if countryname == "LESOTHO"
capture replace stateinyeart_g=1 if countryname=="Lesotho" & year >=1966

replace countryname = "Liberia" if countryname == "LIBERIA"
capture replace stateinyeart_g=1 if countryname=="Liberia" & year >=1847

replace countryname = "Libya" if countryname == "Libyan Arab Jamahiriya"
replace countryname = "Libya" if countryname == "Libya Arab Jamahiriy"
replace countryname = "Libya" if countryname == "LIBYA"
replace countryname = "Libya" if countryname == "Libyan Arab Jamahiriya"
replace countryname = "Libya" if countryname == "LIBYAN ARAB JAMAHIRIYA"
capture replace stateinyeart_g=1 if countryname=="Libya" & year >=1816 & year <=1834
capture replace stateinyeart_g=1 if countryname=="Libya" & year >=1951

replace countryname = "Liechtenstein" if countryname == "LIECHTENSTEIN"
capture replace microstateinyeart_g=1 if countryname=="Liechtenstein" & year >=1816

replace countryname = "Lithuania" if countryname == "LITHUANIA"
replace countryname = "Lithuania" if countryname == "lithuania"
capture replace stateinyeart_g=1 if countryname=="Lithuania" & year >=1918 & year <=1940
capture replace stateinyeart_g=1 if countryname=="Lithuania" & year >=1991

replace countryname = "Luxembourg" if countryname == "LUXEMBOURG"
capture replace stateinyeart_g=1 if countryname=="Luxembourg" & year >=1867

*M

replace countryname = "Macao" if countryname == "Macao, China"
replace countryname = "Macao" if countryname == "Macau"
replace countryname = "Macao" if countryname == "MACAO"
replace countryname = "Macao" if countryname == "China,P.R.:Macao"
replace countryname = "Macao" if countryname == "Macao, China"
****Not in Gleditsch****

replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia (Former Yugoslav Republic Of)"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Fyrmacedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Fyrom-Macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia, The Fmr Yug Rp"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia, Fyr"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "The Former Yugoslav Republic Of Macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Fyr Macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia, Former Yugoslav Republic Of"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "FYROM-MACEDONIA"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "MACEDONIA"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "MACEDONIA"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "MACEDONIA, FYR"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia, FYR"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "The fmr Yug Rep Macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "The Former Yugoslav Republic of Macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "The Fmr Yug Rep Macedonia"
replace countryname = "Macedonia (Former Yugoslav Republic of)" if countryname == "Macedonia (Fmr.Yugouslavia)"
capture replace stateinyeart_g=1 if countryname=="Macedonia (Former Yugoslav Republic of)" & year >=1991

replace countryname = "Madagascar (Malagasy)" if countryname == "Madagascar"
replace countryname = "Madagascar (Malagasy)" if countryname == "MADAGASCAR"
replace countryname = "Madagascar (Malagasy)" if countryname == "madagascar"
capture replace stateinyeart_g=1 if countryname=="Madagascar (Malagasy)" & year >=1816 & year <=1896
capture replace stateinyeart_g=1 if countryname=="Madagascar (Malagasy)" & year >=1960

replace countryname = "Malawi" if countryname == "MALAWI"
replace countryname = "Malawi" if countryname == "malawi"
capture replace stateinyeart_g=1 if countryname=="Malawi" & year >=1964

replace countryname = "Malaysia" if countryname == "MALAYSIA"
replace countryname = "Malaysia" if countryname == "malaysia"
capture replace stateinyeart_g=1 if countryname=="Malaysia" & year >=1957

replace countryname = "Maldives" if countryname == "MALDIVES"
replace countryname = "Maldives" if countryname == "Maldive Islands"
capture replace stateinyeart_g=1 if countryname=="Maldives" & year >=1965

replace countryname = "Mali" if countryname == "MALI"
replace countryname = "Mali" if countryname == "mali"
capture replace stateinyeart_g=1 if countryname=="Mali" & year >=1960

replace countryname = "Malta" if countryname == "MALTA"
capture replace stateinyeart_g=1 if countryname=="Malta" & year >=1964

replace countryname = "Marshall Islands" if countryname == "MARSHALL ISLANDS"
capture replace microstateinyeart_g=1 if countryname=="Marshall Islands" & year >=1986

replace countryname = "Martinique" if countryname == "MARTINIQUE"
****Not in Gleditsch****

replace countryname = "Mauritania" if countryname == "Mauritiana"
replace countryname = "Mauritania" if countryname == "mauritania"
replace countryname = "Mauritania" if countryname == "MAURITANIA" 
capture replace stateinyeart_g=1 if countryname=="Mauritania" & year >=1960

replace countryname = "Mauritius" if countryname == "Mauritus"
replace countryname = "Mauritius" if countryname == "Maurituis"
replace countryname = "Mauritius" if countryname == "MAURITIUS"
capture replace stateinyeart_g=1 if countryname=="Mauritius" & year >=1968

replace countryname = "Mayotte" if countryname == "MAYOTTE" 
****Not in Gleditsch****

*Mecklenburg-Schwerin
capture replace stateinyeart_g=1 if countryname=="Mecklenburg-Schwerin" & year >=1816 & year <=1871

replace countryname = "Mexico" if countryname == "mexico"
replace countryname = "Mexico" if countryname == "MEXICO"
capture replace stateinyeart_g=1 if countryname=="Mexico" & year >=1821

*Modena
capture replace stateinyeart_g=1 if countryname=="Modena" & year >=1816 & year <=1861

replace countryname = "Moldova" if countryname == "Moldova, Republic Of"
replace countryname = "Moldova" if countryname == "Moldova, Rep"
replace countryname = "Moldova" if countryname == "Moldova, Rep."
replace countryname = "Moldova" if countryname == "Republic Of Moldova"
replace countryname = "Moldova" if countryname == "moldova"
replace countryname = "Moldova" if countryname == "MOLDOVA"
replace countryname = "Moldova" if countryname == "Moldova, Republic of"
replace countryname = "Moldova" if countryname == "Moldova, Republic Of"
replace countryname = "Moldova" if countryname == "Republic of Moldova"
capture replace stateinyeart_g=1 if countryname=="Moldova" & year >=1991

*Monaco
capture replace microstateinyeart_g=1 if countryname=="Monaco" & year >=1816

replace countryname = "Mongolia" if countryname == "MONGOLIA"
capture replace stateinyeart_g=1 if countryname=="Mongolia" & year >=1921

*Montenegro
capture replace stateinyeart_g=1 if countryname=="Montenegro" & year >=1868 & year <= 1915
capture replace stateinyeart_g=1 if countryname=="Montenegro" & year >=2006

replace countryname = "Montserrat" if countryname == "MONTSERRAT"
****Not in Gleditsch****

replace countryname = "Morocco" if countryname == "morocco"
replace countryname = "Morocco" if countryname == "MOROCCO"
capture replace stateinyeart_g=1 if countryname=="Morocco" & year >=1816 & year <=1904
capture replace stateinyeart_g=1 if countryname=="Morocco" & year >=1956

replace countryname = "Mozambique" if countryname == "mozambique"
replace countryname = "Mozambique" if countryname == "MOZAMBIQUE"
capture replace stateinyeart_g=1 if countryname=="Mozambique" & year >=1975

replace countryname = "Myanmar (Burma)" if countryname == "Myanmar"
replace countryname = "Myanmar (Burma)" if countryname == "Mayanmar"
replace countryname = "Myanmar (Burma)" if countryname == "MYANMAR"
replace countryname = "Myanmar (Burma)" if countryname == "Burma"
capture replace stateinyeart_g=1 if countryname=="Myanmar (Burma)" & year >=1816 & year <= 1885
capture replace stateinyeart_g=1 if countryname=="Myanmar (Burma)" & year >=1948


*N 

replace countryname = "Namibia" if countryname == "NAMIBIA"
replace countryname = "Namibia" if countryname == "namibia"
capture replace stateinyeart_g=1 if countryname=="Namibia" & year >=1990

replace countryname = "Nauru" if countryname == "NAURU"
capture replace microstateinyeart_g=1 if countryname=="Nauru" & year >=1968

replace countryname = "Nepal" if countryname == "NEPAL"
capture replace stateinyeart_g=1 if countryname=="Nepal" & year >=1816

replace countryname = "Netherlands" if countryname == "NETHERLANDS"
capture replace stateinyeart_g=1 if countryname=="Netherlands" & year >=1816

replace countryname = "New Caledonia" if countryname == "Newcaledonia"
replace countryname = "New Caledonia" if countryname == "NEW CALEDONIA"
****Not in Gleditsch****

replace countryname = "New Zealand" if countryname == "NEW ZEALAND"
capture replace stateinyeart_g=1 if countryname=="New Zealand" & year >=1907

replace countryname = "Nicaragua" if countryname == "NICARAGUA"
replace countryname = "Nicaragua" if countryname == "nicaragua"
capture replace stateinyeart_g=1 if countryname=="Nicaragua" & year >=1840

replace countryname = "Niger" if countryname == "NIGER"
capture replace stateinyeart_g=1 if countryname=="Niger" & year >=1960

replace countryname = "Nigeria" if countryname == "NIGERIA"
replace countryname = "Nigeria" if countryname == "nigeria"
capture replace stateinyeart_g=1 if countryname=="Nigeria" & year >=1960

replace countryname = "Niue" if countryname == "NIUE"
****Not in Gleditsch****

replace countryname = "Northern Marianas" if countryname == "N. Mariana Islands"
replace countryname = "Northern Marianas" if countryname == "N Mariana Islands"
replace countryname = "Northern Marianas" if countryname == "Northern Mariana Islands"
replace countryname = "Northern Marianas" if countryname == "Northern Mariana Is" 
replace countryname = "Northern Marianas" if countryname == "Northern Mariana Is." 
replace countryname = "Northern Marianas" if countryname == "NORTHERN MARIANAS"
****Not in Gleditsch****

replace countryname = "Norway" if countryname == "NORWAY"
capture replace stateinyeart_g=1 if countryname=="Norway" & year >=1905


*O 

replace countryname = "Oman" if countryname == "OMAN"
replace countryname = "Oman" if countryname == "oman"
capture replace stateinyeart_g=1 if countryname=="Oman" & year >=1816

*Orange Free State
capture replace stateinyeart_g=1 if countryname=="Orange Free State" & year >=1854 & year <=1910


*P

replace countryname = "Pakistan" if countryname == "PAKISTAN"
replace countryname = "Pakistan" if countryname == "pakistan"
capture replace stateinyeart_g=1 if countryname=="Pakistan" & year >=1947

replace countryname = "Palau" if countryname == "PALAU"
capture replace microstateinyeart_g=1 if countryname=="Palau" & year >=1994

replace countryname = "Palestine" if countryname == "West Bank And Gaza"
replace countryname = "Palestine" if countryname == "Gaza Strip (Palestine)"
replace countryname = "Palestine" if countryname == "Gaza/West Bank"
replace countryname = "Palestine" if countryname == "Gaza Strip"
replace countryname = "Palestine" if countryname == "Palestinian Admin. Areas"
replace countryname = "Palestine" if countryname == "Palestinian Admin Areas"
replace countryname = "Palestine" if countryname == "Gaza/West Bank"
replace countryname = "Palestine" if countryname == "PALESTINE"
replace countryname = "Palestine" if countryname == "PALESTINIAN ADMIN. AREAS"
replace countryname = "Palestine" if countryname == "WEST BANK AND GAZA"
replace countryname = "Palestine" if countryname == "west bank and gaza"
replace countryname = "Palestine" if countryname == "West Bank and Gaza"
****Not in Gleditsch****

replace countryname = "Panama" if countryname == "PANAMA"
replace countryname = "Panama" if countryname == "panama"
capture replace stateinyeart_g=1 if countryname=="Panama" & year >=1903

*Papal state: not the same as Holy See
capture replace stateinyeart_g=1 if countryname=="Papal States" & year >=1816 & year <=1870

replace countryname = "Papua New Guinea" if countryname == "Papau New Guinea"
replace countryname = "Papua New Guinea" if countryname == "Png"
replace countryname = "Papua New Guinea" if countryname == "Papau New Guinea"
replace countryname = "Papua New Guinea" if countryname == "PAPUA N.GUINEA"
replace countryname = "Papua New Guinea" if countryname == "PAPUA NEW GUINEA"
replace countryname = "Papua New Guinea" if countryname == "Papua N.Guinea"
replace countryname = "Papua New Guinea" if countryname == "Papua New Guin"
replace countryname = "Papua New Guinea" if countryname == "Papua New Guin."
capture replace stateinyeart_g=1 if countryname=="Papua New Guinea" & year >=1975

replace countryname = "Paraguay" if countryname == "PARAGUAY"
capture replace stateinyeart_g=1 if countryname=="Paraguay" & year >=1816

*Parma
capture replace stateinyeart_g=1 if countryname=="Parma" & year >=1816 & year<=1861

replace countryname = "Peru" if countryname == "PERU"
replace countryname = "Peru" if countryname == "peru"
capture replace stateinyeart_g=1 if countryname=="Peru" & year >=1824

replace countryname = "Philippines" if countryname == "PHILIPPINES"
replace countryname = "Philippines" if countryname == "philippines"
capture replace stateinyeart_g=1 if countryname=="Philippines" & year >=1946

replace countryname = "Poland" if countryname == "POLAND"
replace countryname = "Poland" if countryname == "poland"
capture replace stateinyeart_g=1 if countryname=="Poland" & year >=1918

replace countryname = "Portugal" if countryname == "PORTUGAL"
capture replace stateinyeart_g=1 if countryname=="Portugal" & year >=1816

replace countryname = "Puerto Rico" if countryname == "Puertorico"
replace countryname = "Puerto Rico" if countryname == "PUERTO RICO"
****Not in Gleditsch****


*Q 

replace countryname = "Qatar" if countryname == "QATAR"
capture replace stateinyeart_g=1 if countryname=="Qatar" & year >=1971


*R

replace countryname = "Reunion" if countryname=="RUnion"
replace countryname = "Reunion" if countryname=="RéUnion"
replace countryname = "Reunion" if countryname == "RÌ©Union"
replace countryname = "Reunion" if countryname == "REUNION"
replace countryname = "Reunion" if countryname == "Réunion"
****Not in Gleditsch****

replace countryname = "Rumania" if countryname == "Romania"
replace countryname = "Rumania" if countryname == "ROMANIA"
replace countryname = "Rumania" if countryname == "romania"
capture replace stateinyeart_g=1 if countryname=="Rumania" & year >=1878

replace countryname = "Russia (Soviet Union)" if countryname == "Russia"
replace countryname = "Russia (Soviet Union)" if countryname == "Europeanrussia"
replace countryname = "Russia (Soviet Union)" if countryname == "Russian Federation"
replace countryname = "Russia (Soviet Union)" if countryname == "RUSSIA"
replace countryname = "Russia (Soviet Union)" if countryname == "RUSSIAN FEDERATION"
replace countryname = "Russia (Soviet Union)" if countryname == "Russian Federation"
replace countryname = "Russia (Soviet Union)" if countryname == "russian federation"
replace countryname = "Russia (Soviet Union)" if countryname == "Russian Federation"
replace countryname = "Russia (Soviet Union)" if countryname == "SOVIET UNION"
replace countryname = "Russia (Soviet Union)" if countryname == "U.S.S.R."
replace countryname = "Russia (Soviet Union)" if countryname == "Ussr"
replace countryname = "Russia (Soviet Union)" if countryname == "Soviet Union"
replace countryname = "Russia (Soviet Union)" if countryname == "U.S.S.R./ Russia"
capture replace stateinyeart_g=1 if countryname=="Russia (Soviet Union)" & year >=1816

replace countryname = "Rwanda" if countryname == "rwanda"
replace countryname = "Rwanda" if countryname == "RWANDA"
capture replace stateinyeart_g=1 if countryname=="Rwanda" & year >=1962


*S

replace countryname = "Saint Helena" if countryname == "St. Helena"
replace countryname = "Saint Helena" if countryname == "ST. HELENA"
****Not in Gleditsch****

replace countryname = "Saint Kitts and Nevis" if countryname == "Saint Kitts And Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts & Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St Kitts & Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "Saint Kitts And Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts And Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts-Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St Kitts And Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St Kitts-Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts/Nevis" 
replace countryname = "Saint Kitts and Nevis" if countryname == "Saint Kitts and Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "Saints Kitts and Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "ST. KITTS AND NEVIS"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts and Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts and Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts/Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "ST. KITTS-NEVIS"
replace countryname = "Saint Kitts and Nevis" if countryname == "ST.KITTS&NEVIS"
replace countryname = "Saint Kitts and Nevis" if countryname == "St. Kitts & Nevis"
replace countryname = "Saint Kitts and Nevis" if countryname == "Saint Kitts & Nevis"
capture replace microstateinyeart_g=1 if countryname=="Saint Kitts and Nevis" & year >=1983

replace countryname = "Saint Lucia" if countryname == "St. Lucia"
replace countryname = "Saint Lucia" if countryname == "St Lucia"
replace countryname = "Saint Lucia" if countryname == "Saint Lucia"
replace countryname = "Saint Lucia" if countryname == "ST. LUCIA"
replace countryname = "Saint Lucia" if countryname == "ST.LUCIA"
replace countryname = "Saint Lucia" if countryname == "St. Lucia"
replace countryname = "Saint Lucia" if countryname == "St.Lucia"
capture replace microstateinyeart_g=1 if countryname=="Saint Lucia" & year >=1979

replace countryname = "Saint Maurice" if countryname == "St. Maurice"
replace countryname = "Saint Maurice" if countryname == "St Maurice"
replace countryname = "Saint Maurice" if countryname == "ST. MAURICE"
replace countryname = "Saint Maurice" if countryname == "St. Maurice"
****Not in Gleditsch****

replace countryname = "Saint Pierre and Miquelon" if countryname == "Saint Pierre And Miquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "Saint Pierre & Miquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "St. Pierre & Miquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "St Pierre & Miquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "Stpierreetmiquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "St. Pierre Et Miquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "St Pierre Et Miquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "ST. PIERRE & MIQUELON"
replace countryname = "Saint Pierre and Miquelon" if countryname == "St. Pierre & Miquelon"
replace countryname = "Saint Pierre and Miquelon" if countryname == "St. Pierre-Miquelon"
****Not in Gleditsch****

replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent And The Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent And Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent & The Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent & Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent & Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent And Gr."
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St Vincent & Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St Vincent & The Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent And Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St Vincent And Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent And The Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St Vincent And The Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent And Gr."
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St Vincent And Gr"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent/Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "ST. VINCENT AND GR."
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent & Grenadine"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent and the Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent & Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent and Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "Saint Vincent and the Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent & Gren."
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent & Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent and the Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St.Vincent & Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "ST.VINCENT&GRE"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent & the Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St. Vincent & The Grenadines"
replace countryname = "Saint Vincent and the Grenadines" if countryname == "St.Vincent & G."
capture replace microstateinyeart_g=1 if countryname=="Saint Vincent and the Grenadines" & year >=1979

replace countryname = "Samoa/Western Samoa" if countryname == "Samoa"
replace countryname = "Samoa/Western Samoa" if countryname == "Western Samoa"
replace countryname = "Samoa/Western Samoa" if countryname == "SAMOA"
replace countryname = "Samoa/Western Samoa" if countryname == "samoa"
replace countryname = "Samoa/Western Samoa" if countryname == "WESTERN SAMOA"
capture replace microstateinyeart_g=1 if countryname=="Samoa/Western Samoa" & year >=1962
****Likely to be problems here if this is used with PLAID data******

replace countryname = "San Marino" if countryname == "SAN MARINO"
capture replace microstateinyeart_g=1 if countryname=="San Marino" & year >=1816

replace countryname = "Sao Tome and Principe" if countryname == "Sao Tome & Principe"
replace countryname = "Sao Tome and Principe" if countryname == "Sao Tome And Principe"
replace countryname = "Sao Tome and Principe" if countryname == "Sao Tome"
replace countryname = "Sao Tome and Principe" if countryname == "SãO Tomé And Principe"
replace countryname = "Sao Tome and Principe" if countryname == " Sao Tome & Principe"
replace countryname = "Sao Tome and Principe" if countryname == "S Tome  Prin."
replace countryname = "Sao Tome and Principe" if countryname == "SAO TOME & PRINCIPE"
replace countryname = "Sao Tome and Principe" if countryname == "Sao Tome and Principe"
replace countryname = "Sao Tome and Principe" if countryname == "Sao Tome and Principe"
replace countryname = "Sao Tome and Principe" if countryname == "SAO TOME AND PRINCIPE"
replace countryname = "Sao Tome and Principe" if countryname == "São Tomé and Principe"
replace countryname = "Sao Tome and Principe" if countryname == "Sao Tome & Principe"
replace countryname = "Sao Tome and Principe" if countryname == "São Tomé & Príncipe"
replace countryname = "Sao Tome and Principe" if countryname == "SãO Tomé & PríNcipe"
capture replace microstateinyeart_g=1 if countryname=="Sao Tome and Principe" & year >=1975
***This is a departure from Gleditsch, where the name is "São Tomé and Principe"***

replace countryname = "Saudi Arabia" if countryname == "Saudiarabia"
replace countryname = "Saudi Arabia" if countryname == "saudi arabia"
replace countryname = "Saudi Arabia" if countryname == "SAUDI ARABIA"
capture replace stateinyeart_g=1 if countryname=="Saudia Arabia" & year >=1932

*Saxony
capture replace stateinyeart_g=1 if countryname=="Saxony" & year >=1816 & year <=1871

replace countryname = "Senegal" if countryname == "SENEGAL"
replace countryname = "Senegal" if countryname == "senegal"
capture replace stateinyeart_g=1 if countryname=="Senegal" & year >=1960

*Serbia
capture replace stateinyeart_g=1 if countryname=="Serbia" & year >=1878 & year <=1915

replace countryname = "Seychelles" if countryname == "SEYCHELLES"
capture replace microstateinyeart_g=1 if countryname=="Seychelles" & year >=1976

replace countryname = "Sierra Leone" if countryname == "Sierraleone"
replace countryname = "Sierra Leone" if countryname == "SIERRA LEONE"
capture replace stateinyeart_g=1 if countryname=="Sierra Leone" & year >=1961

replace countryname = "Singapore" if countryname == "SINGAPORE"
capture replace stateinyeart_g=1 if countryname=="Singapore" & year >=1965

replace countryname = "Slovakia" if countryname == "Slovak Republic"
replace countryname = "Slovakia" if countryname == "SLOVAK REPUBLIC"
replace countryname = "Slovakia" if countryname == "slovak republic"
replace countryname = "Slovakia" if countryname == "SLOVAKIA"
replace countryname = "Slovakia" if countryname == "Slovak Republi"
capture replace stateinyeart_g=1 if countryname=="Slovakia" & year >=1993

replace countryname = "Slovenia" if countryname == "SLOVENIA"
capture replace stateinyeart_g=1 if countryname=="Slovenia" & year >=1991

replace countryname = "Solomon Islands" if countryname == "SOLOMON IS."
replace countryname = "Solomon Islands" if countryname == "SOLOMON ISLANDS"
capture replace stateinyeart_g=1 if countryname=="Solomon Islands" & year >=1978

replace countryname = "Somalia" if countryname == "SOMALIA"
capture replace stateinyeart_g=1 if countryname=="Somalia" & year >=1960

replace countryname = "South Africa" if countryname == "Southafrica"
replace countryname = "South Africa" if countryname == "SOUTH AFRICA"
replace countryname = "South Africa" if countryname == "south africa"
replace countryname = "South Africa" if countryname == "South Africa0"
capture replace stateinyeart_g=1 if countryname=="South Africa" & year >=1910

replace countryname = "Spain" if countryname == "SPAIN"
capture replace stateinyeart_g=1 if countryname=="Spain" & year >=1816

replace countryname = "Sri Lanka (Ceylon)" if countryname == "Sri Lanka"
replace countryname = "Sri Lanka (Ceylon)" if countryname == "Srilanka"
replace countryname = "Sri Lanka (Ceylon)" if countryname == "SRI LANKA"
replace countryname = "Sri Lanka (Ceylon)" if countryname == "sri lanka"
capture replace stateinyeart_g=1 if countryname=="Sri Lanka (Ceylon)" & year >=1948

replace countryname = "Sudan" if countryname == "SUDAN"
capture replace stateinyeart_g=1 if countryname=="Sudan" & year >=1956

replace countryname = "Surinam" if countryname == "Suriname"
replace countryname = "Surinam" if countryname == "SURINAME"
capture replace stateinyeart_g=1 if countryname=="Surinam" & year >=1975

replace countryname = "Swaziland" if countryname == "SWAZILAND"
replace countryname = "Swaziland" if countryname == "swaziland"
capture replace stateinyeart_g=1 if countryname=="Swaziland" & year >=1968

replace countryname = "Sweden" if countryname == "SWEDEN"
capture replace stateinyeart_g=1 if countryname=="Sweden" & year >=1816

replace countryname = "Switzerland" if countryname == "SWITZERLAND"
capture replace stateinyeart_g=1 if countryname=="Switzerland" & year >=1816

replace countryname = "Syria" if countryname == "Syrian Arab Republic"
replace countryname = "Syria" if countryname == "Syrian Arab Rep."
replace countryname = "Syria" if countryname == "Syrian Arab Rep"
replace countryname = "Syria" if countryname == "SYRIA"
replace countryname = "Syria" if countryname == "syria"
replace countryname = "Syria" if countryname == "Syrian Arab Rep."
replace countryname = "Syria" if countryname == "Syrian Arab Republic"
replace countryname = "Syria" if countryname == "Syrian Arab Republic"
replace countryname = "Syria" if countryname == "SYRIAN ARAB REPUBLIC"
capture replace stateinyeart_g=1 if countryname=="Syria" & year >=1946


*T

replace countryname = "Taiwan" if countryname == "Taiwan, China"
replace countryname = "Taiwan" if countryname == "Chinese Taipei"
replace countryname = "Taiwan" if countryname == "Taiwan Province Of China"
replace countryname = "Taiwan" if countryname == "CHINESE TAIPEI"
replace countryname = "Taiwan" if countryname == "TAIWAN"
capture replace stateinyeart_g=1 if countryname=="Taiwan" & year >=1949

replace countryname = "Tajikistan" if countryname == "TAJIKISTAN"
replace countryname = "Tajikistan" if countryname == "tajikistan"
capture replace stateinyeart_g=1 if countryname=="Tajikistan" & year >=1991

replace countryname = "Tanzania/Tanganyika" if countryname == "Tanzania"
replace countryname = "Tanzania/Tanganyika" if countryname == "Tanzania United Republic Of"
replace countryname = "Tanzania/Tanganyika" if countryname == "Tanzinia"
replace countryname = "Tanzania/Tanganyika" if countryname == "United Republic Of Tanzania"
replace countryname = "Tanzania/Tanganyika" if countryname == "Tanzania United Republic of"
replace countryname = "Tanzania/Tanganyika" if countryname == "TANZANIA"
replace countryname = "Tanzania/Tanganyika" if countryname == "tanzania"
replace countryname = "Tanzania/Tanganyika" if countryname == "United Republic of Tanzania"
replace countryname = "Tanzania/Tanganyika" if countryname == "UNITED REPUBLIC OF TANZANIA"
capture replace stateinyeart_g=1 if countryname=="Tanzania/Tanganyika" & year >=1961

replace countryname = "Thailand" if countryname == "THAILAND"
replace countryname = "Thailand" if countryname == "thailand"
capture replace stateinyeart_g=1 if countryname=="Thailand" & year >=1816

replace countryname = "Togo" if countryname == "TOGO"
replace countryname = "Togo" if countryname == "togo"
capture replace stateinyeart_g=1 if countryname=="Togo" & year >=1960

replace countryname = "Tokelau" if countryname == "TOKELAU"
replace countryname = "Tokelau" if countryname == "Tokelau Islands"
****Not in Gleditsch****

replace countryname = "Tonga" if countryname == "TONGA"
capture replace microstateinyeart_g=1 if countryname=="Tonga" & year >=1970

*Transvaal
capture replace stateinyeart_g=1 if countryname=="Transvaal" & year >=1852 & year <=1910

replace countryname = "Trinidad and Tobago" if countryname == "Trinidad And Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad & Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad/Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "TRINIDAD & TOBAGO"
replace countryname = "Trinidad and Tobago" if countryname == "TRINIDAD & TOBAGO"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad &Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "TRINIDAD AND TOBAGO"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad and Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad and Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "TRINIDAD&TOBAGO"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad & Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad&Tobago"
replace countryname = "Trinidad and Tobago" if countryname == "Trinidad & Tob."
capture replace stateinyeart_g=1 if countryname=="Trinidad and Tobago" & year >=1962

replace countryname = "Tunisia" if countryname == "Tunisa"
replace countryname = "Tunisia" if countryname == "TUNISIA"
replace countryname = "Tunisia" if countryname == "tunisia"
capture replace stateinyeart_g=1 if countryname=="Tunisia" & year >=1816 & year <=1881
capture replace stateinyeart_g=1 if countryname=="Tunisia" & year >=1956

replace countryname = "Turkey/Ottoman Empire" if countryname == "Turkey"
replace countryname = "Turkey/Ottoman Empire" if countryname == "Tukey"
replace countryname = "Turkey/Ottoman Empire" if countryname == "Tukey"
replace countryname = "Turkey/Ottoman Empire" if countryname == "Turkey"
replace countryname = "Turkey/Ottoman Empire" if countryname == "turkey"
replace countryname = "Turkey/Ottoman Empire" if countryname == "TURKEY"
capture replace stateinyeart_g=1 if countryname=="Turkey/Ottoman Empire" & year >=1816

replace countryname = "Turkmenistan" if countryname == "turkmenistan"
replace countryname = "Turkmenistan" if countryname == "TURKMENISTAN"
replace countryname = "Turkmenistan" if countryname == "Turkemenistan"
capture replace stateinyeart_g=1 if countryname=="Turkmenistan" & year >=1991

replace countryname = "Turks and Caicos Islands" if countryname == "Turks And Caicos Islands"
replace countryname = "Turks and Caicos Islands" if countryname == "Turks & Caicos Islands"
replace countryname = "Turks and Caicos Islands" if countryname == "Turks/Caicos Islands"
replace countryname = "Turks and Caicos Islands" if countryname == "Turks & Caicos Islands"
replace countryname = "Turks and Caicos Islands" if countryname == "TURKS & CAICOS ISLANDS"
replace countryname = "Turks and Caicos Islands" if countryname == "Turks and Caicos Islands"
replace countryname = "Turks and Caicos Islands" if countryname == "Turks & Caicos Islands"
****Not in Gleditsch****

*Tuscany
capture replace stateinyeart_g=1 if countryname=="Tuscany" & year >=1816 & year<=1861

*Two Sicilies
capture replace stateinyeart_g=1 if countryname=="Two Sicilies" & year >=1816 & year <=1861

replace countryname = "Tuvalu" if countryname == "TUVALU"
capture replace microstateinyeart_g=1 if countryname=="Tuvalu" & year >=1978


*U 

replace countryname = "Uganda" if countryname == "UGANDA"
replace countryname = "Uganda" if countryname == "uganda"
capture replace stateinyeart_g=1 if countryname=="Uganda" & year >=1962

replace countryname = "Ukraine" if countryname == "UKRAINE"
replace countryname = "Ukraine" if countryname == "ukraine"
capture replace stateinyeart_g=1 if countryname=="Ukraine" & year >=1991

replace countryname = "United Arab Emirates" if countryname == "Uae"
replace countryname = "United Arab Emirates" if countryname == "UNITED ARAB E."
replace countryname = "United Arab Emirates" if countryname == "United Arab Emir."
replace countryname = "United Arab Emirates" if countryname == "UNITED ARAB EMIRATES"
replace countryname = "United Arab Emirates" if countryname == "UAE"
replace countryname = "United Arab Emirates" if countryname == "United Arab E."
replace countryname = "United Arab Emirates" if countryname == "United Arab Em."
capture replace stateinyeart_g=1 if countryname=="United Arab Emirates" & year >=1971

replace countryname = "United Kingdom" if countryname == "U.K."
replace countryname = "United Kingdom" if countryname == "United Kingdom of Great Britain and Northern Ireland"
replace countryname = "United Kingdom" if countryname == "UNITED KINGDOM"
capture replace stateinyeart_g=1 if countryname=="United Kingdom" & year >=1816

*United Provinces of Central America
capture replace stateinyeart_g=1 if countryname=="United Provinces of Central America" & year >=1823 & year<=1839

replace countryname = "United States of America" if countryname == "United States Of America"
replace countryname = "United States of America" if countryname == "United States"
replace countryname = "United States of America" if countryname == "Usa"
replace countryname = "United States of America" if countryname == "U.S.A."
replace countryname = "United States of America" if countryname == "United States of America"
replace countryname = "United States of America" if countryname == "UNITED STATES"
capture replace stateinyeart_g=1 if countryname=="United States of America" & year >=1816

replace countryname = "Uruguay" if countryname == "URUGUAY"
capture replace stateinyeart_g=1 if countryname=="Uruguay" & year >=1830

replace countryname = "Uzbekistan" if countryname == "UZBEKISTAN"
replace countryname = "Uzbekistan" if countryname == "uzbekistan"
capture replace stateinyeart_g=1 if countryname=="Uzbekistan" & year >=1991


*V

replace countryname = "Vanuatu" if countryname == "Vanutatu"
replace countryname = "Vanuatu" if countryname == "VANUATU"
capture replace microstateinyeart_g=1 if countryname=="Vanuatu" & year >=1980

replace countryname = "Venezuela" if countryname == "Venezuela Rep?blica Bolivariana de"
replace countryname = "Venezuela" if countryname == "Venezuela, Rb"
replace countryname = "Venezuela" if countryname == "Venezuela Rep?blica Bolivariana de"
replace countryname = "Venezuela" if countryname == "VENEZUELA"
replace countryname = "Venezuela" if countryname == "venezuela"
replace countryname = "Venezuela" if countryname == "Venezuela, RB"
replace countryname = "Venezuela" if countryname == "Venezuela,"
capture replace stateinyeart_g=1 if countryname=="Venezuela" & year >=1829

*Vietnam (Annam/Cochin China/Tonkin)
capture replace stateinyeart_g=1 if countryname=="Vietnam (Annam/Cochin China/Tonkin)" & year >=1816 & year<=1893

replace countryname = "Vietnam, Democratic Republic of" if countryname == "Vietnam"
replace countryname = "Vietnam, Democratic Republic of" if countryname == "Vietnam, Democratic Republic Of"
replace countryname = "Vietnam, Democratic Republic of" if countryname == "Viet nam"
replace countryname = "Vietnam, Democratic Republic of" if countryname == "VIET NAM"
replace countryname = "Vietnam, Democratic Republic of" if countryname == "VIETNAM"
replace countryname = "Vietnam, Democratic Republic of" if countryname == "vietnam"
replace countryname = "Vietnam, Democratic Republic of" if countryname == "Vietnam, Republic of"
replace countryname = "Vietnam, Democratic Republic of" if countryname =="Viet Nam"
replace countryname = "Vietnam, Democratic Republic of" if countryname =="Vietnam North"
replace countryname = "Vietnam, Democratic Republic of" if countryname =="Vietnam, North"
replace countryname = "Vietnam, Democratic Republic of" if countryname =="Vietnam, N."
capture replace stateinyeart_g=1 if countryname=="Vietnam, Democratic Republic of" & year >=1954

*Vietnam, Republic of
replace countryname = "Vietnam, Republic of" if countryname =="Vietnam, South"
replace countryname = "Vietnam, Republic of" if countryname =="Vietnam South"
replace countryname = "Vietnam, Republic of" if countryname =="Vietnam, S."
capture replace stateinyeart_g=1 if countryname=="Vietnam, Republic of" & year >=1954 & year <=1975

replace countryname = "Virgin Islands" if countryname == "Virginislands"
replace countryname = "Virgin Islands" if countryname == "United States Virgin Islands"
replace countryname = "Virgin Islands" if countryname == "Virgin Islands (U.S.)"
replace countryname = "Virgin Islands" if countryname == "Virgin Islands (Usa)"
replace countryname = "Virgin Islands" if countryname == "US VIRGIN ISLANDS"
replace countryname = "Virgin Islands" if countryname == "VIRGIN ISLANDS"
****Not in Gleditsch****


*W

replace countryname = "Wallis and Futuna" if countryname == "Wallis And Futuna"
replace countryname = "Wallis and Futuna" if countryname == "Wallis & Futuna"
replace countryname = "Wallis and Futuna" if countryname == "WALLIS & FUTUNA"
replace countryname = "Wallis and Futuna" if countryname == "Wallis and Futuna Islands"
replace countryname = "Wallis and Futuna" if countryname == "Wallis and Futuna Is."
replace countryname = "Wallis and Futuna" if countryname == "WALLIS & FUTUNA"
replace countryname = "Wallis and Futuna" if countryname == "Wallis & Futuna"
replace countryname = "Wallis and Futuna" if countryname == "Wallis-Futuna"
****Not in Gleditsch****

*Württemberg
capture replace stateinyeart_g=1 if countryname=="Württemberg" & year >=1816 & year <=1871


*X


*Y

replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen (Arab Republic Of Yemen)"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen North"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen (North)"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, North"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen (Arabl Republic Of Yemen)"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "North Yemen"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, Arab Rep."
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, Republic Of"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen Republic Of"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Republic Of Yemen"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemenunited"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, Rep."
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, Rep"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "REPUBLIC OF YEMEN"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Republic of Yemen"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "NORTH YEMEN"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen Arab Republic (North Yemen)"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, N."
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "YEMEN"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, Rep."
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen Arab Republic"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "North Yemen"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen,(Arab Republic of Yemen)"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen,(Arab Republic Of Yemen)"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen, N.Arab"
replace countryname = "Yemen (Arab Republic of Yemen)" if countryname == "Yemen Arab Republic (North, Sana)"
capture replace stateinyeart_g=1 if countryname=="Yemen (Arab Republic of Yemen)" & year >=1918

replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, People's Republic Of"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen South"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, Dem."
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, Dem"
replace countryname = "Yemen, People's Republic of" if countryname == "South Yemen"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen PDR"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen PDR."
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen P.D.R."
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen Dem"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen Dem."
replace countryname = "Yemen, People's Republic of" if countryname == "SOUTH YEMEN"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen PDR"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen People's Republic"
replace countryname = "Yemen, People's Republic of" if countryname == "South Yemen"
replace countryname = "Yemen, People's Republic of" if countryname == "YEMEN, DEM.                                                                "
replace countryname = "Yemen, People's Republic of" if countryname == "YEMEN, DEM."
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, South"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, Dem.                                                                "
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, S."
replace countryname = "Yemen, People's Republic of" if countryname == "Democratic Yemen"
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen, P.D. Rep."
replace countryname = "Yemen, People's Republic of" if countryname == "Yemen Pdr (South, Aden)"
capture replace stateinyeart_g=1 if countryname=="Yemen, People's Republic of" & year >=1967 & year <= 1990
*From WDI-"Footnote: Data for the Republic of Yemen refer to that countryname from 1990 onward; data for previous years refer to aggregated data of the former People's Democratic Republic of Yemen and the former Yemen Arab Republic unless otherwise noted."

replace countryname = "Yugoslavia (Serbia)" if countryname == "Sts Ex-Yugoslavia Unsp."
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia Sfr"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia Sfr."
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia S.f.r."
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yogoslavia"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia Fed. Rep. Of"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia Fed Rep Of"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia, Fed. Rep."
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia, Fed Rep"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia, Former"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia (Former)"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Serbia & Montenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Serbia And Montenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Serbia And Montenegro, Former"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Serbia"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia, Fr (Serbia/Montenegro)"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Serbiamontenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Serbia Montenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Fry-Serbia & Montenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Fry-Serbia And Montenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "FRY-SERBIA & MONTENEGRO"
replace countryname = "Yugoslavia (Serbia)" if countryname == "SERBIA & MONTENEGRO"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Serbia & Montenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "STS EX-YUGOSLAVIA UNSP."
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yogoslavia"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia Fed. Rep. of"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia post"
replace countryname = "Yugoslavia (Serbia)" if countryname == "YUGOSLAVIA"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia†"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia Post"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia, Sfr"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia, Fr"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Former Serbia and Montenegro"
replace countryname = "Yugoslavia (Serbia)" if countryname == "Yugoslavia2"
capture replace stateinyeart_g=1 if countryname=="Yugoslavia (Serbia)" & year >=1918


*Z

replace countryname = "Zambia" if countryname == "ZAMBIA"
replace countryname = "Zambia" if countryname == "zambia"
capture replace stateinyeart_g=1 if countryname=="Zambia" & year >=1964

*Zanzibar
capture replace stateinyeart_g=1 if countryname=="Zanzibar" & year >=1963 & year <=1964

replace countryname = "Zimbabwe (Rhodesia)" if countryname == "Zimbabwe"
replace countryname = "Zimbabwe (Rhodesia)" if countryname == "Zimbabwe (Rhodesia)"
replace countryname = "Zimbabwe (Rhodesia)" if countryname == "ZIMBABWE"
replace countryname = "Zimbabwe (Rhodesia)" if countryname == "zimbabwe"
capture replace stateinyeart_g=1 if countryname=="Zimbabwe (Rhodesia)" & year >=1965


