module TeamTavern.Shared.Timezones where

type Timezone =
    { city :: String
    , country :: String
    , name :: String
    }

allTimezones :: Array Timezone
allTimezones =
    [
        { name: "Africa/Abidjan"
        , city: "Abidjan"
        , country: "Ivory Coast"
        },
        { name: "Africa/Accra"
        , city: "Accra"
        , country: "Ghana"
        },
        { name: "Africa/Addis_Ababa"
        , city: "Addis Ababa"
        , country: "Ethiopia"
        },
        { name: "Africa/Algiers"
        , city: "Algiers"
        , country: "Algeria"
        },
        { name: "Africa/Asmara"
        , city: "Asmara"
        , country: "Eritrea"
        },
        { name: "Africa/Bamako"
        , city: "Bamako"
        , country: "Mali"
        },
        { name: "Africa/Bangui"
        , city: "Bangui"
        , country: "Central African Republic"
        },
        { name: "Africa/Banjul"
        , city: "Banjul"
        , country: "The Gambia"
        },
        { name: "Africa/Bissau"
        , city: "Bissau"
        , country: "Guinea-Bissau"
        },
        { name: "Africa/Blantyre"
        , city: "Blantyre"
        , country: "Malawi"
        },
        { name: "Africa/Brazzaville"
        , city: "Brazzaville"
        , country: "Republic of the Congo"
        },
        { name: "Africa/Bujumbura"
        , city: "Bujumbura"
        , country: "Burundi"
        },
        { name: "Africa/Cairo"
        , city: "Cairo"
        , country: "Egypt"
        },
        { name: "Africa/Casablanca"
        , city: "Casablanca"
        , country: "Morocco"
        },
        { name: "Africa/Ceuta"
        , city: "Ceuta"
        , country: "Spain"
        },
        { name: "Africa/Conakry"
        , city: "Conakry"
        , country: "Guinea"
        },
        { name: "Africa/Dakar"
        , city: "Dakar"
        , country: "Senegal"
        },
        { name: "Africa/Dar_es_Salaam"
        , city: "Dar es Salaam"
        , country: "Tanzania"
        },
        { name: "Africa/Djibouti"
        , city: "Djibouti"
        , country: "Djibouti"
        },
        { name: "Africa/Douala"
        , city: "Douala"
        , country: "Cameroon"
        },
        { name: "Africa/El_Aaiun"
        , city: "Laayoune"
        , country: "Morocco"
        },
        { name: "Africa/Freetown"
        , city: "Freetown"
        , country: "Sierra Leone"
        },
        { name: "Africa/Gaborone"
        , city: "Gaborone"
        , country: "Botswana"
        },
        { name: "Africa/Harare"
        , city: "Harare"
        , country: "Zimbabwe"
        },
        { name: "Africa/Johannesburg"
        , city: "Johannesburg"
        , country: "South Africa"
        },
        { name: "Africa/Juba"
        , city: "Juba"
        , country: "South Sudan"
        },
        { name: "Africa/Kampala"
        , city: "Kampala"
        , country: "Uganda"
        },
        { name: "Africa/Khartoum"
        , city: "Khartoum"
        , country: "Sudan"
        },
        { name: "Africa/Kigali"
        , city: "Kigali"
        , country: "Rwanda"
        },
        { name: "Africa/Kinshasa"
        , city: "Kinshasa"
        , country: "Democratic Republic of the Congo"
        },
        { name: "Africa/Lagos"
        , city: "Lagos"
        , country: "Lagos"
        },
        { name: "Africa/Libreville"
        , city: "Libreville"
        , country: "Gabon"
        },
        { name: "Africa/Lome"
        , city: "Lome"
        , country: "Togo"
        },
        { name: "Africa/Luanda"
        , city: "Luanda"
        , country: "Angola"
        },
        { name: "Africa/Lubumbashi"
        , city: "Lubumbashi"
        , country: "Democratic Republic of the Congo"
        },
        { name: "Africa/Lusaka"
        , city: "Lusaka"
        , country: "Zambia"
        },
        { name: "Africa/Malabo"
        , city: "Malabo"
        , country: "Equatorial Guinea"
        },
        { name: "Africa/Maputo"
        , city: "Maputo"
        , country: "Mozambique"
        },
        { name: "Africa/Maseru"
        , city: "Maseru"
        , country: "Lesotho"
        },
        { name: "Africa/Mbabane"
        , city: "Mbabane"
        , country: "Eswatini"
        },
        { name: "Africa/Mogadishu"
        , city: "Mogadishu"
        , country: "Somalia"
        },
        { name: "Africa/Monrovia"
        , city: "Monrovia"
        , country: "Libera"
        },
        { name: "Africa/Nairobi"
        , city: "Nairobi"
        , country: "Kenya"
        },
        { name: "Africa/Ndjamena"
        , city: "N'Djamena"
        , country: "Chad"
        },
        { name: "Africa/Niamey"
        , city: "Niamey"
        , country: "Niger"
        },
        { name: "Africa/Nouakchott"
        , city: "Nouakchott"
        , country: "Mauritania"
        },
        { name: "Africa/Ouagadougou"
        , city: "Ouagadougou"
        , country: "Burkina Faso"
        },
        { name: "Africa/Porto-Novo"
        , city: "Porto-Novo"
        , country: "Benin"
        },
        { name: "Africa/Sao_Tome"
        , city: "São Tomé"
        , country: "São Tomé and Príncipe"
        },
        { name: "Africa/Timbuktu"
        , city: "Timbuktu"
        , country: "Mali"
        },
        { name: "Africa/Tripoli"
        , city: "Tripoli"
        , country: "Libya"
        },
        { name: "Africa/Tunis"
        , city: "Tunis"
        , country: "Tunisia"
        },
        { name: "Africa/Windhoek"
        , city: "Windhoek"
        , country: "Namibia"
        },
        { name: "America/Adak"
        , city: "Adak"
        , country: "United States"
        },
        { name: "America/Anchorage"
        , city: "Anchorage"
        , country: "United States"
        },
        { name: "America/Anguilla"
        , city: "The Valley"
        , country: "Anguilla"
        },
        { name: "America/Antigua"
        , city: "Antigua"
        , country: "Antigua and Barbuda"
        },
        { name: "America/Araguaina"
        , city: "Araguaina"
        , country: "Brazil"
        },
        { name: "America/Argentina/Buenos_Aires"
        , city: "Buenos Aires"
        , country: "Argentina"
        },
        { name: "America/Argentina/Catamarca"
        , city: "Catamarca"
        , country: "Argentina"
        },
        { name: "America/Argentina/ComodRivadavia"
        , city: "Comodoro Rivadavia"
        , country: "Argentina"
        },
        { name: "America/Argentina/Cordoba"
        , city: "Córdoba"
        , country: "Argentina"
        },
        { name: "America/Argentina/Jujuy"
        , city: "San Salvador de Jujuy"
        , country: "Argentina"
        },
        { name: "America/Argentina/La_Rioja"
        , city: "La Rioja"
        , country: "Argentina"
        },
        { name: "America/Argentina/Mendoza"
        , city: "Mendoza"
        , country: "Argentina"
        },
        { name: "America/Argentina/Rio_Gallegos"
        , city: "Río Gallegos"
        , country: "Argentina"
        },
        { name: "America/Argentina/Salta"
        , city: "Salta"
        , country: "Argentina"
        },
        { name: "America/Argentina/San_Juan"
        , city: "San Juan"
        , country: "Argentina"
        },
        { name: "America/Argentina/San_Luis"
        , city: "San Luis"
        , country: "Argentina"
        },
        { name: "America/Argentina/Tucuman"
        , city: "Tucumán"
        , country: "Argentina"
        },
        { name: "America/Argentina/Ushuaia"
        , city: "Ushuaia"
        , country: "Argentina"
        },
        { name: "America/Aruba"
        , city: "Oranjestad"
        , country: "Aruba"
        },
        { name: "America/Asuncion"
        , city: "Asunción"
        , country: "Paraguay"
        },
        { name: "America/Atikokan"
        , city: "Atikokan"
        , country: "Canada"
        },
        { name: "America/Atka"
        , city: "Atka"
        , country: "United States"
        },
        { name: "America/Bahia"
        , city: "Bahia"
        , country: "Brazil"
        },
        { name: "America/Bahia_Banderas"
        , city: "Bahía de Banderas"
        , country: "Mexico"
        },
        { name: "America/Barbados"
        , city: "Bridgetown"
        , country: "Barbados"
        },
        { name: "America/Belem"
        , city: "Belém"
        , country: "Brazil"
        },
        { name: "America/Belize"
        , city: "Belmopan"
        , country: "Belize"
        },
        { name: "America/Blanc-Sablon"
        , city: "Blanc-Sablon"
        , country: "Canada"
        },
        { name: "America/Boa_Vista"
        , city: "Boa Vista"
        , country: "Brazil"
        },
        { name: "America/Bogota"
        , city: "Bogotá"
        , country: "Colombia"
        },
        { name: "America/Boise"
        , city: "Boise"
        , country: "United States"
        },
        { name: "America/Cambridge_Bay"
        , city: "Cambridge Bay"
        , country: "Canada"
        },
        { name: "America/Campo_Grande"
        , city: "Campo Grande"
        , country: "Brazil"
        },
        { name: "America/Cancun"
        , city: "Cancun"
        , country: "Mexico"
        },
        { name: "America/Caracas"
        , city: "Caracas"
        , country: "Venezuela"
        },
        { name: "America/Cayenne"
        , city: "Cayenne"
        , country: "French Guiana"
        },
        { name: "America/Cayman"
        , city: "George Town"
        , country: "Cayman"
        },
        { name: "America/Chicago"
        , city: "Chicago"
        , country: "United States"
        },
        { name: "America/Chihuahua"
        , city: "Chihuahua"
        , country: "Mexico"
        },
        { name: "America/Coral_Harbour"
        , city: "Coral Harbour"
        , country: "Canada"
        },
        { name: "America/Costa_Rica"
        , city: "San José"
        , country: "Costa Rica"
        },
        { name: "America/Creston"
        , city: "Creston"
        , country: "Canada"
        },
        { name: "America/Cuiaba"
        , city: "Cuiabá"
        , country: "Brazil"
        },
        { name: "America/Curacao"
        , city: "Willemstad"
        , country: "Curaçao"
        },
        { name: "America/Danmarkshavn"
        , city: "Danmarkshavn"
        , country: "Greenland"
        },
        { name: "America/Dawson"
        , city: "Dawson"
        , country: "Canada"
        },
        { name: "America/Dawson_Creek"
        , city: "Dawson Creek"
        , country: "Canada"
        },
        { name: "America/Denver"
        , city: "Denver"
        , country: "United States"
        },
        { name: "America/Detroit"
        , city: "Detroit"
        , country: "United States"
        },
        { name: "America/Dominica"
        , city: "Roseau"
        , country: "Dominica"
        },
        { name: "America/Edmonton"
        , city: "Edmonton"
        , country: "Canada"
        },
        { name: "America/Eirunepe"
        , city: "Eirunepé"
        , country: "Brazil"
        },
        { name: "America/El_Salvador"
        , city: "San Salvador"
        , country: "El Salvador"
        },
        { name: "America/Ensenada"
        , city: "Ensenada"
        , country: "Mexico"
        },
        { name: "America/Fort_Nelson"
        , city: "Fort Nelson"
        , country: "Canada"
        },
        { name: "America/Fort_Wayne"
        , city: "Fort Wayne"
        , country: "United States"
        },
        { name: "America/Fortaleza"
        , city: "Fortaleza"
        , country: "Brazil"
        },
        { name: "America/Glace_Bay"
        , city: "Glace Bay"
        , country: "Canada"
        },
        { name: "America/Godthab"
        , city: "Nuuk"
        , country: "Greenland"
        },
        { name: "America/Goose_Bay"
        , city: "Goose Bay"
        , country: "Canada"
        },
        { name: "America/Grand_Turk"
        , city: "Cockburn Town"
        , country: "Turks and Caicos Islands"
        },
        { name: "America/Grenada"
        , city: "St. George's"
        , country: "Grenada"
        },
        { name: "America/Guadeloupe"
        , city: "Basse-Terre"
        , country: "Guadeloupe"
        },
        { name: "America/Guatemala"
        , city: "Guatemala City"
        , country: "Guatemala"
        },
        { name: "America/Guayaquil"
        , city: "Guayaquil"
        , country: "Ecuador"
        },
        { name: "America/Guyana"
        , city: "Georgetown"
        , country: "Guyana"
        },
        { name: "America/Halifax"
        , city: "Halifax"
        , country: "Canada"
        },
        { name: "America/Havana"
        , city: "Havana"
        , country: "Cuba"
        },
        { name: "America/Hermosillo"
        , city: "Hermosillo"
        , country: "Mexico"
        },
        { name: "America/Indiana/Indianapolis"
        , city: "Indianapolis"
        , country: "United States"
        },
        { name: "America/Indiana/Knox"
        , city: "Knox"
        , country: "United States"
        },
        { name: "America/Indiana/Marengo"
        , city: "Marengo"
        , country: "United States"
        },
        { name: "America/Indiana/Petersburg"
        , city: "Petersburg"
        , country: "United States"
        },
        { name: "America/Indiana/Tell_City"
        , city: "Tell City"
        , country: "United States"
        },
        { name: "America/Indiana/Vevay"
        , city: "Vevay"
        , country: "United States"
        },
        { name: "America/Indiana/Vincennes"
        , city: "Vincennes"
        , country: "United States"
        },
        { name: "America/Indiana/Winamac"
        , city: "Winamac"
        , country: "United States"
        },
        { name: "America/Indianapolis"
        , city: "Indianapolis"
        , country: "United States"
        },
        { name: "America/Inuvik"
        , city: "Inuvik"
        , country: "Canada"
        },
        { name: "America/Iqaluit"
        , city: "Iqaluit"
        , country: "Canada"
        },
        { name: "America/Jamaica"
        , city: "Kingston"
        , country: "Jamaica"
        },
        { name: "America/Juneau"
        , city: "Juneau"
        , country: "United States"
        },
        { name: "America/Kentucky/Louisville"
        , city: "Louisville"
        , country: "United States"
        },
        { name: "America/Kentucky/Monticello"
        , city: "Monticello"
        , country: "United States"
        },
        { name: "America/Kralendijk"
        , city: "Kralendijk"
        , country: "Bonaire"
        },
        { name: "America/La_Paz"
        , city: "La Paz"
        , country: "Bolivia"
        },
        { name: "America/Lima"
        , city: "Lima"
        , country: "Peru"
        },
        { name: "America/Los_Angeles"
        , city: "Los Angeles"
        , country: "United States"
        },
        { name: "America/Lower_Princes"
        , city: "Philipsburg"
        , country: "Sint Maarten"
        },
        { name: "America/Maceio"
        , city: "Maceió"
        , country: "Brazil"
        },
        { name: "America/Managua"
        , city: "Managua"
        , country: "Nicaragua"
        },
        { name: "America/Manaus"
        , city: "Manaus"
        , country: "Brazil"
        },
        { name: "America/Marigot"
        , city: "Marigot"
        , country: "Saint Martin"
        },
        { name: "America/Martinique"
        , city: "Fort-de-France"
        , country: "Martinique"
        },
        { name: "America/Matamoros"
        , city: "Matamoros"
        , country: "Mexico"
        },
        { name: "America/Mazatlan"
        , city: "Mazatlan"
        , country: "Mexico"
        },
        { name: "America/Menominee"
        , city: "Menominee"
        , country: "United States"
        },
        { name: "America/Merida"
        , city: "Merida"
        , country: "Mexico"
        },
        { name: "America/Metlakatla"
        , city: "Metlakatla"
        , country: "United States"
        },
        { name: "America/Mexico_City"
        , city: "Mexico City"
        , country: "Mexico"
        },
        { name: "America/Miquelon"
        , city: "Saint-Pierre"
        , country: "Saint Pierre and Miquelon"
        },
        { name: "America/Moncton"
        , city: "Moncton"
        , country: "Canada"
        },
        { name: "America/Monterrey"
        , city: "Monterrey"
        , country: "Mexico"
        },
        { name: "America/Montevideo"
        , city: "Montevideo"
        , country: "Uruguay"
        },
        { name: "America/Montreal"
        , city: "Montreal"
        , country: "Canada"
        },
        { name: "America/Montserrat"
        , city: "Brades"
        , country: "Montserrat"
        },
        { name: "America/Nassau"
        , city: "Nassau"
        , country: "The Bahamas"
        },
        { name: "America/New_York"
        , city: "New York"
        , country: "United States"
        },
        { name: "America/Nipigon"
        , city: "Nipigon"
        , country: "Canada"
        },
        { name: "America/Nome"
        , city: "Nome"
        , country: "United States"
        },
        { name: "America/Noronha"
        , city: "Fernando de Noronha"
        , country: "Brazil"
        },
        { name: "America/North_Dakota/Beulah"
        , city: "Beulah"
        , country: "United States"
        },
        { name: "America/North_Dakota/Center"
        , city: "Center"
        , country: "United States"
        },
        { name: "America/North_Dakota/New_Salem"
        , city: "New Salem"
        , country: "United States"
        },
        { name: "America/Ojinaga"
        , city: "Ojinaga"
        , country: "Mexico"
        },
        { name: "America/Panama"
        , city: "Panama City"
        , country: "Panama"
        },
        { name: "America/Pangnirtung"
        , city: "Pangnirtung"
        , country: "Canada"
        },
        { name: "America/Paramaribo"
        , city: "Paramaribo"
        , country: "Suriname"
        },
        { name: "America/Phoenix"
        , city: "Phoenix"
        , country: "United States"
        },
        { name: "America/Port-au-Prince"
        , city: "Port-au-Prince"
        , country: "Haiti"
        },
        { name: "America/Port_of_Spain"
        , city: "Port of Spain"
        , country: "Trinidad and Tobago"
        },
        { name: "America/Porto_Acre"
        , city: "Porto Acre"
        , country: "Brazil"
        },
        { name: "America/Porto_Velho"
        , city: "Porto Velho"
        , country: "Brazil"
        },
        { name: "America/Puerto_Rico"
        , city: "San Juan"
        , country: "Puerto Rico"
        },
        { name: "America/Punta_Arenas"
        , city: "Punta Arenas"
        , country: "Chile"
        },
        { name: "America/Rainy_River"
        , city: "Rainy River"
        , country: "Canada"
        },
        { name: "America/Rankin_Inlet"
        , city: "Rankin Inlet"
        , country: "Canada"
        },
        { name: "America/Recife"
        , city: "Recife"
        , country: "Brazil"
        },
        { name: "America/Regina"
        , city: "Regina"
        , country: "Canada"
        },
        { name: "America/Resolute"
        , city: "Resolute"
        , country: "Canada"
        },
        { name: "America/Rio_Branco"
        , city: "Rio Branco"
        , country: "Brazil"
        },
        { name: "America/Rosario"
        , city: "Rosario"
        , country: "Argentina"
        },
        { name: "America/Santa_Isabel"
        , city: "Santa Isabel"
        , country: "Mexico"
        },
        { name: "America/Santarem"
        , city: "Santarém"
        , country: "Brazil"
        },
        { name: "America/Santiago"
        , city: "Santiago"
        , country: "Chile"
        },
        { name: "America/Santo_Domingo"
        , city: "Santo Domingo"
        , country: "Dominican Republic"
        },
        { name: "America/Sao_Paulo"
        , city: "São Paulo"
        , country: "Brazil"
        },
        { name: "America/Scoresbysund"
        , city: "Ittoqqortoormiit"
        , country: "Greenland"
        },
        { name: "America/Shiprock"
        , city: "Shiprock"
        , country: "United States"
        },
        { name: "America/Sitka"
        , city: "Sitka"
        , country: "United States"
        },
        { name: "America/St_Barthelemy"
        , city: "Gustavia"
        , country: "Saint Barthélemy"
        },
        { name: "America/St_Johns"
        , city: "St. John's"
        , country: "Canada"
        },
        { name: "America/St_Kitts"
        , city: "Basseterre"
        , country: "Saint Kitts and Nevis"
        },
        { name: "America/St_Lucia"
        , city: "Castries"
        , country: "Saint Lucia"
        },
        { name: "America/St_Thomas"
        , city: "Charlotte Amalie"
        , country: "United States Virgin Islands"
        },
        { name: "America/St_Vincent"
        , city: "Kingstown"
        , country: "Saint Vincent and the Grenadines"
        },
        { name: "America/Swift_Current"
        , city: "Swift Current"
        , country: "Canada"
        },
        { name: "America/Tegucigalpa"
        , city: "Tegucigalpa"
        , country: "Honduras"
        },
        { name: "America/Thule"
        , city: "Qaanaaq"
        , country: "Greenland"
        },
        { name: "America/Thunder_Bay"
        , city: "Thunder Bay"
        , country: "Canada"
        },
        { name: "America/Tijuana"
        , city: "Tijuana"
        , country: "Mexico"
        },
        { name: "America/Toronto"
        , city: "Toronto"
        , country: "Canada"
        },
        { name: "America/Tortola"
        , city: "Road Town"
        , country: "British Virgin Islands"
        },
        { name: "America/Vancouver"
        , city: "Vancouver"
        , country: "Canada"
        },
        { name: "America/Whitehorse"
        , city: "Whitehorse"
        , country: "Canada"
        },
        { name: "America/Winnipeg"
        , city: "Winnipeg"
        , country: "Canada"
        },
        { name: "America/Yakutat"
        , city: "Yakutat"
        , country: "United States"
        },
        { name: "America/Yellowknife"
        , city: "Yellowknife"
        , country: "Canada"
        },
        { name: "Antarctica/Casey"
        , city: "Casey Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/Davis"
        , city: "Davis Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/DumontDUrville"
        , city: "Dumont-d'Urville Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/Macquarie"
        , city: "Macquarie Island"
        , country: "Antarctica"
        },
        { name: "Antarctica/Mawson"
        , city: "Mawson Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/McMurdo"
        , city: "McMurdo Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/Palmer"
        , city: "Palmer Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/Rothera"
        , city: "Rothera Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/Syowa"
        , city: "Showa Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/Troll"
        , city: "Troll Station"
        , country: "Antarctica"
        },
        { name: "Antarctica/Vostok"
        , city: "Vostok Station"
        , country: "Antarctica"
        },
        { name: "Arctic/Longyearbyen"
        , city: "Longyearbyen"
        , country: "Norway"
        },
        { name: "Asia/Aden"
        , city: "Aden"
        , country: "Yemen"
        },
        { name: "Asia/Almaty"
        , city: "Almaty"
        , country: "Kazakhstan"
        },
        { name: "Asia/Amman"
        , city: "Amman"
        , country: "Jordan"
        },
        { name: "Asia/Anadyr"
        , city: "Anadyr"
        , country: "Russia"
        },
        { name: "Asia/Aqtau"
        , city: "Aqtau"
        , country: "Kazakhstan"
        },
        { name: "Asia/Aqtobe"
        , city: "Aqtobe"
        , country: "Kazakhstan"
        },
        { name: "Asia/Ashgabat"
        , city: "Ashgabat"
        , country: "Turkmenistan"
        },
        { name: "Asia/Atyrau"
        , city: "Atyrau"
        , country: "Kazakhstan"
        },
        { name: "Asia/Baghdad"
        , city: "Baghdad"
        , country: "Iraq"
        },
        { name: "Asia/Bahrain"
        , city: "Manama"
        , country: "Bahrain"
        },
        { name: "Asia/Baku"
        , city: "Baku"
        , country: "Azerbaijan"
        },
        { name: "Asia/Bangkok"
        , city: "Bangkok"
        , country: "Thailand"
        },
        { name: "Asia/Barnaul"
        , city: "Barnaul"
        , country: "Russia"
        },
        { name: "Asia/Beirut"
        , city: "Beirut"
        , country: "Lebanon"
        },
        { name: "Asia/Bishkek"
        , city: "Bishkek"
        , country: "Kyrgyzstan"
        },
        { name: "Asia/Brunei"
        , city: "Bandar Seri Begawan"
        , country: "Brunei"
        },
        { name: "Asia/Chita"
        , city: "Chita"
        , country: "Russia"
        },
        { name: "Asia/Choibalsan"
        , city: "Choibalsan"
        , country: "Mongolia"
        },
        { name: "Asia/Chongqing"
        , city: "Chongqing"
        , country: "China"
        },
        { name: "Asia/Colombo"
        , city: "Colombo"
        , country: "Sri Lanka"
        },
        { name: "Asia/Damascus"
        , city: "Damascus"
        , country: "Syria"
        },
        { name: "Asia/Dhaka"
        , city: "Dhaka"
        , country: "Bangladesh"
        },
        { name: "Asia/Dili"
        , city: "Dili"
        , country: "East Timor"
        },
        { name: "Asia/Dubai"
        , city: "Dubai"
        , country: "United Arab Emirates"
        },
        { name: "Asia/Dushanbe"
        , city: "Dushanbe"
        , country: "Tajikistan"
        },
        { name: "Asia/Famagusta"
        , city: "Famagusta"
        , country: "Cyprus"
        },
        { name: "Asia/Gaza"
        , city: "Gaza"
        , country: "Palestine"
        },
        { name: "Asia/Harbin"
        , city: "Harbin"
        , country: "China"
        },
        { name: "Asia/Hebron"
        , city: "Hebron"
        , country: "Palestine"
        },
        { name: "Asia/Ho_Chi_Minh"
        , city: "Ho Chi Minh City"
        , country: "Vietnam"
        },
        { name: "Asia/Hong_Kong"
        , city: "Hong Kong"
        , country: "Hong Kong"
        },
        { name: "Asia/Hovd"
        , city: "Khovd"
        , country: "Mongolia"
        },
        { name: "Asia/Irkutsk"
        , city: "Irkutsk"
        , country: "Russia"
        },
        { name: "Asia/Istanbul"
        , city: "Istanbul"
        , country: "Turkey"
        },
        { name: "Asia/Jakarta"
        , city: "Jakarta"
        , country: "Indonesia"
        },
        { name: "Asia/Jayapura"
        , city: "Jayapura"
        , country: "Indonesia"
        },
        { name: "Asia/Jerusalem"
        , city: "Jerusalem"
        , country: "Israel"
        },
        { name: "Asia/Kabul"
        , city: "Kabul"
        , country: "Afghanistan"
        },
        { name: "Asia/Kamchatka"
        , city: "Petropavlovsk-Kamchatsky"
        , country: "Russia"
        },
        { name: "Asia/Karachi"
        , city: "Karachi"
        , country: "Pakistan"
        },
        { name: "Asia/Kashgar"
        , city: "Kashgar"
        , country: "China"
        },
        { name: "Asia/Kathmandu"
        , city: "Kathmandu"
        , country: "Nepal"
        },
        { name: "Asia/Khandyga"
        , city: "Khandyga"
        , country: "Russia"
        },
        { name: "Asia/Kolkata"
        , city: "Kolkata"
        , country: "India"
        },
        { name: "Asia/Krasnoyarsk"
        , city: "Krasnoyarsk"
        , country: "Russia"
        },
        { name: "Asia/Kuala_Lumpur"
        , city: "Kuala Lumpur"
        , country: "Malaysia"
        },
        { name: "Asia/Kuching"
        , city: "Kuching"
        , country: "Malaysia"
        },
        { name: "Asia/Kuwait"
        , city: "Kuwait City"
        , country: "Kuwait"
        },
        { name: "Asia/Macau"
        , city: "Macau"
        , country: "Macau"
        },
        { name: "Asia/Magadan"
        , city: "Magadan"
        , country: "Russia"
        },
        { name: "Asia/Makassar"
        , city: "Makassar"
        , country: "Indonesia"
        },
        { name: "Asia/Manila"
        , city: "Manila"
        , country: "Philippines"
        },
        { name: "Asia/Muscat"
        , city: "Muscat"
        , country: "Oman"
        },
        { name: "Asia/Nicosia"
        , city: "Nicosia"
        , country: "Cyprus"
        },
        { name: "Asia/Novokuznetsk"
        , city: "Novokuznetsk"
        , country: "Russia"
        },
        { name: "Asia/Novosibirsk"
        , city: "Novosibirsk"
        , country: "Russia"
        },
        { name: "Asia/Omsk"
        , city: "Omsk"
        , country: "Russia"
        },
        { name: "Asia/Oral"
        , city: "Oral"
        , country: "Kazakhstan"
        },
        { name: "Asia/Phnom_Penh"
        , city: "Phnom Penh"
        , country: "Cambodia"
        },
        { name: "Asia/Pontianak"
        , city: "Pontianak"
        , country: "Indonesia"
        },
        { name: "Asia/Pyongyang"
        , city: "Pyongyang"
        , country: "North Korea"
        },
        { name: "Asia/Qatar"
        , city: "Doha"
        , country: "Qatar"
        },
        { name: "Asia/Qyzylorda"
        , city: "Qyzylorda"
        , country: "Kazakhstan"
        },
        { name: "Asia/Riyadh"
        , city: "Riyadh"
        , country: "Saudi Arabia"
        },
        { name: "Asia/Sakhalin"
        , city: "Yuzhno-Sakhalinsk"
        , country: "Russia"
        },
        { name: "Asia/Samarkand"
        , city: "Samarkand"
        , country: "Uzbekistan"
        },
        { name: "Asia/Seoul"
        , city: "Seoul"
        , country: "South Korea"
        },
        { name: "Asia/Shanghai"
        , city: "Shanghai"
        , country: "China"
        },
        { name: "Asia/Singapore"
        , city: "Singapore"
        , country: "Singapore"
        },
        { name: "Asia/Srednekolymsk"
        , city: "Srednekolymsk"
        , country: "Russia"
        },
        { name: "Asia/Taipei"
        , city: "Taipei"
        , country: "Taiwan"
        },
        { name: "Asia/Tashkent"
        , city: "Tashkent"
        , country: "Uzbekistan"
        },
        { name: "Asia/Tbilisi"
        , city: "Tbilisi"
        , country: "Georgia"
        },
        { name: "Asia/Tehran"
        , city: "Tehran"
        , country: "Iran"
        },
        { name: "Asia/Tel_Aviv"
        , city: "Tel Aviv"
        , country: "Israel"
        },
        { name: "Asia/Thimphu"
        , city: "Thimphu"
        , country: "Bhutan"
        },
        { name: "Asia/Tokyo"
        , city: "Tokyo"
        , country: "Japan"
        },
        { name: "Asia/Tomsk"
        , city: "Tomsk"
        , country: "Russia"
        },
        { name: "Asia/Ujung_Pandang"
        , city: "Makassar"
        , country: "Indonesia"
        },
        { name: "Asia/Ulaanbaatar"
        , city: "Ulaanbaatar"
        , country: "Mongolia"
        },
        { name: "Asia/Urumqi"
        , city: "Ürümqi"
        , country: "China"
        },
        { name: "Asia/Ust-Nera"
        , city: "Ust-Nera"
        , country: "Russia"
        },
        { name: "Asia/Vientiane"
        , city: "Vientiane"
        , country: "Laos"
        },
        { name: "Asia/Vladivostok"
        , city: "Vladivostok"
        , country: "Russia"
        },
        { name: "Asia/Yakutsk"
        , city: "Yakutsk"
        , country: "Russia"
        },
        { name: "Asia/Yangon"
        , city: "Yangon"
        , country: "Myanmar"
        },
        { name: "Asia/Yekaterinburg"
        , city: "Yekaterinburg"
        , country: "Russia"
        },
        { name: "Asia/Yerevan"
        , city: "Yerevan"
        , country: "Armenia"
        },
        { name: "Atlantic/Azores"
        , city: "Ponta Delgada"
        , country: "Azores"
        },
        { name: "Atlantic/Bermuda"
        , city: "Hamilton"
        , country: "Hamilton"
        },
        { name: "Atlantic/Canary"
        , city: "Las Palmas"
        , country: "Canary Islands"
        },
        { name: "Atlantic/Cape_Verde"
        , city: "Praia"
        , country: "Cape Verde"
        },
        { name: "Atlantic/Faroe"
        , city: "Tórshavn"
        , country: "Faroe Islands"
        },
        { name: "Atlantic/Jan_Mayen"
        , city: "Jan Mayen"
        , country: "Norway"
        },
        { name: "Atlantic/Madeira"
        , city: "Funchal"
        , country: "Madeira"
        },
        { name: "Atlantic/Reykjavik"
        , city: "Reykjavik"
        , country: "Iceland"
        },
        { name: "Atlantic/South_Georgia"
        , city: "King Edward Point"
        , country: "South Georgia and the South Sandwich Islands"
        },
        { name: "Atlantic/St_Helena"
        , city: "Jamestown"
        , country: "Saint Helena"
        },
        { name: "Atlantic/Stanley"
        , city: "Stanley"
        , country: "Falkland Islands"
        },
        { name: "Australia/Adelaide"
        , city: "Adelaide"
        , country: "Australia"
        },
        { name: "Australia/Brisbane"
        , city: "Brisbane"
        , country: "Australia"
        },
        { name: "Australia/Broken_Hill"
        , city: "Broken Hill"
        , country: "Australia"
        },
        { name: "Australia/Canberra"
        , city: "Canberra"
        , country: "Australia"
        },
        { name: "Australia/Currie"
        , city: "Currie"
        , country: "Australia"
        },
        { name: "Australia/Darwin"
        , city: "Darwin"
        , country: "Australia"
        },
        { name: "Australia/Eucla"
        , city: "Eucla"
        , country: "Australia"
        },
        { name: "Australia/Hobart"
        , city: "Hobart"
        , country: "Australia"
        },
        { name: "Australia/Lindeman"
        , city: "Lindeman"
        , country: "Australia"
        },
        { name: "Australia/Lord_Howe"
        , city: "Lord Howe Island"
        , country: "Australia"
        },
        { name: "Australia/Melbourne"
        , city: "Melbourne"
        , country: "Australia"
        },
        { name: "Australia/Perth"
        , city: "Perth"
        , country: "Australia"
        },
        { name: "Australia/Sydney"
        , city: "Sydney"
        , country: "Australia"
        },
        { name: "Europe/Amsterdam"
        , city: "Amsterdam"
        , country: "Netherlands"
        },
        { name: "Europe/Andorra"
        , city: "Andorra la Vella"
        , country: "Andorra"
        },
        { name: "Europe/Astrakhan"
        , city: "Astrakhan"
        , country: "Russia"
        },
        { name: "Europe/Athens"
        , city: "Athens"
        , country: "Greece"
        },
        { name: "Europe/Belfast"
        , city: "Belfast"
        , country: "United Kingdom"
        },
        { name: "Europe/Belgrade"
        , city: "Belgrade"
        , country: "Serbia"
        },
        { name: "Europe/Berlin"
        , city: "Berlin"
        , country: "Germany"
        },
        { name: "Europe/Bratislava"
        , city: "Bratislava"
        , country: "Slovakia"
        },
        { name: "Europe/Brussels"
        , city: "Brussels"
        , country: "Belgium"
        },
        { name: "Europe/Bucharest"
        , city: "Bucharest"
        , country: "Romania"
        },
        { name: "Europe/Budapest"
        , city: "Budapest"
        , country: "Hungary"
        },
        { name: "Europe/Busingen"
        , city: "Büsingen"
        , country: "Germany"
        },
        { name: "Europe/Chisinau"
        , city: "Chișinău"
        , country: "Moldova"
        },
        { name: "Europe/Copenhagen"
        , city: "Copenhagen"
        , country: "Denmark"
        },
        { name: "Europe/Dublin"
        , city: "Dublin"
        , country: "Ireland"
        },
        { name: "Europe/Gibraltar"
        , city: "Gibraltar"
        , country: "Gibraltar"
        },
        { name: "Europe/Guernsey"
        , city: "Saint Peter Port"
        , country: "Guernsey"
        },
        { name: "Europe/Helsinki"
        , city: "Helsinki"
        , country: "Finland"
        },
        { name: "Europe/Isle_of_Man"
        , city: "Douglas"
        , country: "Isle of Man"
        },
        { name: "Europe/Istanbul"
        , city: "Istanbul"
        , country: "Turkey"
        },
        { name: "Europe/Jersey"
        , city: "Saint Helier"
        , country: "Jersey"
        },
        { name: "Europe/Kaliningrad"
        , city: "Kaliningrad"
        , country: "Russia"
        },
        { name: "Europe/Kiev"
        , city: "Kiev"
        , country: "Ukraine"
        },
        { name: "Europe/Kirov"
        , city: "Kirov"
        , country: "Russia"
        },
        { name: "Europe/Lisbon"
        , city: "Lisbon"
        , country: "Portugal"
        },
        { name: "Europe/Ljubljana"
        , city: "Ljubljana"
        , country: "Slovenia"
        },
        { name: "Europe/London"
        , city: "London"
        , country: "United Kingdom"
        },
        { name: "Europe/Luxembourg"
        , city: "Luxembourg City"
        , country: "Luxembourg"
        },
        { name: "Europe/Madrid"
        , city: "Madrid"
        , country: "Spaid"
        },
        { name: "Europe/Malta"
        , city: "Valletta"
        , country: "Malta"
        },
        { name: "Europe/Mariehamn"
        , city: "Mariehamn"
        , country: "Åland Islands"
        },
        { name: "Europe/Minsk"
        , city: "Minsk"
        , country: "Belarus"
        },
        { name: "Europe/Monaco"
        , city: "Monaco City"
        , country: "Monaco"
        },
        { name: "Europe/Moscow"
        , city: "Moscow"
        , country: "Russia"
        },
        { name: "Europe/Oslo"
        , city: "Oslo"
        , country: "Norway"
        },
        { name: "Europe/Paris"
        , city: "Paris"
        , country: "France"
        },
        { name: "Europe/Podgorica"
        , city: "Podgorica"
        , country: "Montenegro"
        },
        { name: "Europe/Prague"
        , city: "Prague"
        , country: "Czech Republic"
        },
        { name: "Europe/Riga"
        , city: "Riga"
        , country: "Latvia"
        },
        { name: "Europe/Rome"
        , city: "Rome"
        , country: "Italy"
        },
        { name: "Europe/Samara"
        , city: "Samara"
        , country: "Russia"
        },
        { name: "Europe/San_Marino"
        , city: "San Marino"
        , country: "San Marino"
        },
        { name: "Europe/Sarajevo"
        , city: "Sarajevo"
        , country: "Bosnia and Herzegovina"
        },
        { name: "Europe/Saratov"
        , city: "Saratov"
        , country: "Russia"
        },
        { name: "Europe/Simferopol"
        , city: "Simferopol"
        , country: "Russia"
        },
        { name: "Europe/Skopje"
        , city: "Skopje"
        , country: "North Macedonia"
        },
        { name: "Europe/Sofia"
        , city: "Sofia"
        , country: "Bulgaria"
        },
        { name: "Europe/Stockholm"
        , city: "Stockholm"
        , country: "Sweden"
        },
        { name: "Europe/Tallinn"
        , city: "Tallinn"
        , country: "Estonia"
        },
        { name: "Europe/Tirane"
        , city: "Tirane"
        , country: "Albania"
        },
        { name: "Europe/Tiraspol"
        , city: "Tiraspol"
        , country: "Moldova"
        },
        { name: "Europe/Ulyanovsk"
        , city: "Ulyanovsk"
        , country: "Russia"
        },
        { name: "Europe/Uzhgorod"
        , city: "Uzhgorod"
        , country: "Ukraine"
        },
        { name: "Europe/Vaduz"
        , city: "Vaduz"
        , country: "Liechtenstein"
        },
        { name: "Europe/Vatican"
        , city: "Vatican City"
        , country: "Vatican City"
        },
        { name: "Europe/Vienna"
        , city: "Vienna"
        , country: "Austria"
        },
        { name: "Europe/Vilnius"
        , city: "Vilnius"
        , country: "Lithuania"
        },
        { name: "Europe/Volgograd"
        , city: "Volgograd"
        , country: "Russia"
        },
        { name: "Europe/Warsaw"
        , city: "Warsaw"
        , country: "Poland"
        },
        { name: "Europe/Zagreb"
        , city: "Zagreb"
        , country: "Croatia"
        },
        { name: "Europe/Zaporozhye"
        , city: "Zaporizhia"
        , country: "Ukraine"
        },
        { name: "Europe/Zurich"
        , city: "Zürich"
        , country: "Switzerland"
        },
        { name: "Indian/Antananarivo"
        , city: "Antananarivo"
        , country: "Madagascar"
        },
        { name: "Indian/Chagos"
        , city: "Diego Garcia"
        , country: "Chagos Archipelago"
        },
        { name: "Indian/Christmas"
        , city: "Flying Fish Cove"
        , country: "Christmas Island"
        },
        { name: "Indian/Cocos"
        , city: "West Island"
        , country: "Cocos (Keeling) Islands"
        },
        { name: "Indian/Comoro"
        , city: "Moroni"
        , country: "Comoros"
        },
        { name: "Indian/Kerguelen"
        , city: "Port-aux-Français"
        , country: "Kerguelen Islands"
        },
        { name: "Indian/Mahe"
        , city: "Victoria"
        , country: "Seychelles"
        },
        { name: "Indian/Maldives"
        , city: "Malé"
        , country: "Maldives"
        },
        { name: "Indian/Mauritius"
        , city: "Port Louis"
        , country: "Mauritius"
        },
        { name: "Indian/Mayotte"
        , city: "Mamoudzou"
        , country: "Mayotte"
        },
        { name: "Indian/Reunion"
        , city: "Saint-Denis"
        , country: "Réunion"
        },
        { name: "Pacific/Apia"
        , city: "Apia"
        , country: "Samoa"
        },
        { name: "Pacific/Auckland"
        , city: "Auckland"
        , country: "New Zealand"
        },
        { name: "Pacific/Bougainville"
        , city: "Buka"
        , country: "Papua New Guinea"
        },
        { name: "Pacific/Chatham"
        , city: "Chatham Islands"
        , country: "New Zealand"
        },
        { name: "Pacific/Chuuk"
        , city: "Weno"
        , country: "Federated States of Micronesia"
        },
        { name: "Pacific/Easter"
        , city: "Easter Island"
        , country: "Chile"
        },
        { name: "Pacific/Efate"
        , city: "Port Vila"
        , country: "Vanuatu"
        },
        { name: "Pacific/Enderbury"
        , city: "Phoenix Islands"
        , country: "Kiribati"
        },
        { name: "Pacific/Fakaofo"
        , city: "Atafu"
        , country: "Tokelau"
        },
        { name: "Pacific/Fiji"
        , city: "Suva"
        , country: "Fiji"
        },
        { name: "Pacific/Funafuti"
        , city: "Funafuti"
        , country: "Tuvalu"
        },
        { name: "Pacific/Galapagos"
        , city: "Galápagos Islands"
        , country: "Ecuador"
        },
        { name: "Pacific/Gambier"
        , city: "Rikitea"
        , country: "French Polynesia"
        },
        { name: "Pacific/Guadalcanal"
        , city: "Honiara"
        , country: "Solomon Islands"
        },
        { name: "Pacific/Guam"
        , city: "Hagåtña"
        , country: "Guam"
        },
        { name: "Pacific/Honolulu"
        , city: "Honolulu"
        , country: "United States"
        },
        { name: "Pacific/Kiritimati"
        , city: "Kiritimati"
        , country: "Kiribati"
        },
        { name: "Pacific/Kosrae"
        , city: "Tofol"
        , country: "Federated States of Micronesia"
        },
        { name: "Pacific/Kwajalein"
        , city: "Kwajalein Atoll"
        , country: "Marshall Islands"
        },
        { name: "Pacific/Majuro"
        , city: "Majuro"
        , country: "Marshall Islands"
        },
        { name: "Pacific/Marquesas"
        , city: "Taiohae"
        , country: "French Polynesia"
        },
        { name: "Pacific/Midway"
        , city: "Midway Atoll"
        , country: "United States"
        },
        { name: "Pacific/Nauru"
        , city: "Yaren"
        , country: "Nauru"
        },
        { name: "Pacific/Niue"
        , city: "Alofi"
        , country: "Niue"
        },
        { name: "Pacific/Norfolk"
        , city: "Kingston"
        , country: "Norfolk Island"
        },
        { name: "Pacific/Noumea"
        , city: "Noumea"
        , country: "New Caledonia"
        },
        { name: "Pacific/Pago_Pago"
        , city: "Pago Pago"
        , country: "American Samoa"
        },
        { name: "Pacific/Palau"
        , city: "Ngerulmud"
        , country: "Palau"
        },
        { name: "Pacific/Pitcairn"
        , city: "Adamstown"
        , country: "Pitcairn Islands"
        },
        { name: "Pacific/Pohnpei"
        , city: "Palikir"
        , country: "Federated States of Micronesia"
        },
        { name: "Pacific/Port_Moresby"
        , city: "Port Moresby"
        , country: "Papua New Guinea"
        },
        { name: "Pacific/Rarotonga"
        , city: "Avarua"
        , country: "Cook Islands"
        },
        { name: "Pacific/Saipan"
        , city: "Saipan"
        , country: "Northern Mariana Islands"
        },
        { name: "Pacific/Samoa"
        , city: "Apia"
        , country: "Samoa"
        },
        { name: "Pacific/Tahiti"
        , city: "Papeete"
        , country: "French Polynesia"
        },
        { name: "Pacific/Tarawa"
        , city: "Tarawa"
        , country: "Kiribati"
        },
        { name: "Pacific/Tongatapu"
        , city: "Nukuʻalofa"
        , country: "Tonga"
        },
        { name: "Pacific/Wake"
        , city: "Wake Island"
        , country: "United States Minor Outlying Islands"
        },
        { name: "Pacific/Wallis"
        , city: "Mata Utu"
        , country: "Wallis and Futuna"
        },
        { name: "Pacific/Yap"
        , city: "Colonia"
        , country: "Federated States of Micronesia"
        }
    ]
