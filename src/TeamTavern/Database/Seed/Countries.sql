-- The countries, one region each (TablesCurrent.sql, brief 5). A player's
-- location is a country and only a country, so this table is the whole of how a
-- player is compared with a group's or a community's regions (brief 7.2).
--
-- The list is every ISO 3166-1 country and inhabited dependency of more than
-- about ten thousand residents, under the name a player would look for rather
-- than the official one: United States, not United States of America. A
-- dependency far from the state that holds it is its own row, since latency is
-- what a region is for: Greenland, Réunion and Guam fall in three different
-- regions from Denmark, France and the United States.
--
-- Which region a country falls in is decided by the servers its players reach,
-- not by the continent a map draws it on, and a region has to hold together at
-- both ends: Lisbon to Warsaw is about 50ms, which is what Europe is allowed to
-- cost, so a country is here when every other country in the region is about
-- that far from it. Where the answer and the map disagree, the block says why.

-- Europe, as far east as the Urals: most of Russia's players are west of them
-- (brief 5). Turkey, the Caucasus and Cyprus are here because Istanbul is about
-- 45ms from Frankfurt and every publisher routes them to European servers.
-- Greenland is here because its cable runs through Iceland to Denmark, and the
-- North America the map puts it in is not on the other end of anything.

insert into country (name, region_name) values
    ('Albania', 'Europe'),
    ('Andorra', 'Europe'),
    ('Armenia', 'Europe'),
    ('Austria', 'Europe'),
    ('Azerbaijan', 'Europe'),
    ('Belarus', 'Europe'),
    ('Belgium', 'Europe'),
    ('Bosnia and Herzegovina', 'Europe'),
    ('Bulgaria', 'Europe'),
    ('Croatia', 'Europe'),
    ('Cyprus', 'Europe'),
    ('Czechia', 'Europe'),
    ('Denmark', 'Europe'),
    ('Estonia', 'Europe'),
    ('Faroe Islands', 'Europe'),
    ('Finland', 'Europe'),
    ('France', 'Europe'),
    ('Georgia', 'Europe'),
    ('Germany', 'Europe'),
    ('Gibraltar', 'Europe'),
    ('Greece', 'Europe'),
    ('Greenland', 'Europe'),
    ('Guernsey', 'Europe'),
    ('Hungary', 'Europe'),
    ('Iceland', 'Europe'),
    ('Ireland', 'Europe'),
    ('Isle of Man', 'Europe'),
    ('Italy', 'Europe'),
    ('Jersey', 'Europe'),
    ('Kosovo', 'Europe'),
    ('Latvia', 'Europe'),
    ('Liechtenstein', 'Europe'),
    ('Lithuania', 'Europe'),
    ('Luxembourg', 'Europe'),
    ('Malta', 'Europe'),
    ('Moldova', 'Europe'),
    ('Monaco', 'Europe'),
    ('Montenegro', 'Europe'),
    ('Netherlands', 'Europe'),
    ('North Macedonia', 'Europe'),
    ('Norway', 'Europe'),
    ('Poland', 'Europe'),
    ('Portugal', 'Europe'),
    ('Romania', 'Europe'),
    ('Russia', 'Europe'),
    ('San Marino', 'Europe'),
    ('Serbia', 'Europe'),
    ('Slovakia', 'Europe'),
    ('Slovenia', 'Europe'),
    ('Spain', 'Europe'),
    ('Sweden', 'Europe'),
    ('Switzerland', 'Europe'),
    ('Turkey', 'Europe'),
    ('Ukraine', 'Europe'),
    ('United Kingdom', 'Europe');

-- The Gulf and the Levant, who meet on the servers in Bahrain and Dubai. Sudan
-- is here rather than in North Africa: its cables land in Jeddah, 30ms away,
-- where Khartoum to Casablanca is over 100ms through Europe.

insert into country (name, region_name) values
    ('Bahrain', 'Middle East'),
    ('Iran', 'Middle East'),
    ('Iraq', 'Middle East'),
    ('Israel', 'Middle East'),
    ('Jordan', 'Middle East'),
    ('Kuwait', 'Middle East'),
    ('Lebanon', 'Middle East'),
    ('Oman', 'Middle East'),
    ('Palestine', 'Middle East'),
    ('Qatar', 'Middle East'),
    ('Saudi Arabia', 'Middle East'),
    ('Sudan', 'Middle East'),
    ('Syria', 'Middle East'),
    ('United Arab Emirates', 'Middle East'),
    ('Yemen', 'Middle East');

-- The Mediterranean coast, which reaches its servers through Marseille: Cairo
-- to Casablanca is about 70ms that way, the same as Europe's own ends.

insert into country (name, region_name) values
    ('Algeria', 'North Africa'),
    ('Egypt', 'North Africa'),
    ('Libya', 'North Africa'),
    ('Morocco', 'North Africa'),
    ('Tunisia', 'North Africa');

-- One region for the rest of the continent, which is the one place the latency
-- rule is bent: Dakar to Nairobi is about 180ms, routed through Europe, so this
-- is really three or four regions' worth of ground. Splitting it would leave
-- each piece with too few players to match against.

insert into country (name, region_name) values
    ('Angola', 'Sub-Saharan Africa'),
    ('Benin', 'Sub-Saharan Africa'),
    ('Botswana', 'Sub-Saharan Africa'),
    ('Burkina Faso', 'Sub-Saharan Africa'),
    ('Burundi', 'Sub-Saharan Africa'),
    ('Cabo Verde', 'Sub-Saharan Africa'),
    ('Cameroon', 'Sub-Saharan Africa'),
    ('Central African Republic', 'Sub-Saharan Africa'),
    ('Chad', 'Sub-Saharan Africa'),
    ('Comoros', 'Sub-Saharan Africa'),
    ('Côte d''Ivoire', 'Sub-Saharan Africa'),
    ('Democratic Republic of the Congo', 'Sub-Saharan Africa'),
    ('Djibouti', 'Sub-Saharan Africa'),
    ('Equatorial Guinea', 'Sub-Saharan Africa'),
    ('Eritrea', 'Sub-Saharan Africa'),
    ('Eswatini', 'Sub-Saharan Africa'),
    ('Ethiopia', 'Sub-Saharan Africa'),
    ('Gabon', 'Sub-Saharan Africa'),
    ('Gambia', 'Sub-Saharan Africa'),
    ('Ghana', 'Sub-Saharan Africa'),
    ('Guinea', 'Sub-Saharan Africa'),
    ('Guinea-Bissau', 'Sub-Saharan Africa'),
    ('Kenya', 'Sub-Saharan Africa'),
    ('Lesotho', 'Sub-Saharan Africa'),
    ('Liberia', 'Sub-Saharan Africa'),
    ('Madagascar', 'Sub-Saharan Africa'),
    ('Malawi', 'Sub-Saharan Africa'),
    ('Mali', 'Sub-Saharan Africa'),
    ('Mauritania', 'Sub-Saharan Africa'),
    ('Mauritius', 'Sub-Saharan Africa'),
    ('Mayotte', 'Sub-Saharan Africa'),
    ('Mozambique', 'Sub-Saharan Africa'),
    ('Namibia', 'Sub-Saharan Africa'),
    ('Niger', 'Sub-Saharan Africa'),
    ('Nigeria', 'Sub-Saharan Africa'),
    ('Republic of the Congo', 'Sub-Saharan Africa'),
    ('Rwanda', 'Sub-Saharan Africa'),
    ('Réunion', 'Sub-Saharan Africa'),
    ('São Tomé and Príncipe', 'Sub-Saharan Africa'),
    ('Senegal', 'Sub-Saharan Africa'),
    ('Seychelles', 'Sub-Saharan Africa'),
    ('Sierra Leone', 'Sub-Saharan Africa'),
    ('Somalia', 'Sub-Saharan Africa'),
    ('South Africa', 'Sub-Saharan Africa'),
    ('South Sudan', 'Sub-Saharan Africa'),
    ('Tanzania', 'Sub-Saharan Africa'),
    ('Togo', 'Sub-Saharan Africa'),
    ('Uganda', 'Sub-Saharan Africa'),
    ('Zambia', 'Sub-Saharan Africa'),
    ('Zimbabwe', 'Sub-Saharan Africa');

-- Mexico is here rather than with Central America: Monterrey is 25ms from
-- Dallas and Tijuana 110ms from Panama City, so the border it plays across is
-- the northern one.

insert into country (name, region_name) values
    ('Bermuda', 'North America'),
    ('Canada', 'North America'),
    ('Mexico', 'North America'),
    ('United States', 'North America');

-- The isthmus and the islands. The Caribbean is here rather than in North
-- America, which Florida makes look closer than it is: Havana is 40ms from
-- Mexico City and 100ms from Seattle, and a region has to hold at both ends.

insert into country (name, region_name) values
    ('Anguilla', 'Central America'),
    ('Antigua and Barbuda', 'Central America'),
    ('Aruba', 'Central America'),
    ('Bahamas', 'Central America'),
    ('Barbados', 'Central America'),
    ('Belize', 'Central America'),
    ('Bonaire', 'Central America'),
    ('British Virgin Islands', 'Central America'),
    ('Cayman Islands', 'Central America'),
    ('Costa Rica', 'Central America'),
    ('Cuba', 'Central America'),
    ('Curaçao', 'Central America'),
    ('Dominica', 'Central America'),
    ('Dominican Republic', 'Central America'),
    ('El Salvador', 'Central America'),
    ('Grenada', 'Central America'),
    ('Guadeloupe', 'Central America'),
    ('Guatemala', 'Central America'),
    ('Haiti', 'Central America'),
    ('Honduras', 'Central America'),
    ('Jamaica', 'Central America'),
    ('Martinique', 'Central America'),
    ('Nicaragua', 'Central America'),
    ('Panama', 'Central America'),
    ('Puerto Rico', 'Central America'),
    ('Saint Barthélemy', 'Central America'),
    ('Saint Kitts and Nevis', 'Central America'),
    ('Saint Lucia', 'Central America'),
    ('Saint Martin', 'Central America'),
    ('Saint Vincent and the Grenadines', 'Central America'),
    ('Sint Maarten', 'Central America'),
    ('Trinidad and Tobago', 'Central America'),
    ('Turks and Caicos Islands', 'Central America'),
    ('United States Virgin Islands', 'Central America');

insert into country (name, region_name) values
    ('Argentina', 'South America'),
    ('Bolivia', 'South America'),
    ('Brazil', 'South America'),
    ('Chile', 'South America'),
    ('Colombia', 'South America'),
    ('Ecuador', 'South America'),
    ('French Guiana', 'South America'),
    ('Guyana', 'South America'),
    ('Paraguay', 'South America'),
    ('Peru', 'South America'),
    ('Suriname', 'South America'),
    ('Uruguay', 'South America'),
    ('Venezuela', 'South America');

insert into country (name, region_name) values
    ('Kazakhstan', 'Central Asia'),
    ('Kyrgyzstan', 'Central Asia'),
    ('Tajikistan', 'Central Asia'),
    ('Turkmenistan', 'Central Asia'),
    ('Uzbekistan', 'Central Asia');

-- Afghanistan is here rather than in Central Asia: what it reaches the world
-- through runs south, through Pakistan.

insert into country (name, region_name) values
    ('Afghanistan', 'South Asia'),
    ('Bangladesh', 'South Asia'),
    ('Bhutan', 'South Asia'),
    ('India', 'South Asia'),
    ('Maldives', 'South Asia'),
    ('Nepal', 'South Asia'),
    ('Pakistan', 'South Asia'),
    ('Sri Lanka', 'South Asia');

insert into country (name, region_name) values
    ('China', 'East Asia'),
    ('Hong Kong', 'East Asia'),
    ('Japan', 'East Asia'),
    ('Macau', 'East Asia'),
    ('Mongolia', 'East Asia'),
    ('North Korea', 'East Asia'),
    ('South Korea', 'East Asia'),
    ('Taiwan', 'East Asia');

insert into country (name, region_name) values
    ('Brunei', 'Southeast Asia'),
    ('Cambodia', 'Southeast Asia'),
    ('Indonesia', 'Southeast Asia'),
    ('Laos', 'Southeast Asia'),
    ('Malaysia', 'Southeast Asia'),
    ('Myanmar', 'Southeast Asia'),
    ('Philippines', 'Southeast Asia'),
    ('Singapore', 'Southeast Asia'),
    ('Thailand', 'Southeast Asia'),
    ('Timor-Leste', 'Southeast Asia'),
    ('Vietnam', 'Southeast Asia');

-- Australia, New Zealand and the Pacific. The islands are as far from Sydney as
-- Sydney is from Singapore, and the second bent rule: there is no other region
-- to give them, and Sydney is where their servers are.

insert into country (name, region_name) values
    ('American Samoa', 'Oceania'),
    ('Australia', 'Oceania'),
    ('Cook Islands', 'Oceania'),
    ('Fiji', 'Oceania'),
    ('French Polynesia', 'Oceania'),
    ('Guam', 'Oceania'),
    ('Kiribati', 'Oceania'),
    ('Marshall Islands', 'Oceania'),
    ('Micronesia', 'Oceania'),
    ('Nauru', 'Oceania'),
    ('New Caledonia', 'Oceania'),
    ('New Zealand', 'Oceania'),
    ('Northern Mariana Islands', 'Oceania'),
    ('Palau', 'Oceania'),
    ('Papua New Guinea', 'Oceania'),
    ('Samoa', 'Oceania'),
    ('Solomon Islands', 'Oceania'),
    ('Tonga', 'Oceania'),
    ('Tuvalu', 'Oceania'),
    ('Vanuatu', 'Oceania'),
    ('Wallis and Futuna', 'Oceania');
