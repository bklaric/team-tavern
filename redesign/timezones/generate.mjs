// Writes src/TeamTavern/Shared/Timezones.purs, the timezones a player can pick:
//   "$(volta which node)" redesign/timezones/generate.mjs
// The zones are tzdb's zone.tab, which gives every country its own zones under
// names of its own (Europe/Zagreb), where zone1970.tab merges countries whose
// clocks have agreed since 1970. A zone's city is CLDR's English name for it,
// and its country is the name Database/Seed/Countries.sql gives the country, so
// the Timezone and Location fields agree. Every name has to be one the Postgres
// image the stacks pin knows, or `at time zone` fails the feed, so the script
// asks that image before writing anything.
import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "../..");
const output = join(root, "src/TeamTavern/Shared/Timezones.purs");

const tzdb = "https://data.iana.org/time-zones/tzdb";
const cldrTag = "48.0.0";
const cldr = `https://raw.githubusercontent.com/unicode-org/cldr-json/${cldrTag}/cldr-json`;

// Countries whose name in Countries.sql differs from CLDR's English one.
const siteNames = {
    AG: "Antigua and Barbuda",
    BA: "Bosnia and Herzegovina",
    BL: "Saint Barthélemy",
    BQ: "Bonaire",
    CD: "Democratic Republic of the Congo",
    CG: "Republic of the Congo",
    CI: "Côte d'Ivoire",
    CV: "Cabo Verde",
    HK: "Hong Kong",
    KN: "Saint Kitts and Nevis",
    LC: "Saint Lucia",
    MF: "Saint Martin",
    MM: "Myanmar",
    MO: "Macau",
    PS: "Palestine",
    ST: "São Tomé and Príncipe",
    TC: "Turks and Caicos Islands",
    TR: "Turkey",
    TT: "Trinidad and Tobago",
    VC: "Saint Vincent and the Grenadines",
    VI: "United States Virgin Islands",
    WF: "Wallis and Futuna",
};

// Territories with zones of their own but too few residents for a row in
// Countries.sql, named in its style.
const outsideCountries = {
    AQ: "Antarctica",
    AX: "Åland Islands",
    CC: "Cocos (Keeling) Islands",
    CX: "Christmas Island",
    EH: "Western Sahara",
    FK: "Falkland Islands",
    GS: "South Georgia and the South Sandwich Islands",
    IO: "British Indian Ocean Territory",
    MS: "Montserrat",
    NF: "Norfolk Island",
    NU: "Niue",
    PM: "Saint Pierre and Miquelon",
    PN: "Pitcairn Islands",
    SH: "Saint Helena",
    SJ: "Svalbard and Jan Mayen",
    TF: "French Southern Territories",
    TK: "Tokelau",
    UM: "United States Minor Outlying Islands",
    VA: "Vatican City",
};

const fail = message => {
    console.error(message);
    process.exit(1);
};

const fetchText = async url => {
    const response = await fetch(url);
    if (!response.ok) fail(`${url} answered ${response.status}`);
    return response.text();
};

const version = (await fetchText(`${tzdb}/version`)).trim();
const zoneTab = (await fetchText(`${tzdb}/zone.tab`))
    .split("\n")
    .filter(line => line && !line.startsWith("#"))
    .map(line => line.split("\t"))
    .map(([code, , name]) => ({ code, name }));

const cldrZones = JSON.parse(await fetchText(`${cldr}/cldr-dates-full/main/en/timeZoneNames.json`))
    .main.en.dates.timeZoneNames.zone;
const cldrTerritories = JSON.parse(await fetchText(`${cldr}/cldr-localenames-full/main/en/territories.json`))
    .main.en.localeDisplayNames.territories;

const siteCountries = new Set(
    [...readFileSync(join(root, "src/TeamTavern/Database/Seed/Countries.sql"), "utf8")
        .matchAll(/^\s*\('((?:[^']|'')*)',/gm)]
    .map(match => match[1].replace(/''/g, "'")));

// CLDR names a zone's city only where its name doesn't: the last part of the
// zone's name, spaced, is the city otherwise.
const cityOf = name => {
    const zone = name.split("/").reduce((node, part) => node?.[part], cldrZones);
    return zone?.exemplarCity ?? name.split("/").at(-1).replace(/_/g, " ");
};

const countryOf = code => {
    if (code in outsideCountries) {
        if (siteCountries.has(outsideCountries[code]))
            fail(`${code} is in Countries.sql as ${outsideCountries[code]}; move it out of outsideCountries`);
        return outsideCountries[code];
    }
    const country = siteNames[code] ?? cldrTerritories[code];
    if (!siteCountries.has(country))
        fail(`${code} (${cldrTerritories[code]}) is neither a Countries.sql name nor in outsideCountries`);
    return country;
};

const zones = zoneTab
    .map(({ code, name }) => ({ name, city: cityOf(name), country: countryOf(code) }))
    .sort((a, b) => a.country.localeCompare(b.country, "en") || a.city.localeCompare(b.city, "en"));

// Facts.purs labels a zone by its country alone where the country has one.
const zoneCounts = new Map();
for (const { country } of zones) zoneCounts.set(country, (zoneCounts.get(country) ?? 0) + 1);
const labels = new Map();
for (const zone of zones) {
    const label = zoneCounts.get(zone.country) === 1 ? zone.country : `${zone.country}: ${zone.city}`;
    if (labels.has(label)) fail(`${labels.get(label)} and ${zone.name} would both read "${label}"`);
    labels.set(label, zone.name);
}

const image = readFileSync(join(root, "stacks/docker-compose.release.yml"), "utf8").match(/image: (postgres:\S+)/)[1];
const postgresNames = new Set(execFileSync("docker", [
    "run", "--rm", "--user", "postgres", image, "sh", "-c",
    "initdb -D /tmp/data >/dev/null 2>&1"
    + " && pg_ctl -D /tmp/data -o '-c listen_addresses=' -w start >/dev/null"
    + " && psql -At -d postgres -c 'select name from pg_timezone_names'",
], { encoding: "utf8" }).split("\n").map(name => name.trim()).filter(Boolean));
const unknown = zones.filter(({ name }) => !postgresNames.has(name)).map(({ name }) => name);
if (unknown.length) fail(`${image} doesn't know ${unknown.join(", ")}`);

const previous = new Set([...readFileSync(output, "utf8").matchAll(/name: "([^"]*)"/g)].map(match => match[1]));
const current = new Set(zones.map(({ name }) => name));

const entry = ({ name, city, country }) =>
    `        { name: ${JSON.stringify(name)}\n`
    + `        , city: ${JSON.stringify(city)}\n`
    + `        , country: ${JSON.stringify(country)}\n`
    + `        }`;

writeFileSync(output,
    "-- Generated by redesign/timezones/generate.mjs from tzdb's zone.tab, CLDR's\n"
    + "-- English city names and the country names of Database/Seed/Countries.sql.\n"
    + "-- Change the script and run it again rather than editing this file.\n"
    + "module TeamTavern.Shared.Timezones where\n"
    + "\n"
    + "type Timezone =\n"
    + "    { city :: String\n"
    + "    , country :: String\n"
    + "    , name :: String\n"
    + "    }\n"
    + "\n"
    + "allTimezones :: Array Timezone\n"
    + "allTimezones =\n"
    + "    [\n"
    + zones.map(entry).join(",\n") + "\n"
    + "    ]\n");

console.log(`tzdb ${version}, CLDR ${cldrTag}, ${image}: ${zones.length} zones`);
console.log(`Added: ${[...current].filter(name => !previous.has(name)).join(", ") || "none"}`);
console.log(`Dropped: ${[...previous].filter(name => !current.has(name)).join(", ") || "none"}`);
