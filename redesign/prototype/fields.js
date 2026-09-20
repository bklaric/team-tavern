// The fields the post screen (post.js) and the account page (account.js) ask
// for: the lists they offer, the control each kind draws, and how one is read
// back. A field is { key, label, kind, options, placeholder, hint }. Classic
// script, loaded after site.js and before game.js and the pages.

// Regions and countries: the site's own lists, not a game's. A region is the
// set of places close enough to each other to play together, which is what the
// latency between them decides (brief 5). A player's location is a country and
// only a country, and is compared through the region that country is in; a
// group or community gives regions and never countries, so the two meet in one
// comparison whichever pair of post types is matched (brief 7.2).
//
// redesign/seed-regions.sql and redesign/seed-countries.sql hold the same two
// lists, and say why each country falls where it does.

const REGIONS = [
    "Europe", "Middle East", "North Africa", "Sub-Saharan Africa", "North America",
    "Central America", "South America", "Central Asia", "South Asia", "East Asia",
    "Southeast Asia", "Oceania",
];

// What a card's fact line and the description bar call a region. No code fits
// all twelve: EU and NA are read everywhere, but SA is South America and South
// Asia at once, so a name cut at the compass point is what stays legible.
const REGION_SHORT = {
    "Europe": "Europe", "Middle East": "Middle East", "North Africa": "N. Africa",
    "Sub-Saharan Africa": "Sub-Saharan Africa", "North America": "N. America",
    "Central America": "C. America", "South America": "S. America",
    "Central Asia": "C. Asia", "South Asia": "S. Asia", "East Asia": "E. Asia",
    "Southeast Asia": "SE Asia", "Oceania": "Oceania",
};

const regionOptions = REGIONS.map(r => ({ value: r, label: r }));

const COUNTRIES_BY_REGION = {
    "Europe": [
        "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium",
        "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
        "Faroe Islands", "Finland", "France", "Georgia", "Germany", "Gibraltar", "Greece",
        "Greenland", "Guernsey", "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", "Jersey",
        "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova",
        "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal",
        "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden",
        "Switzerland", "Turkey", "Ukraine", "United Kingdom",
    ],
    "Middle East": [
        "Bahrain", "Iran", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Oman", "Palestine",
        "Qatar", "Saudi Arabia", "Sudan", "Syria", "United Arab Emirates", "Yemen",
    ],
    "North Africa": [
        "Algeria", "Egypt", "Libya", "Morocco", "Tunisia",
    ],
    "Sub-Saharan Africa": [
        "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon",
        "Central African Republic", "Chad", "Comoros", "Côte d'Ivoire",
        "Democratic Republic of the Congo", "Djibouti", "Equatorial Guinea", "Eritrea", "Eswatini",
        "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho",
        "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mayotte",
        "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Réunion", "Rwanda",
        "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa",
        "South Sudan", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe",
    ],
    "North America": [
        "Bermuda", "Canada", "Mexico", "United States",
    ],
    "Central America": [
        "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Belize", "Bonaire",
        "British Virgin Islands", "Cayman Islands", "Costa Rica", "Cuba", "Curaçao", "Dominica",
        "Dominican Republic", "El Salvador", "Grenada", "Guadeloupe", "Guatemala", "Haiti",
        "Honduras", "Jamaica", "Martinique", "Nicaragua", "Panama", "Puerto Rico",
        "Saint Barthélemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin",
        "Saint Vincent and the Grenadines", "Sint Maarten", "Trinidad and Tobago",
        "Turks and Caicos Islands", "United States Virgin Islands",
    ],
    "South America": [
        "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "French Guiana", "Guyana",
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela",
    ],
    "Central Asia": [
        "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan",
    ],
    "South Asia": [
        "Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan",
        "Sri Lanka",
    ],
    "East Asia": [
        "China", "Hong Kong", "Japan", "Macau", "Mongolia", "North Korea", "South Korea", "Taiwan",
    ],
    "Southeast Asia": [
        "Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines",
        "Singapore", "Thailand", "Timor-Leste", "Vietnam",
    ],
    "Oceania": [
        "American Samoa", "Australia", "Cook Islands", "Fiji", "French Polynesia", "Guam",
        "Kiribati", "Marshall Islands", "Micronesia", "Nauru", "New Caledonia", "New Zealand",
        "Northern Mariana Islands", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands",
        "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna",
    ],
};

const COUNTRIES = Object.entries(COUNTRIES_BY_REGION)
    .flatMap(([region, names]) => names.map(name => ({ name, region })))
    .sort((a, b) => a.name.localeCompare(b.name));

const REGION_OF = Object.fromEntries(COUNTRIES.map(c => [c.name, c.region]));
const locationOptions = COUNTRIES.map(c => ({ value: c.name, label: c.name }));

// The languages players write in, with the code cards show them by.

const LANGUAGE_CODES = {
    English: "EN", German: "DE", Russian: "RU", French: "FR", Hindi: "HI", Spanish: "ES",
    Filipino: "FIL", Polish: "PL", Dutch: "NL", Arabic: "AR", Portuguese: "PT", Swedish: "SV",
    Romanian: "RO", Czech: "CS", Italian: "IT", Ukrainian: "UK", Danish: "DA", Turkish: "TR",
    Hungarian: "HU", Serbian: "SR", Mandarin: "ZH", Greek: "EL", Croatian: "HR", Malay: "MS",
    Slovak: "SK", Japanese: "JA", Norwegian: "NO", Indonesian: "ID", Bengali: "BN", Finnish: "FI",
    Lithuanian: "LT", Bosnian: "BS", Cantonese: "YUE", Persian: "FA", Thai: "TH", Vietnamese: "VI",
    Bulgarian: "BG", Urdu: "UR", Korean: "KO", Latvian: "LV", Estonian: "ET", Slovenian: "SL",
    Albanian: "SQ", Macedonian: "MK", Hebrew: "HE", Tagalog: "TL", Tamil: "TA", Punjabi: "PA",
};
const languageCode = name => LANGUAGE_CODES[name] || name;
const languageList = Object.keys(LANGUAGE_CODES).sort().map(l => ({ value: l, label: l }));

// The game account players add each other by. A game names the one it uses, and
// the account holds one of each the player has given (brief 11.5). The note
// says where it shows, so the account page can ask for one the player's current
// game doesn't use.

const GAME_ACCOUNTS = {
    riot: { key: "riot", label: "Riot ID", placeholder: "Name#TAG", note: "On your Valorant and League of Legends posts." },
    steam: { key: "steam", label: "Steam profile", placeholder: "steamcommunity.com/id/…", note: "On your posts for every other game." },
    ea: { key: "ea", label: "EA ID", placeholder: "Your EA ID", note: "On your Apex Legends posts." },
};
const GAME_ACCOUNT_OF = { apex: "ea", valorant: "riot", lol: "riot" };
const gameAccountOf = handle => GAME_ACCOUNTS[GAME_ACCOUNT_OF[handle] || "steam"];

const timezoneOptions = () => (Intl.supportedValuesOf ? Intl.supportedValuesOf("timeZone") : [VIEWER_TZ])
    .map(zone => ({ value: zone, label: zone.replaceAll("_", " ") }));

const formatDate = value =>
    new Date(value).toLocaleDateString(DATE_LOCALE, { day: "numeric", month: "long", year: "numeric", timeZone: "UTC" });

// Controls.

const controlId = key => `c-${key.replace(":", "-")}`;

const selectHtml = (id, options, value, placeholder) =>
    `<select class="select" id="${id}" data-control>${placeholder ? `<option value="">${placeholder}</option>` : ""}${options.map(o =>
        `<option value="${escapeHtml(o.value)}"${o.value === value ? " selected" : ""}>${escapeHtml(o.label)}</option>`).join("")}</select>`;

const inputHtml = (f, value, type = "text") =>
    `<input class="input" type="${type}" id="${controlId(f.key)}" data-control value="${escapeHtml(value ?? "")}" placeholder="${escapeHtml(f.placeholder || "")}" autocomplete="${f.autocomplete || "off"}">`;

// The control for a field, by kind. A kind a page owns itself, such as the post
// screen's count steppers, isn't here.
const fieldControlHtml = (f, value) => {
    const id = controlId(f.key);
    switch (f.kind) {
        case "text": return inputHtml(f, value);
        case "email": return inputHtml(f, value, "email");
        case "password": return inputHtml(f, value, "password");
        case "date": return `<input class="input" type="date" id="${id}" data-control value="${value ?? ""}" style="max-width: 200px">`;
        case "textarea":
            return `<textarea class="textarea" id="${id}" data-control rows="5" placeholder="${escapeHtml(f.placeholder)}">${escapeHtml(value ?? "")}</textarea>`;
        case "choose":
        case "select":
            return selectHtml(id, f.options, value, f.placeholder);
        case "timezone":
            return selectHtml(id, timezoneOptions(), value);
        case "pills":
        case "radioPills": {
            const multi = f.kind === "pills";
            const chosen = multi ? value || [] : [value];
            return `<div class="pills" role="${multi ? "group" : "radiogroup"}" aria-labelledby="l-${id}">${f.options.map(o =>
                `<label class="pill"><input type="${multi ? "checkbox" : "radio"}" name="${id}" value="${escapeHtml(o.value)}"${chosen.includes(o.value) ? " checked" : ""}>${icon("check")}${escapeHtml(o.label)}</label>`
            ).join("")}</div>`;
        }
        case "radioList":
            return `<div class="choice-list" role="radiogroup" aria-labelledby="l-${id}">${f.options.map(o =>
                `<label class="choice"><input type="radio" name="${id}" value="${o.value}"${o.value === value ? " checked" : ""}>${escapeHtml(o.label)}</label>`
            ).join("")}</div>`;
        case "check":
            return `<label class="check"><input type="checkbox" id="${id}" data-control${value ? " checked" : ""}>${escapeHtml(f.text)}</label>`;
        // A switch is on or off the moment it is flipped, where a checkbox
        // waits for the form it sits in: the email switches (brief 11.5) have
        // nothing to submit.
        case "switch":
            return `<label class="switch"><input type="checkbox" id="${id}" data-control${value ? " checked" : ""}>
                <span class="switch-track"></span>
                <span class="switch-text">${escapeHtml(f.text)}${f.note ? `<span class="switch-note">${escapeHtml(f.note)}</span>` : ""}</span></label>`;
        case "tokens": {
            const chosen = value || [];
            const rest = f.options.filter(o => !chosen.includes(o.value));
            return `<div class="tokens">${chosen.map(v =>
                `<span class="token">${escapeHtml(v)}<button class="icon-button" type="button" data-remove="${escapeHtml(v)}" aria-label="Remove ${escapeHtml(v)}">${icon("x")}</button></span>`
            ).join("")}
                <select class="select" id="${id}" data-add aria-labelledby="l-${id}"><option value="">${chosen.length ? "Add another" : `Add a ${escapeHtml(f.one || "language")}`}</option>${rest.map(o =>
                    `<option value="${escapeHtml(o.value)}">${escapeHtml(o.label)}</option>`).join("")}</select></div>`;
        }
        default: return "";
    }
};

const readControl = (element, f) => {
    const control = element.querySelector("[data-control]");
    switch (f.kind) {
        case "pills": return [...element.querySelectorAll("input:checked")].map(i => i.value);
        case "radioPills":
        case "radioList": return (element.querySelector("input:checked") || {}).value;
        case "check":
        case "switch": return control.checked;
        default: return control.value === "" ? undefined : control.value;
    }
};

// What a field's value reads as when it is shown rather than asked for: on the
// post screen as a fact the account holds, on the account page as the account.
const factText = (f, value) =>
    f.kind === "tokens" ? value.join(", ")
    : f.kind === "date" ? `${formatDate(value)}, shown as age ${ageAt(value)}`
    : f.kind === "timezone" ? value.replaceAll("_", " ")
    : value;

// The shell around a control: the label over it, then the hint, a note and an
// error. The note and the error keep their places, so a page can fill them in
// without drawing the field again.

const LABELLED = ["text", "email", "password", "textarea", "date", "choose", "select", "timezone", "discord"];

const errorHtml = message => message ? `<span class="field-error" role="alert">${icon("circle-alert")}${escapeHtml(message)}</span>` : "";

const fieldShellHtml = (f, control, { labelled = LABELLED.includes(f.kind), hint = f.hint, note = "", error = "" } = {}) => {
    const id = controlId(f.key);
    const tag = labelled ? "label" : "span";
    const label = ["check", "switch"].includes(f.kind) ? "" : `<${tag} class="field-label" id="l-${id}"${labelled ? ` for="${id}"` : ""}>${escapeHtml(f.label)}${
        f.required ? `<span class="field-tag">Required</span>` : ""}</${tag}>`;
    return `<div class="field${error ? " field-invalid" : ""}" data-field="${f.key}">
        ${label}
        ${control}
        ${hint ? `<span class="field-hint">${escapeHtml(hint)}</span>` : ""}
        <span data-note>${note}</span>
        <span data-error>${error}</span>
    </div>`;
};
