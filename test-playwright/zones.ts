import { Page } from "@playwright/test";

// Makes the page's Intl report its own zone as `reported`, and a zone it is asked by name as
// that name, as a browser does that doesn't take an old name and a new one for the same
// zone. Asked by a name in `unknown`, it throws, as a browser too old to know the name does.
export async function reportZone(page: Page, reported: string, unknown: string[] = []) {
    await page.addInitScript(({ reported, unknown }) => {
        const Original = Intl.DateTimeFormat;
        const Reporting = function (locales?: string | string[], options?: Intl.DateTimeFormatOptions) {
            if (options?.timeZone && unknown.includes(options.timeZone))
                throw new RangeError(`Invalid time zone specified: ${options.timeZone}`);
            const format = new Original(locales, options);
            const resolvedOptions = format.resolvedOptions.bind(format);
            format.resolvedOptions = () => ({ ...resolvedOptions(), timeZone: options?.timeZone ?? reported });
            return format;
        };
        Object.setPrototypeOf(Reporting, Original);
        Reporting.prototype = Original.prototype;
        Intl.DateTimeFormat = Reporting as unknown as typeof Intl.DateTimeFormat;
    }, { reported, unknown });
}
