// The description bar draws an unseen copy of its chips at its own width: the
// chips it always shows, the ones it may keep behind More, a More button and
// Clear all, each marked with data-chip. Laying their widths out as the bar's
// rows wrap them says how many of those More may keep fit in two rows.
const rowsOf = (widths, width, gap) => {
    let rows = 0;
    let used = 0;
    for (const chip of widths) {
        if (rows > 0 && used + gap + chip <= width + 0.5) {
            used += gap + chip;
        }
        else {
            rows += 1;
            used = chip;
        }
    }
    return rows;
};

// What the bar shows when the first `fitting` chips More may keep are out: those,
// any other that holds something, and More while one is left behind it.
const shownWith = (chips, fitting) => {
    const shown = [];
    let index = 0;
    let behind = false;
    for (const chip of chips) {
        if (chip.kind === "more") {
            if (index < fitting || chip.filled) {
                shown.push(chip.width);
            }
            else {
                behind = true;
            }
            index += 1;
        }
        else if (chip.kind !== "more-button" || behind) {
            shown.push(chip.width);
        }
    }
    return shown;
};

const measure = copy => {
    const chips = [...copy.children].map(chip => ({
        kind: chip.dataset.chip,
        filled: "filled" in chip.dataset,
        width: chip.getBoundingClientRect().width,
    }));
    const more = chips.filter(chip => chip.kind === "more").length;
    const width = copy.clientWidth;
    const gap = parseFloat(getComputedStyle(copy).columnGap) || 0;
    const fits = fitting => rowsOf(shownWith(chips, fitting), width, gap) <= 2;
    if (fits(more)) {
        return { shown: more, all: true };
    }
    let fitting = more - 1;
    while (fitting > 0 && !fits(fitting)) {
        fitting -= 1;
    }
    return { shown: Math.max(fitting, 0), all: false };
};

export const onChipsFit = callback => () => {
    let copy = null;
    let ready = false;
    let scheduled = false;
    const check = () => {
        scheduled = false;
        if (copy && ready) {
            callback(measure(copy))();
        }
    };
    const schedule = () => {
        if (!scheduled) {
            scheduled = true;
            requestAnimationFrame(check);
        }
    };
    const resized = new ResizeObserver(schedule);
    const changed = new MutationObserver(schedule);
    // The copy is drawn once the game has loaded, and again whenever the bar
    // is, so the page is watched only for it coming and going.
    const attach = () => {
        const found = document.querySelector(".field-chips-measure");
        if (found === copy) {
            return;
        }
        resized.disconnect();
        changed.disconnect();
        copy = found;
        ready = false;
        if (!copy) {
            return;
        }
        resized.observe(copy);
        changed.observe(copy, { childList: true, subtree: true, characterData: true, attributes: true });
        // Widths read in a fallback font would be kept, so nothing is measured
        // until the chips' own face is in.
        const style = getComputedStyle(copy.firstElementChild ?? copy);
        document.fonts.load(`${style.fontStyle} ${style.fontWeight} ${style.fontSize} ${style.fontFamily}`)
            .catch(() => {})
            .then(() => {
                if (copy === found) {
                    ready = true;
                    schedule();
                }
            });
    };
    const page = new MutationObserver(attach);
    page.observe(document.body, { childList: true, subtree: true });
    attach();
    return () => {
        copy = null;
        page.disconnect();
        resized.disconnect();
        changed.disconnect();
    };
};
