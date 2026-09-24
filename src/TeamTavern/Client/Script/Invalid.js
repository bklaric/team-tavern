// A field's error describes its control and marks it invalid for as long as
// the error shows. The control is drawn by the field's caller, apart from the
// error, so the page keeps the two in step as it changes.
const control = field =>
    field.querySelector("input:not([type=hidden]), select, textarea, [role=radiogroup], [role=group]");

const sync = () => {
    for (const field of document.querySelectorAll(".field")) {
        const described = control(field);
        if (!described) {
            continue;
        }
        const error = field.querySelector(":scope > .field-error[id]");
        if (error) {
            described.setAttribute("aria-invalid", "true");
            described.setAttribute("aria-describedby", error.id);
        }
        else if (described.hasAttribute("aria-invalid")) {
            described.removeAttribute("aria-invalid");
            described.removeAttribute("aria-describedby");
        }
    }
};

export const describeInvalid = () => {
    let scheduled = false;
    new MutationObserver(() => {
        if (!scheduled) {
            scheduled = true;
            requestAnimationFrame(() => {
                scheduled = false;
                sync();
            });
        }
    }).observe(document.body, { childList: true, subtree: true });
};
