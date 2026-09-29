export const onScrollImpl = callback => () => {
    const listener = () => callback(window.scrollY)();
    window.addEventListener("scroll", listener, { passive: true });
    return () => window.removeEventListener("scroll", listener);
};

export const scrollRestorationManual = () => {
    history.scrollRestoration = "manual";
};

export const scrollToFragmentImpl = () => {
    requestAnimationFrame(() => {
        const id = decodeURIComponent(location.hash.slice(1));
        if (id) document.getElementById(id)?.scrollIntoView({ block: "start" });
    });
};

export const focusCenteredImpl = id => () => {
    requestAnimationFrame(() => {
        const element = document.getElementById(id);
        if (!element) return;
        element.scrollIntoView({ block: "center" });
        element.focus({ preventScroll: true });
    });
};

export const focusFirstInvalid = () => {
    requestAnimationFrame(() => {
        const field = document.querySelector(".field-invalid");
        if (!field) return;
        field.scrollIntoView({ block: "center" });
        const control = field.querySelector("input, textarea, select");
        if (control) control.focus({ preventScroll: true });
    });
};
