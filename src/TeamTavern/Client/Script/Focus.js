export const focusSoon = selector => () => {
    requestAnimationFrame(() => {
        const element = document.querySelector(selector);
        if (element) element.focus();
    });
};

export const focusStill = selector => () => {
    requestAnimationFrame(() => {
        const element = document.querySelector(selector);
        if (element) element.focus({ preventScroll: true });
    });
};
