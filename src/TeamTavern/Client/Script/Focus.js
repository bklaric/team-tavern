export const focusSoon = selector => () => {
    requestAnimationFrame(() => {
        const element = document.querySelector(selector);
        if (element) element.focus();
    });
};
