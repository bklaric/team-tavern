export const onScrollImpl = callback => () => {
    const listener = () => callback(window.scrollY)();
    window.addEventListener("scroll", listener, { passive: true });
    return () => window.removeEventListener("scroll", listener);
};

export const scrollRestorationManual = () => {
    history.scrollRestoration = "manual";
};
