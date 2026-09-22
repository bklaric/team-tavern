export const onScrollImpl = callback => () => {
    const listener = () => callback(window.scrollY)();
    window.addEventListener("scroll", listener, { passive: true });
    return () => window.removeEventListener("scroll", listener);
};

// A page put back draws what it had over its next few frames, so it can be
// scrolled only once it reaches that far. It gives up after two seconds and
// scrolls as far as it can.
export const scrollToOnceDrawnImpl = y => done => () => {
    const started = performance.now();
    const attempt = () => {
        const reachable = document.documentElement.scrollHeight - window.innerHeight >= y;
        if (reachable || performance.now() - started > 2000) {
            window.scrollTo(0, y);
            done();
        } else {
            requestAnimationFrame(attempt);
        }
    };
    attempt();
};

export const scrollRestorationManual = () => {
    history.scrollRestoration = "manual";
};
