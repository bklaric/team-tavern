const fit = element => {
    element.style.height = "auto";
    element.style.height = `${element.scrollHeight + element.offsetHeight - element.clientHeight}px`;
};

// Again once the page has drawn, since a message box emptied by sending has
// its value taken away on the next render.
export const autosize = element => () => {
    fit(element);
    requestAnimationFrame(() => fit(element));
};

export const scrollThreadsToEnd = () => {
    requestAnimationFrame(() => {
        document.querySelectorAll(".thread-well, .conversation-body").forEach(element => {
            element.scrollTop = element.scrollHeight;
        });
    });
};

export const isWide = () => matchMedia("(min-width: 1024px)").matches;
