export const announceUnread = () => {
    window.dispatchEvent(new Event("tt-unread"));
};

export const onUnread = callback => () => {
    const listener = () => callback();
    window.addEventListener("tt-unread", listener);
    return () => window.removeEventListener("tt-unread", listener);
};
