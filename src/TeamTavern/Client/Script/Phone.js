const phone = matchMedia("(max-width: 639px)");

export const isPhone = () => phone.matches;

export const onPhoneChange = callback => () => {
    const listener = event => callback(event.matches)();
    phone.addEventListener("change", listener);
    return () => phone.removeEventListener("change", listener);
};
