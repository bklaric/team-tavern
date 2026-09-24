// Whether the player is on the toast, with the pointer or the focus.
export const held = () => document.querySelector(".toast:hover, .toast:focus-within") !== null;

export const holdsFocus = () => document.activeElement !== null && document.activeElement.closest(".toast") !== null;
