export const cardOf = event => () => event.target.closest(".card")

export const heightOf = card => () => card ? card.offsetHeight : 0

export const animateFrom = card => from => () => {
    if (!card || matchMedia("(prefers-reduced-motion: reduce)").matches) return
    const to = card.offsetHeight
    card.animate(
        [{ height: `${from}px`, overflow: "hidden" }, { height: `${to}px`, overflow: "hidden" }],
        { duration: 180, easing: "ease-out" })
}
