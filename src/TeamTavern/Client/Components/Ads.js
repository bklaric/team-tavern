// The Venatus script defines __VM and drains the queue it holds. The prerender
// shell loads no ad script, so the queue has to exist on its own there: without
// it every unit throws and takes the rest of the page's render with it.
const queue = () => {
    self.__VM = self.__VM || [];
    return self.__VM;
};

// Whether a modal overlay is open, as `Overlay.js` announces it.
let modalOpen = false;
document.addEventListener("modalchange", event => { modalOpen = event.detail; });

// The unit is asked for now, and the queue fills in its placement once the
// script has loaded, unless the unit was taken down first.
const show = (name, element) => {
    const shown = { placement: null, removed: false };
    queue().push((admanager, scope) => {
        if (!shown.removed) {
            const placement = scope.Config.get(name);
            shown.placement = element ? placement.display(element) : placement.displayBody();
        }
    });
    return shown;
};

// Through the queue as well, so it runs after the unit's display however soon
// the page lets go of it.
const hide = shown => {
    shown.removed = true;
    queue().push(() => {
        if (shown.placement) {
            shown.placement.remove();
        }
    });
};

// Shows the placement into the element while the media query matches. Without
// an element it goes on the window's floor, and steps aside while a modal
// overlay is open, since it would cover the bottom of the overlay, where a
// phone's sheet keeps its Send.
export const mount = name => query => element => () => {
    const media = matchMedia(query);
    let shown = null;
    const update = () => {
        const wanted = media.matches && !(element === null && modalOpen);
        if (wanted && !shown) {
            shown = show(name, element);
        }
        else if (!wanted && shown) {
            hide(shown);
            shown = null;
        }
    };
    media.addEventListener("change", update);
    document.addEventListener("modalchange", update);
    update();
    return () => {
        media.removeEventListener("change", update);
        document.removeEventListener("modalchange", update);
        if (shown) {
            hide(shown);
        }
    };
};
