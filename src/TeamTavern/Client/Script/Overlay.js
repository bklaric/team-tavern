// The overlays open now, the newest last. Escape closes only the newest, and
// Tab is kept inside the newest modal one.
const open = [];

// Every modal overlay locks the page's scroll, and the page scrolls again only
// once the last of them lets go. The page hears of both, as `modalchange`.
let scrollLocks = 0;

const announceModal = open => {
    document.dispatchEvent(new CustomEvent("modalchange", { detail: open }));
};

const focusable =
    "a[href], button:not(:disabled), input:not(:disabled), select:not(:disabled), textarea:not(:disabled), summary";

const focusablesIn = element =>
    [...element.querySelectorAll(focusable)].filter(candidate => candidate.getClientRects().length > 0);

const topModal = () => [...open].reverse().find(entry => entry.modal);

// Everything beside the layer and beside each of its ancestors goes inert, so
// the page behind can't be clicked, focused or read. What was inert already is
// left alone, so releasing doesn't wake it.
const inertAround = layer => {
    const taken = [];
    for (let node = layer; node && node !== document.body; node = node.parentElement) {
        for (const sibling of node.parentElement.children) {
            if (sibling !== node && !sibling.inert) {
                sibling.inert = true;
                taken.push(sibling);
            }
        }
    }
    return taken;
};

// A dropdown hangs from its button, so one opened near the window's right edge
// moves left until it is clear of it by the page's margin.
const keepInView = layer => {
    const dropdown = layer.firstElementChild;
    if (dropdown) {
        const margin = 16;
        const overshoot = dropdown.getBoundingClientRect().right - (document.documentElement.clientWidth - margin);
        if (overshoot > 0) {
            dropdown.style.translate = `${-overshoot}px 0`;
        }
    }
};

export const hold = layer => modal => onClose => () => {
    const opener = document.activeElement;
    const entry = { layer, modal };
    open.push(entry);

    const taken = modal ? inertAround(layer) : [];
    if (!modal) {
        keepInView(layer);
    }
    if (modal && scrollLocks++ === 0) {
        document.body.style.overflow = "hidden";
        announceModal(true);
    }

    const body = layer.querySelector(".overlay-body");
    const first = (body && focusablesIn(body)[0]) || focusablesIn(layer)[0];
    if (first) {
        first.focus();
    }

    const onKeyDown = event => {
        if (event.key === "Escape" && open[open.length - 1] === entry) {
            event.preventDefault();
            onClose();
        }
        else if (event.key === "Tab" && topModal() === entry) {
            const focusables = focusablesIn(layer);
            if (focusables.length === 0) {
                event.preventDefault();
                return;
            }
            const firstFocusable = focusables[0];
            const lastFocusable = focusables[focusables.length - 1];
            if (!layer.contains(document.activeElement)) {
                event.preventDefault();
                firstFocusable.focus();
            }
            else if (event.shiftKey && document.activeElement === firstFocusable) {
                event.preventDefault();
                lastFocusable.focus();
            }
            else if (!event.shiftKey && document.activeElement === lastFocusable) {
                event.preventDefault();
                firstFocusable.focus();
            }
        }
    };

    // A dropdown closes on a press anywhere but itself and the button that
    // opened it, which toggles it on its own.
    const onPointerDown = event => {
        if (!layer.contains(event.target) && !(opener && opener.contains(event.target))) {
            onClose();
        }
    };

    document.addEventListener("keydown", onKeyDown);
    if (!modal) {
        document.addEventListener("pointerdown", onPointerDown);
    }

    return () => {
        document.removeEventListener("keydown", onKeyDown);
        document.removeEventListener("pointerdown", onPointerDown);
        open.splice(open.indexOf(entry), 1);
        taken.forEach(element => { element.inert = false; });
        if (modal && --scrollLocks === 0) {
            document.body.style.overflow = "";
            announceModal(false);
        }
        // Focus goes back to the opener unless the player has put it
        // somewhere else, as a press outside a dropdown does.
        const active = document.activeElement;
        const lost = !active || active === document.body || layer.contains(active) || !active.isConnected;
        if (lost && opener && opener.isConnected && opener !== document.body) {
            opener.focus();
        }
    };
};
