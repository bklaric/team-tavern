// The overlays open now, the newest last. Escape closes only the newest, and
// Tab is kept inside the newest modal one.
const open = [];

// Every modal overlay locks the page's scroll, and the page scrolls again only
// once the last of them lets go. The page hears of both, as `modalchange`.
let scrollLocks = 0;

const announceModal = open => {
    document.dispatchEvent(new CustomEvent("modalchange", { detail: open }));
};

// Safari doesn't focus a button it is pressing, so the control last pressed
// stands in for the focus as what opened an overlay.
let lastPressed = null;
document.addEventListener("pointerdown", event => {
    lastPressed = event.target instanceof Element ? event.target.closest("button, a[href], summary") : null;
}, true);

const openerNow = () => {
    const active = document.activeElement;
    return active && active !== document.body ? active : lastPressed;
};

const focusable =
    "a[href], button:not(:disabled), input:not(:disabled), select:not(:disabled), textarea:not(:disabled), "
    + "summary, [tabindex]:not([tabindex='-1'])";

// What Tab stops at: shown, and of a group of radios only the checked one, or
// the first where none is.
const tabStop = candidate => {
    if (candidate.getClientRects().length === 0) {
        return false;
    }
    if (candidate.type !== "radio" || candidate.checked || !candidate.name) {
        return true;
    }
    const group = [...(candidate.form || document).querySelectorAll("input[type=radio]")]
        .filter(radio => radio.name === candidate.name);
    return !group.some(radio => radio.checked) && group[0] === candidate;
};

const focusablesIn = element => [...element.querySelectorAll(focusable)].filter(tabStop);

// The regions that speak for the page, such as its toasts, stay live and in
// reach while a modal overlay holds the rest of it.
const live = () => [...document.querySelectorAll("[data-overlay-live]")];

const topModal = () => [...open].reverse().find(entry => entry.modal);

// Everything beside the layer and beside each of its ancestors goes inert, so
// the page behind can't be clicked, focused or read. What was inert already is
// left alone, so releasing doesn't wake it, and so are the live regions and
// what holds them.
const inertAround = layer => {
    const keep = live();
    const taken = [];
    for (let node = layer; node && node !== document.body; node = node.parentElement) {
        for (const sibling of node.parentElement.children) {
            if (sibling !== node && !sibling.inert && !keep.some(region => sibling.contains(region))) {
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

// Where the focus goes as an overlay opens: what the caller marked, or the
// first thing in its body, or failing that the first thing in it, such as
// Close. Only what is marked or in the body counts as arrived. On a touch
// screen a text field would raise the keyboard over the overlay it opens, so
// the overlay's heading takes the focus instead.
const typed = element => element.matches("textarea, input:not([type=checkbox], [type=radio], [type=button], [type=submit])");

const arrival = layer => {
    const marked = [...layer.querySelectorAll("[data-autofocus]")].find(tabStop);
    const body = layer.querySelector(".overlay-body") || layer.firstElementChild || layer;
    const first = marked || focusablesIn(body)[0];
    const heading = layer.querySelector("[role=dialog] h2[id]");
    if (first && typed(first) && heading && matchMedia("(pointer: coarse)").matches) {
        heading.tabIndex = -1;
        return heading;
    }
    return first;
};

export const hold = layer => modal => onClose => () => {
    const opener = openerNow();
    const entry = { layer, modal };
    open.push(entry);

    const taken = modal ? inertAround(layer) : [];
    if (modal && scrollLocks++ === 0) {
        document.body.style.overflow = "hidden";
        announceModal(true);
    }
    if (!modal) {
        keepInView(layer);
    }

    // An overlay whose content is still coming, as the notifications are, takes
    // the focus in once it comes, unless the player has moved it meanwhile.
    const fallback = arrival(layer) ? null : focusablesIn(layer)[0];
    const first = arrival(layer) || fallback;
    if (first) {
        first.focus();
    }
    const waiting = () => {
        const active = document.activeElement;
        return !active || active === document.body || active === opener || (fallback && active === fallback);
    };
    const arriving = new MutationObserver(() => {
        const arrived = arrival(layer);
        if (!waiting()) {
            arriving.disconnect();
        }
        else if (arrived) {
            arriving.disconnect();
            arrived.focus();
        }
    });
    if (!arrival(layer)) {
        arriving.observe(layer, { childList: true, subtree: true });
    }

    const onKeyDown = event => {
        if (event.key === "Escape" && open[open.length - 1] === entry) {
            event.preventDefault();
            onClose();
        }
        else if (event.key === "Tab" && topModal() === entry) {
            const focusables = [...focusablesIn(layer), ...live().flatMap(focusablesIn)];
            if (focusables.length === 0) {
                event.preventDefault();
                return;
            }
            const firstFocusable = focusables[0];
            const lastFocusable = focusables[focusables.length - 1];
            if (!focusables.includes(document.activeElement)) {
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
    // opened it, which toggles it on its own, and once the focus leaves both.
    // A press on something in it that takes no focus, such as a checkbox's
    // label, sends the focus to the nearest focusable element around it, and
    // `main#content` is one, so the focus landing around the dropdown hasn't
    // left it.
    const outside = target =>
        target instanceof Node && !layer.contains(target) && !(opener && opener.contains(target));
    const onPointerDown = event => {
        if (outside(event.target)) {
            onClose();
        }
    };
    const onFocusIn = event => {
        if (outside(event.target) && !event.target.contains(layer)) {
            onClose();
        }
    };

    document.addEventListener("keydown", onKeyDown);
    if (!modal) {
        document.addEventListener("pointerdown", onPointerDown);
        document.addEventListener("focusin", onFocusIn);
    }

    return () => {
        arriving.disconnect();
        document.removeEventListener("keydown", onKeyDown);
        document.removeEventListener("pointerdown", onPointerDown);
        document.removeEventListener("focusin", onFocusIn);
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
