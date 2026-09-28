// The browser names the running script only while it first runs, which is
// when this line does.
const bundle = document.currentScript?.src ?? "";

export const bundleName = bundle.slice(bundle.lastIndexOf("/") + 1);

export const onOwnError = report => () => {
    window.addEventListener("error", event => {
        if (bundle !== "" && event.filename === bundle) report(event.message)();
    });
};
