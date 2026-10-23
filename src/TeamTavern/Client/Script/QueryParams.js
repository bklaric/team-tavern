const storageKey = "tt-link-secrets";

// The parameters index.html took out of the address for holding a secret, raw,
// while their page is open: in memory, or after a reload in session storage.
const linkSecrets = () => {
    let kept = self.__linkSecrets;
    if (!kept) {
        try { kept = JSON.parse(sessionStorage.getItem(storageKey)); } catch (error) {}
    }
    return kept && kept.path === window.location.pathname ? kept.pairs : [];
};

const keepLinkSecrets = pairs => {
    const kept = { path: window.location.pathname, pairs };
    self.__linkSecrets = kept;
    try { sessionStorage.setItem(storageKey, JSON.stringify(kept)); } catch (error) {}
};

const pairs = () =>
    window.location.search.substring(1).split("&").filter(pair => pair !== "").concat(linkSecrets());

export const openedQuery = () => pairs().join("&");

export const getQueryParamImpl = function (param) {
    return function () {
        var vars = pairs();
        for (var i = 0; i < vars.length; i++) {
            var pair = vars[i].split("=");
            if (pair[0] == param) { return pair[1]; }
        }
        return null;
    }
}

export const removeQueryParamImpl = param => () => {
    const secrets = linkSecrets();
    if (secrets.length > 0) {
        keepLinkSecrets(secrets.filter(pair => pair.split("=")[0] !== param));
    }
    const url = new URL(window.location.href);
    url.searchParams.delete(param);
    history.replaceState(history.state, "", url.pathname + url.search + url.hash);
};

export const forgetLinkSecrets = () => {
    delete self.__linkSecrets;
    try { sessionStorage.removeItem(storageKey); } catch (error) {}
};
