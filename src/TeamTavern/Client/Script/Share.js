// A desktop browser's share sheet is the system's, which offers little a
// player pastes a link into, so only a touch screen gets it.
export const canShare = () =>
    typeof navigator.share === "function" && matchMedia("(pointer: coarse)").matches;

// A player closing the sheet is no failure.
export const shareImpl = errorCallback => successCallback => data => () => {
    navigator.share(data).then(
        () => successCallback(),
        error => error.name === "AbortError" ? successCallback() : errorCallback(error)
    );
};
