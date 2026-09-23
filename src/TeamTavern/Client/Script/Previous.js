export const stampPrevious = previous => () => {
    history.replaceState({ ...history.state, previous }, "");
};
