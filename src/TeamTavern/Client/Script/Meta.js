// The prerenderer serializes the script's text as it is, so a `<` in a name
// could close the element; JSON reads `<` as the same character.
export const setStructuredData_ = json => () => {
    let script = document.getElementById("structured-data");
    if (!script) {
        script = document.createElement("script");
        script.id = "structured-data";
        script.type = "application/ld+json";
        document.head.appendChild(script);
    }
    script.textContent = json.replace(/</g, "\\u003c");
};

export const clearStructuredData_ = () => {
    document.getElementById("structured-data")?.remove();
};
