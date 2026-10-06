export const openIdParams = () =>
    Object.fromEntries([...new URLSearchParams(window.location.search)]
        .filter(([key]) => key.startsWith("openid.")));
