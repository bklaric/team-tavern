export const openIdParamsOf = query =>
    Object.fromEntries([...new URLSearchParams(query)].filter(([key]) => key.startsWith("openid.")));
