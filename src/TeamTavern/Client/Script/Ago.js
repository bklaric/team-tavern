export const millisOf = time => Date.parse(time)

export const microsOf = time => {
    const fraction = /\.(\d+)/.exec(time)?.[1] ?? ""
    return Date.parse(time.replace(/\.\d+/, "")) * 1000 + Number(fraction.padEnd(6, "0").slice(0, 6))
}

export const isoOf = time => new Date(time).toISOString()
