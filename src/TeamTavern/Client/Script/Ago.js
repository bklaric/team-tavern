export const millisOf = time => Date.parse(time)

export const isoOf = time => new Date(time).toISOString()
