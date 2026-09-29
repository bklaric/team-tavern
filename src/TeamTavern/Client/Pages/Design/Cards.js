export const minutesBefore = now => minutes => new Date(now - minutes * 60000).toISOString()
