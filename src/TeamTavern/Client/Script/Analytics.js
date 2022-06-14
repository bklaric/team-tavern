export const registerPageView = function () {
  if (window.gtag) {
    gtag('config', 'UA-150934365-1', { 'page_path': location.pathname });
  }
}

export const sendEvent = function (action) {
  return function (category) {
    return function (label) {
      return function () {
        if (window.gtag) {
          gtag('event', action, {
            event_category: category,
            event_label: label
          })
        }
      }
    }
  }
}
