export const trackImpl = function (event) {
  return function (properties) {
    return function () {
      if (window.mixpanel) {
        mixpanel.track(event, properties)
      }
    }
  }
}

export const trackImpl_ = function (event) {
  return function () {
    if (window.mixpanel) {
      mixpanel.track(event)
    }
  }
}

export const aliasImpl = function (id) {
  return function () {
    if (window.mixpanel) {
      mixpanel.alias(id)
    }
  }
}

export const identifyImpl = function (id) {
  return function () {
    if (window.mixpanel) {
      mixpanel.identify(id)
    }
  }
}

export const registerImpl = function (properties) {
  return function (persistent) {
    return function () {
      if (window.mixpanel) {
        mixpanel.register(properties, { persistent })
      }
    }
  }
}
