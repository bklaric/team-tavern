"use strict";

exports.registerPageView = function () {
  if (window.gtag) {
    gtag('config', 'UA-150934365-1', { 'page_path': location.pathname });
  }
}
