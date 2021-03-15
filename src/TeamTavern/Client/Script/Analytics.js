"use strict";

exports.registerPageView = function () {
  gtag('config', 'UA-150934365-1', { 'page_path': location.pathname });
}
