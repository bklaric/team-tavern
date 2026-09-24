module TeamTavern.Routes.Sitemap.ViewSitemap where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, OkText)

-- | Caddy hands `/sitemap.xml` to node as it is, outside `/api`.
type ViewSitemap =
    Get_ (Literal "sitemap.xml")
    ==> OkText "application/xml; charset=utf-8" ! Internal_
