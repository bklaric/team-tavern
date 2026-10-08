module TeamTavern.Server.Guide.ViewGuides (viewGuides) where

import Prelude

import Async (Async)
import Data.Variant (Variant)
import Jarilo (OkRow, ok_)
import TeamTavern.Routes.Guide.ViewGuides as ViewGuides
import TeamTavern.Server.Guide.Guides (guides)
import Type.Row (type (+))

viewGuides :: ∀ left. Async left (Variant (OkRow ViewGuides.OkContent + ()))
viewGuides = pure $ ok_ $ guides <#> \{ slug, heading, description, updated } ->
    { slug, heading, description, updated }
