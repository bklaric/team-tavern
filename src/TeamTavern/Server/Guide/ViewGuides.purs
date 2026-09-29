module TeamTavern.Server.Guide.ViewGuides (viewGuides) where

import Prelude

import Async (Async)
import Jarilo (ok_)
import TeamTavern.Server.Guide.Guides (guides)

viewGuides :: ∀ left. Async left _
viewGuides = pure $ ok_ $ guides <#> \{ slug, heading, description, updated } ->
    { slug, heading, description, updated }
