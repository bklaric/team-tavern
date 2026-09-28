module TeamTavern.Server.Guide.ViewGuide (viewGuide) where

import Prelude

import Async (Async)
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Jarilo (notFound__, ok_)
import TeamTavern.Server.Guide.Guides (guides)

viewGuide :: ∀ left. String -> Async left _
viewGuide slug = pure case guides # find (_.slug >>> eq slug) of
    Just { heading, title, description, published, updated, html } ->
        ok_ { heading, title, description, published, updated, html }
    Nothing -> notFound__
