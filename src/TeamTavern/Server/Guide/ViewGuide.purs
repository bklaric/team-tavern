module TeamTavern.Server.Guide.ViewGuide (viewGuide) where

import Prelude

import Async (Async)
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Jarilo (NotFoundRow_, OkRow, notFound__, ok_)
import TeamTavern.Routes.Guide.ViewGuide as ViewGuide
import TeamTavern.Server.Guide.Guides (guides)
import Type.Row (type (+))

viewGuide :: ∀ left. String -> Async left (Variant (OkRow ViewGuide.OkContent + NotFoundRow_ + ()))
viewGuide slug = pure case guides # find (_.slug >>> eq slug) of
    Just { heading, title, description, published, updated, html } ->
        ok_ { heading, title, description, published, updated, html }
    Nothing -> notFound__
