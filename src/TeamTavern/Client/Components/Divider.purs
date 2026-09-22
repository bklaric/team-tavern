module TeamTavern.Client.Components.Divider (divider, rule, tierHeading) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

-- A tier of the feed, with how many posts it holds where that is known.
tierHeading :: ∀ w i. String -> Maybe Int -> HH.HTML w i
tierHeading heading count =
    HH.h3 [ HS.class_ "tier-heading" ] $
    [ HH.text heading ]
    <> maybe [] (\count' -> [ HH.span [ HS.class_ "tier-count tabular" ] [ HH.text $ show count' ] ]) count

-- A labelled line across the feed, above the posts that have expired.
divider :: ∀ w i. String -> HH.HTML w i
divider label = HH.div [ HS.class_ "divider" ] [ HH.text label ]

-- A labelled rule, "or", between two ways to do one thing.
rule :: ∀ w i. String -> HH.HTML w i
rule label = HH.div [ HS.class_ "rule" ] [ HH.text label ]
