module TeamTavern.Client.Components.Button
    ( Size(..)
    , Weight(..)
    , button
    , buttonLink
    , iconButton
    ) where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

-- Filled ember is the one thing on a screen that asks for a click; outlined is
-- for contact buttons and secondary actions; text is for Details, Sign in and
-- the like; destructive is outlined in error.
data Weight = Primary | Outline | Text | Destructive

data Size = Regular | Small

classes :: Weight -> Size -> String
classes weight size = "button " <> weightClass <> sizeClass
    where
    weightClass = case weight of
        Primary -> "button-primary"
        Outline -> "button-outline"
        Text -> "button-text"
        Destructive -> "button-destructive"
    sizeClass = case size of
        Regular -> ""
        Small -> " button-small"

-- An icon, if any, goes before the label.
button :: ∀ w i. Weight -> Size -> i -> Array (HH.HTML w i) -> HH.HTML w i
button weight size onClick =
    HH.button
    [ HS.class_ $ classes weight size, HP.type_ HP.ButtonButton, HE.onClick $ const onClick ]

-- A button that goes to a page of the site without reloading it.
buttonLink :: ∀ w m. MonadEffect m =>
    Weight -> Size -> String -> Array (HH.HTML w (m Unit)) -> HH.HTML w (m Unit)
buttonLink weight size path =
    HH.a [ HS.class_ $ classes weight size, HP.href path, HE.onClick $ navigateWithEvent_ path ]

iconButton :: ∀ w i. String -> i -> HH.HTML w i -> HH.HTML w i
iconButton label onClick icon =
    HH.button
    [ HS.class_ "icon-button"
    , HP.type_ HP.ButtonButton
    , HPA.label label
    , HE.onClick $ const onClick
    ]
    [ icon ]
