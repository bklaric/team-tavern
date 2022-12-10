module TeamTavern.Client.Components.Picture where

import Prelude

import Data.MediaType (MediaType(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

picture :: ∀ slots action. String -> String -> String -> HH.HTML slots action
picture class_ alt baseSrc =
    HH.element (HH.ElemName "picture") []
    [ HH.source
        [ HP.prop (HH.PropName "srcset") $ baseSrc <> ".webp"
        , HP.type_ $ MediaType "image/webp"
        ]
    , HH.img
        [ HS.class_ class_
        , HP.alt alt
        , HP.src $ baseSrc <> ".png"
        ]
    ]
