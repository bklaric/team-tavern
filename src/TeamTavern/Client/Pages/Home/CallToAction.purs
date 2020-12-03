module TeamTavern.Client.Pages.Home.CallToAction where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Landing (landingSectionButton)
import Web.UIEvent.MouseEvent (MouseEvent)

titleOrEsports :: Maybe String -> String
titleOrEsports = maybe "esports" identity

callToAction :: forall action slots. Maybe String -> (MouseEvent -> action) -> HH.HTML slots action
callToAction title createAccount =
    HH.div [ HP.class_ $ HH.ClassName "call-to-action" ]
    [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-content" ]
        [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-text" ] $
            [ HH.h1 [ HP.class_ $ HH.ClassName "call-to-action-heading" ]
                [ HH.text $ "Find your " <> titleOrEsports title <> " teammates" ]
            , HH.p [ HP.class_ $ HH.ClassName "call-to-action-paragraph" ]
                [ HH.text $ """Search through player and team profiles to find your new """
                    <> titleOrEsports title <> """ teammates. Create
                    your own player or team profile and let them find you."""
                ]
            , landingSectionButton "Start finding teammates" "/preboarding/start" createAccount
            ]
        ]
    ]
