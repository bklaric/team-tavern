module TeamTavern.Client.Pages.Home.CallToAction where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Landing (landingSectionButton)
import TeamTavern.Client.Snippets.Class as HS
import Web.UIEvent.MouseEvent (MouseEvent)

titleOrEsports :: Maybe String -> String
titleOrEsports = maybe "esports" identity

callToAction :: forall action slots. Maybe String -> Maybe String -> (MouseEvent -> action) -> HH.HTML slots action
callToAction handle title createAccount =
    HH.div [ HS.class_ "call-to-action" ]
    [ HH.video
        [ HS.class_ "call-to-action-video"
        , HP.autoplay true
        , HP.loop true
        , HP.muted true
        , HP.src $ maybe "/images/video.mp4" (\handle' -> "/images/" <> handle' <> "/video.mp4") handle
        ]
        []
    , HH.div [ HS.class_ "call-to-action-overlay" ] []
    , HH.div [ HS.class_ "call-to-action-content" ]
        [ HH.div [ HS.class_ "call-to-action-text" ] $
            [ HH.h1 [ HS.class_ "call-to-action-heading" ]
                [ HH.text $ "Find your " <> titleOrEsports title <> " teammates" ]
            , HH.p [ HS.class_ "call-to-action-paragraph" ]
                [ HH.text $ """Search through player and team profiles to find your new """
                    <> titleOrEsports title <> """ teammates. Create
                    your own player or team profile and let them find you."""
                ]
            , landingSectionButton "Start finding teammates" "/preboarding/start" createAccount
            ]
        ]
    ]
