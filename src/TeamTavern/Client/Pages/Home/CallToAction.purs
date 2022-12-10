module TeamTavern.Client.Pages.Home.CallToAction where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.ArticledNoun (indefiniteNoun)
import TeamTavern.Client.Snippets.Class as HS
import Web.UIEvent.MouseEvent (MouseEvent)

callToActionButton :: ∀ slots action.
    String -> String -> String -> (MouseEvent -> action) -> HH.HTML slots action
callToActionButton icon text url onClick =
    HH.a
    [ HS.class_ "call-to-action-button"
    , HP.href url
    , HE.onClick onClick
    ]
    [ HH.i [ HS.class_ $ icon <> " button-icon" ] [], HH.text text ]

callToAction :: ∀ action slots.
    (MouseEvent -> action) -> (MouseEvent -> action) -> HH.HTML slots action
callToAction viewGames createAccount =
    HH.div [ HS.class_ "call-to-action" ]
    [ HH.video
        [ HS.class_ "call-to-action-video"
        , HP.autoplay true
        , HP.loop true
        , HP.muted true
        , HP.src "/images/video.mp4"
        ]
        []
    , HH.div [ HS.class_ "call-to-action-overlay" ] []
    , HH.div [ HS.class_ "call-to-action-content" ]
        [ HH.div [ HS.class_ "call-to-action-text" ] $
            [ HH.h1 [ HS.class_ "call-to-action-heading" ]
                [ HH.text $ "Find your esports teammates" ]
            , HH.p [ HS.class_ "call-to-action-paragraph" ]
                [ HH.text $ "Find esports players and teams looking for teammates on TeamTavern, "
                    <> "an esports team finding platform. "
                    <> "Create your own player or team profile and let them find you."
                ]
            , HH.div [ HS.class_ "call-to-action-buttons" ]
                [ callToActionButton "fas fa-gamepad" "View games" "/games" viewGames
                , callToActionButton "fas fa-user-plus" "Create profile" "/preboarding/start" createAccount
                ]
            ]
        ]
    ]

callToAction'
    :: ∀ action slots
    .  String -> String
    -> (MouseEvent -> action) -> (MouseEvent -> action) -> (MouseEvent -> action)
    -> HH.HTML slots action
callToAction' handle title viewPlayers viewTeams createAccount =
    HH.div [ HS.class_ "call-to-action" ]
    [ HH.video
        [ HS.class_ "call-to-action-video"
        , HP.autoplay true
        , HP.loop true
        , HP.muted true
        , HP.src $ "/images/" <> handle <> "/video.mp4"
        ]
        []
    , HH.div [ HS.class_ "call-to-action-overlay" ] []
    , HH.div [ HS.class_ "call-to-action-content" ]
        [ HH.div [ HS.class_ "call-to-action-text" ] $
            [ HH.h1 [ HS.class_ "call-to-action-heading" ]
                [ HH.text $ "Find your " <> title <> " teammates" ]
            , HH.p [ HS.class_ "call-to-action-paragraph" ]
                [ HH.text $ "Find " <> title
                    <> " players and teams looking for teammates on TeamTavern, "
                    <> indefiniteNoun title <> " team finding platform. "
                    <> "Create your own player or team profile and let them find you."
                ]
            , HH.div [ HS.class_ "call-to-action-buttons" ]
                [ callToActionButton "fas fa-user" "Find players" ("/games/" <> handle <> "/players") viewPlayers
                , callToActionButton "fas fa-users" "Find teams" ("/games/" <> handle <> "/teams") viewTeams
                , callToActionButton "fas fa-user-plus" "Create profile" "/preboarding/start" createAccount
                ]
            ]
        ]
    ]
