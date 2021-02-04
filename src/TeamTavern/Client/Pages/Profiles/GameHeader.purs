module TeamTavern.Client.Pages.Profiles.GameHeader
    (Handle, Title, Tab(..), Input(..), component) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Content (contentHeader)
import TeamTavern.Client.Script.Navigate (navigate_)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Handle = String

type Title = String

data Tab = Players | Teams

derive instance eqTab :: Eq Tab

data Input = Input Handle Title Tab

type State = Input

data Action = Navigate String MouseEvent | Receive Input

playersPath :: String -> String
playersPath handle = "/games/" <> handle <> "/players"

teamsPath :: String -> String
teamsPath handle = "/games/" <> handle <> "/teams"

renderTabs :: forall slots. String -> Tab -> Array (HH.HTML slots Action)
renderTabs handle Players =
    [ HH.h1 [ HP.class_ $ HH.ClassName "content-header-tab" ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-user button-icon" ] []
        , HH.text "Players"
        ]
    , HH.a
        [ HP.class_ $ HH.ClassName "content-header-tab"
        , HP.href $ teamsPath handle
        , HE.onClick $ Just <<< Navigate (teamsPath handle)
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-users button-icon" ] []
        , HH.text "Teams"
        ]
    ]
renderTabs handle Teams =
    [ HH.a
        [ HP.class_ $ HH.ClassName "content-header-tab"
        , HP.href $ playersPath handle
        , HE.onClick $ Just <<< Navigate (playersPath handle)
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-user button-icon" ] []
        , HH.text "Players"
        ]
    , HH.h1 [ HP.class_ $ HH.ClassName "content-header-tab" ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-users button-icon" ] []
        , HH.text "Teams"
        ]
    ]

render :: forall slots. Input -> HH.HTML slots Action
render (Input handle title tab) = HH.div_
    [ contentHeader
        [ HH.div_
            [ HH.a
                [ HP.class_ $ HH.ClassName "content-heading"
                , HP.href $ "/games/" <> handle
                , HE.onClick $ Just <<< Navigate ("/games/" <> handle)
                ]
                [ HH.text title ]
            , HH.div [ HP.class_ $ HH.ClassName "content-header-tabs" ]
                (renderTabs handle tab)
            ]
        ]
    , HH.p [ HP.class_ $ HH.ClassName "content-description" ]
        [ HH.text
            case tab of
            Players -> "Search through " <> title <> " players looking for a team."
            Teams -> "Search through " <> title <> " teams looking for players."
        ]
    ]
handleAction :: forall monad.
    Bind monad => MonadEffect monad => MonadState Input monad =>
    Action -> monad Unit
handleAction (Navigate path mouseEvent) = do
    liftEffect $ preventDefault $ toEvent mouseEvent
    liftEffect $ navigate_ path
handleAction (Receive input) =
    H.put input

component :: forall query output monad. MonadEffect monad =>
    H.Component HH.HTML query Input output monad
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
