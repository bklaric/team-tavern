module TeamTavern.Client.Pages.Profiles.GameHeader
    (Handle, Title, ShortTitle, Tab(..), Input(..), component) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Content (contentDescription, contentHeader)
import TeamTavern.Client.Pages.Profiles.Ads (descriptionAd)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.Class as HS
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.Window (innerWidth)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Handle = String

type Title = String

type ShortTitle = String

data Tab = Players | Teams

derive instance eqTab :: Eq Tab

data Input = Input Handle Title ShortTitle Tab

type State = { input :: Input, adWidth :: Maybe Int }

data Action = Navigate String MouseEvent | Initialize | Receive Input

playersPath :: String -> String
playersPath handle = "/games/" <> handle <> "/players"

teamsPath :: String -> String
teamsPath handle = "/games/" <> handle <> "/teams"

renderTabs :: forall slots. String -> Tab -> Array (HH.HTML slots Action)
renderTabs handle Players =
    [ HH.h1 [ HS.class_ "content-header-tab" ]
        [ HH.i [ HS.class_ "fas fa-user button-icon" ] []
        , HH.text "Players"
        ]
    , HH.a
        [ HS.class_ "content-header-tab"
        , HP.href $ teamsPath handle
        , HE.onClick $ Just <<< Navigate (teamsPath handle)
        ]
        [ HH.i [ HS.class_ "fas fa-users button-icon" ] []
        , HH.text "Teams"
        ]
    ]
renderTabs handle Teams =
    [ HH.a
        [ HS.class_ "content-header-tab"
        , HP.href $ playersPath handle
        , HE.onClick $ Just <<< Navigate (playersPath handle)
        ]
        [ HH.i [ HS.class_ "fas fa-user button-icon" ] []
        , HH.text "Players"
        ]
    , HH.h1 [ HS.class_ "content-header-tab" ]
        [ HH.i [ HS.class_ "fas fa-users button-icon" ] []
        , HH.text "Teams"
        ]
    ]

render :: forall slots. State -> HH.HTML slots Action
render ({ input: Input handle title shortTitle tab, adWidth }) =
    HH.div_ $
    [ contentHeader
        [ HH.div_
            [ HH.a
                [ HS.class_ "content-heading"
                , HP.href $ "/games/" <> handle
                , HE.onClick $ Just <<< Navigate ("/games/" <> handle)
                ]
                [ HH.img
                    [ HS.class_ "content-heading-icon"
                    , HP.src $ "/images/" <> handle <> "/icon-orange.png"
                    ]
                , HH.text title
                ]
            , HH.div [ HS.class_ "content-header-tabs" ]
                (renderTabs handle tab)
            ]
        ]
    , contentDescription
        case tab of
        Players -> "Find " <> shortTitle <> " players looking for a team. Create your own player profile and let everyone know you're looking to team up."
        Teams -> "Find " <> shortTitle <> " teams looking for players. Create your own team profile and recruit new members for your team."
    ]
    <>
    case adWidth of
    Just width -> [ descriptionAd width ]
    Nothing -> []
handleAction :: forall monad.
    Bind monad => MonadEffect monad => MonadState State monad =>
    Action -> monad Unit
handleAction (Navigate path mouseEvent) = do
    liftEffect $ preventDefault $ toEvent mouseEvent
    liftEffect $ navigate_ path
handleAction Initialize = do
    width <- window >>= innerWidth # H.liftEffect
    H.modify_ _ { adWidth = Just $ max 1200 (width - 42) }
handleAction (Receive input) =
    H.modify_ _ { input = input }

component :: forall query output monad. MonadEffect monad =>
    H.Component HH.HTML query Input output monad
component = H.mkComponent
    { initialState: { input: _, adWidth: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }
