module TeamTavern.Client.Pages.Wizard where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails (enterPlayerDetails)
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails as EnterPlayerDetails
import TeamTavern.Client.Script.Navigate (navigate_)

data Step = Greeting | PlayerDetails

type Input = { step :: Step, nickname :: String, playerDetails :: EnterPlayerDetails.Input }

type State = { step :: Step, nickname :: String, playerDetails :: EnterPlayerDetails.Input }

data Action
    = Receive Input
    | Skip
    | SetStep Step
    | UpdatePlayerDetails EnterPlayerDetails.Output

type Slot = H.Slot (Const Void) Void Unit

renderPage { step: Greeting, nickname } =
    [ HH.div [ HP.class_ $ HH.ClassName "page-wizard-step" ]
        [ HH.h1 [ HP.class_ $ HH.ClassName "page-wizard-step-title" ]
            [ HH.text $ "Hi, " <> nickname <> "!" ]
        , HH.p [ HP.class_ $ HH.ClassName "page-wizard-step-description" ]
            [ HH.text """Let's start with setting up your TeamTavern account and
                your first game profile."""
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "page-wizard-step-buttons" ]
        [ HH.button
            [ HP.class_ $ HH.ClassName "secondary-button"
            , HE.onClick $ const $ Just Skip
            ]
            [ HH.text "Skip" ]
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HE.onClick $ const $ Just $ SetStep PlayerDetails
            ]
            [ HH.text "Let's go" ]
        ]
    ]
renderPage { step: PlayerDetails, playerDetails } =
    [HH.div [ HP.class_ $ HH.ClassName "page-wizard-step" ]
        [ HH.h1 [ HP.class_ $ HH.ClassName "page-wizard-step-title" ]
                [ HH.text "Player details" ]
        , HH.p [ HP.class_ $ HH.ClassName "page-wizard-step-description" ]
            [ HH.text """Enter details about yourself so your new bruh gamer friends
                can find you, bruh. Fill out as much as you can to ensure the
                bruhest gamers find you. All fields are optional, bruh."""
            ]
        , enterPlayerDetails playerDetails (Just <<< UpdatePlayerDetails)
        ]
    , HH.div [ HP.class_ $ HH.ClassName "page-wizard-step-buttons" ]
        [ HH.button
            [ HP.class_ $ HH.ClassName "secondary-button page-wizard-step-button"
            , HE.onClick $ const $ Just $ SetStep Greeting ]
            [ HH.text "Back" ]
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button page-wizard-step-button"
            , HE.onClick $ const $ Just $ SetStep PlayerDetails
            ]
            [ HH.text "Next" ]
        ]
    ]

-- render :: forall slots. State -> HH.HTML slots Action
render state = HH.div [ HP.class_ $ HH.ClassName "page-wizard"]
    $ renderPage state

handleAction (Receive input) =
    H.put input
handleAction Skip = do
    { nickname } <- H.get
    H.liftEffect $ navigate_ $ "/players/" <> nickname
handleAction (SetStep step) =
    H.liftEffect $ navigate_
        case step of
        Greeting -> "/wizard/greeting"
        PlayerDetails -> "/wizard/player"
handleAction (UpdatePlayerDetails details) =
    H.modify_ \state -> state
        { playerDetails = state.playerDetails
            { birthday = details.birthday
            , location = details.location
            , languages = details.languages
            , microphone = details.microphone
            , discordTag = details.discordTag
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            , about = details.about
            }
        }

-- component :: forall query output monad. MonadEffect monad =>
--     H.Component HH.HTML query Input output monad
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

-- wizard :: forall action slots monad. MonadEffect monad =>
--     Input -> HH.ComponentHTML action (wizard :: Slot | slots) monad
wizard input = HH.slot (SProxy :: SProxy "wizard") unit component input absurd
