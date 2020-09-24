module TeamTavern.Client.Pages.Wizard where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails (enterPlayerDetails)
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails as EnterPlayerDetails
import TeamTavern.Client.Pages.Wizard.SelectGame (selectGame)
import TeamTavern.Client.Pages.Wizard.SelectGame as SelectGame
import TeamTavern.Client.Script.Navigate (navigate_)

data Step = Greeting | PlayerDetails | Game

type Input = { step :: Step, nickname :: String }

type State = { step :: Step, nickname :: String, playerDetails :: EnterPlayerDetails.Input, game :: SelectGame.Input }

data Action
    = Receive Input
    | Skip
    | SetStep Step
    | UpdatePlayerDetails EnterPlayerDetails.Output
    | UpdateGame SelectGame.Output

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
    [ HH.div [ HP.class_ $ HH.ClassName "page-wizard-step" ]
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
            , HE.onClick $ const $ Just $ SetStep Game
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: Game, game } =
    [ HH.div [ HP.class_ $ HH.ClassName "page-wizard-step" ]
        [ HH.h1 [ HP.class_ $ HH.ClassName "page-wizard-step-title" ]
                [ HH.text "Game" ]
        , HH.p [ HP.class_ $ HH.ClassName "page-wizard-step-description" ]
            [ HH.text """Select a game to create your first profile muhfugga."""
            ]
        , selectGame game (Just <<< UpdateGame)
        ]
    , HH.div [ HP.class_ $ HH.ClassName "page-wizard-step-buttons" ]
        [ HH.button
            [ HP.class_ $ HH.ClassName "secondary-button page-wizard-step-button"
            , HE.onClick $ const $ Just $ SetStep PlayerDetails ]
            [ HH.text "Back" ]
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button page-wizard-step-button"
            , HE.onClick $ const $ Just $ SetStep Game
            ]
            [ HH.text "Next" ]
        ]
    ]

-- render :: forall slots. State -> HH.HTML slots Action
render state = HH.div [ HP.class_ $ HH.ClassName "page-wizard"]
    $ renderPage state

handleAction (Receive { step }) =
    H.modify_ _ { step = step }
handleAction Skip = do
    { nickname } <- H.get
    H.liftEffect $ navigate_ $ "/players/" <> nickname
handleAction (SetStep step) =
    H.liftEffect $ navigate_
        case step of
        Greeting -> "/wizard/greeting"
        PlayerDetails -> "/wizard/player"
        Game -> "/wizard/game"
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
handleAction (UpdateGame game) =
    H.modify_ _ { game = Just game }

-- component :: forall query output monad.
--     H.Component HH.HTML query Input output (Async monad)
component = H.mkComponent
    { initialState:
        Record.insert (SProxy :: SProxy "playerDetails") EnterPlayerDetails.emptyPlayerDetails
        >>> Record.insert (SProxy :: SProxy "game") Nothing
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

-- wizard :: forall action slots monad. MonadEffect monad =>
--     Input -> HH.ComponentHTML action (wizard :: Slot | slots) monad
wizard input = HH.slot (SProxy :: SProxy "wizard") unit component input absurd
