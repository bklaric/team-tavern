module TeamTavern.Client.Pages.Wizard where

import Prelude

import Async (Async(..))
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Options ((:=))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails (enterPlayerDetails)
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails as EnterPlayerDetails
import TeamTavern.Client.Pages.Wizard.EnterPlayerProfileDetails (enterPlayerProfileDetails)
import TeamTavern.Client.Pages.Wizard.EnterPlayerProfileDetails as EnterPlayerProfileDetails
import TeamTavern.Client.Pages.Wizard.SelectGame (selectGame)
import TeamTavern.Client.Pages.Wizard.SelectGame as SelectGame
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Server.Wizard.Onboard as Onboard

data Step = Greeting | PlayerDetails | Game | PlayerProfileDetails

type Input = { step :: Step, nickname :: String }

type State =
    { step :: Step
    , nickname :: String
    , playerDetails :: EnterPlayerDetails.Input
    , game :: SelectGame.Input
    , playerProfileDetails :: EnterPlayerProfileDetails.Input
    }

data Action
    = Receive Input
    | Skip
    | SetStep Step
    | UpdatePlayerDetails EnterPlayerDetails.Output
    | UpdateGame SelectGame.Output
    | UpdatePlayerProfileDetails EnterPlayerProfileDetails.Output
    | SetUpAccount

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
            , HP.disabled $ isNothing game
            , HE.onClick $ const $ Just $ SetStep PlayerProfileDetails
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: PlayerProfileDetails, playerProfileDetails } =
    [ HH.div [ HP.class_ $ HH.ClassName "page-wizard-step" ]
        [ HH.h1 [ HP.class_ $ HH.ClassName "page-wizard-step-title" ]
                [ HH.text "Profile details" ]
        , HH.p [ HP.class_ $ HH.ClassName "page-wizard-step-description" ]
            [ HH.text """Enter details about your gameplay. Fill out everything."""
            ]
        , enterPlayerProfileDetails playerProfileDetails (Just <<< UpdatePlayerProfileDetails)
        ]
    , HH.div [ HP.class_ $ HH.ClassName "page-wizard-step-buttons" ]
        [ HH.button
            [ HP.class_ $ HH.ClassName "secondary-button page-wizard-step-button"
            , HE.onClick $ const $ Just $ SetStep Game ]
            [ HH.text "Back" ]
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button page-wizard-step-button"
            , HE.onClick $ const $ Just SetUpAccount
            ]
            [ HH.text "Done" ]
        ]
    ]

-- render :: forall slots. State -> HH.HTML slots Action
render state = HH.div [ HP.class_ $ HH.ClassName "page-wizard"]
    $ renderPage state

sendRequest :: forall left. State -> Async left (Maybe (Either Onboard.BadRequestContent Unit))
sendRequest (state :: State) = Async.unify do
    (body :: Onboard.RequestBody) <-
        case state.game, state.playerDetails, state.playerProfileDetails of
        Just game, personalDetails, profileDetails -> Async.right
            { handle: game.handle
            , personalDetails:
                { birthday: if String.null personalDetails.birthday then Nothing else Just personalDetails.birthday
                , location: personalDetails.location
                , languages: personalDetails.languages
                , microphone: personalDetails.microphone
                , discordTag: if String.null personalDetails.discordTag then Nothing else Just personalDetails.discordTag
                , timezone: personalDetails.timezone
                , weekdayFrom: if String.null personalDetails.weekdayFrom then Nothing else Just personalDetails.weekdayFrom
                , weekdayTo: if String.null personalDetails.weekdayTo then Nothing else Just personalDetails.weekdayTo
                , weekendFrom: if String.null personalDetails.weekendFrom then Nothing else Just personalDetails.weekendFrom
                , weekendTo: if String.null personalDetails.weekendTo then Nothing else Just personalDetails.weekendTo
                , about: personalDetails.about
                }
            , profileDetails:
                { fieldValues: profileDetails.fieldValues # Array.filter \{ url, optionKey, optionKeys } ->
                    isJust url || isJust optionKey || isJust optionKeys
                , newOrReturning: profileDetails.newOrReturning
                , summary: profileDetails.summary
                }
            }
        _, _, _ -> Async.left Nothing
    response <-
        Fetch.fetch "/api/wizard/onboard"
        ( Fetch.method := POST
        <> Fetch.body := Json.writeJSON body
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const Nothing)
    content :: Either Onboard.BadRequestContent Unit <-
        case FetchRes.status response of
        204 -> pure $ Right unit
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just content

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
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
        PlayerProfileDetails -> "/wizard/profile"
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
    H.modify_ \state -> state
        { game = Just game
        , playerProfileDetails = state.playerProfileDetails
            { fields = game.fields
            , fieldValues = []
            , newOrReturning = false
            , summary = ""
            }
        }
handleAction (UpdatePlayerProfileDetails details) =
    H.modify_ \state -> state
        { playerProfileDetails = state.playerProfileDetails
            { fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , summary = details.summary
            }
        }
handleAction SetUpAccount = do
    state <- H.get
    response <- H.lift $ sendRequest state
    case response of
        Just (Right _) -> H.liftEffect $ navigate_ "/account"
        _ -> logShow response

-- component :: forall query output monad.
--     H.Component HH.HTML query Input output (Async monad)
component = H.mkComponent
    { initialState:
        Record.insert (SProxy :: SProxy "playerDetails") EnterPlayerDetails.emptyInput
        >>> Record.insert (SProxy :: SProxy "game") Nothing
        >>> Record.insert (SProxy :: SProxy "playerProfileDetails") EnterPlayerProfileDetails.emptyInput
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

-- wizard :: forall action slots monad. MonadEffect monad =>
--     Input -> HH.ComponentHTML action (wizard :: Slot | slots) monad
wizard input = HH.slot (SProxy :: SProxy "wizard") unit component input absurd
