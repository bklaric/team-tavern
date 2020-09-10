module TeamTavern.Client.Pages.Home.Wizard where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Home.Wizard.EnterGeneralPlayerDetails (enterGeneralPlayerDetails)
import TeamTavern.Client.Pages.Home.Wizard.EnterGeneralPlayerDetails as EnterGeneralPlayerDetails
import TeamTavern.Client.Pages.Home.Wizard.EnterProfilePlayerDetails (enterProfilePlayerDetails)
import TeamTavern.Client.Pages.Home.Wizard.EnterProfilePlayerDetails as EnterProfilePlayerDetails
import TeamTavern.Client.Pages.Home.Wizard.EnterRegistrationDetails (enterRegistrationDetails)
import TeamTavern.Client.Pages.Home.Wizard.EnterRegistrationDetails as EnterRegistrationDetails
import TeamTavern.Client.Pages.Home.Wizard.SelectGame (selectGame)
import TeamTavern.Client.Pages.Home.Wizard.SelectGame as SelectGame
import TeamTavern.Client.Pages.Home.Wizard.Shared (Ilk(..))
import TeamTavern.Server.Game.View.SendResponse (OkContent)
import TeamTavern.Server.Player.Register.SendResponse as Register
import TeamTavern.Server.Wizard.CreateAccount as CreateAccount

type Handle = String

type Game = OkContent

data Step
    = SelectGame
    | EnterGeneralPlayerDetails
    | EnterProfilePlayerDetails
    | EnterRegistrationDetails

type Input = { ilk :: Ilk }

type Output = {}

type State =
    { ilk :: Ilk
    , step :: Step
    , handle :: Maybe String
    , game :: Maybe Game
    , generalPlayerDetailsOutput :: Maybe EnterGeneralPlayerDetails.Output
    , profilePlayerDetailsInput :: Maybe EnterProfilePlayerDetails.Input
    , registrationDetailsInput :: EnterRegistrationDetails.Input
    }

data Action
    = TakeSelectedGame Handle
    | TakeGeneralPlayerDetails EnterGeneralPlayerDetails.Output
    | TakeProfilePlayerDetails EnterProfilePlayerDetails.Output
    | TakeRegistrationDetails EnterRegistrationDetails.Output
    | SetStep Step

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Output) Unit

type Slots slots =
    ( selectGame :: SelectGame.Slot
    , enterGeneralPlayerDetails :: EnterGeneralPlayerDetails.Slot
    , enterProfilePlayerDetails :: EnterProfilePlayerDetails.Slot
    , enterRegistrationDetails :: EnterRegistrationDetails.Slot
    | slots )

profileIlk :: Ilk -> String
profileIlk Player = "player"
profileIlk Team = "team"

render :: forall slots left.
    State -> HH.ComponentHTML Action (Slots slots) (Async left)
render state @ { step, ilk } =
    HH.div
    [ HP.class_ $ HH.ClassName "wide-single-form-container wizard" ]
    case step of
        SelectGame ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Select game" ]
            , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
                [ HH.text $ "Select one of the games below to start creating your "
                    <> profileIlk ilk <> " profile"
                ]
            , selectGame { ilk, selectedHandle: state.handle } (Just <<< TakeSelectedGame)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HP.disabled $ isNothing state.handle
                    , HE.onClick $ const $ Just $ SetStep EnterGeneralPlayerDetails
                    ]
                    [ HH.text "Next" ]
                ]
            ]
        EnterGeneralPlayerDetails ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Enter player details" ]
            , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
                [ HH.text "Fill out general details about yourself." ]
            , enterGeneralPlayerDetails
                (state.generalPlayerDetailsOutput >>= EnterGeneralPlayerDetails.outputToInput)
                (Just <<< TakeGeneralPlayerDetails)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HE.onClick $ const $ Just $ SetStep EnterProfilePlayerDetails
                    ]
                    [ HH.text "Next" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "form-back-button"
                    , HE.onClick $ const $ Just $ SetStep SelectGame
                    ]
                    [ HH.text "Back" ]
                ]
            ]
        EnterProfilePlayerDetails | Just input <- state.profilePlayerDetailsInput ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Enter profile details" ]
            , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
                [ HH.text "Fill out game related details about yourself." ]
            , enterProfilePlayerDetails input (Just <<< TakeProfilePlayerDetails)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HE.onClick $ const $ Just $ SetStep EnterRegistrationDetails
                    ]
                    [ HH.text "Next" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "form-back-button"
                    , HE.onClick $ const $ Just $ SetStep EnterGeneralPlayerDetails
                    ]
                    [ HH.text "Back" ]
                ]
            ]
        EnterRegistrationDetails ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Enter registration details" ]
            , enterRegistrationDetails state.registrationDetailsInput (Just <<< TakeRegistrationDetails)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    -- , HE.onClick $ const $ Just $ SetStep EnterProfilePlayerDetails
                    ]
                    [ HH.text "Next" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "form-back-button"
                    , HE.onClick $ const $ Just $ SetStep EnterProfilePlayerDetails
                    ]
                    [ HH.text "Back" ]
                ]
            ]
        _ -> []

loadGame :: forall left. String -> Async left (Maybe Game)
loadGame handle = Async.unify do
    response <-
        Fetch.fetch_ ("/api/games/by-handle/" <> handle)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just content

-- sendRequest :: forall left. State -> Async left (Either CreateAccount.BadRequestContent CreateAccount.OkContent)
-- sendRequest state = pure $ Left {}

profilePlayerInput :: Game -> EnterProfilePlayerDetails.Input
profilePlayerInput game =
    { fields: game.fields
    , fieldValues: []
    , newOrReturning: false
    , summary: ""
    , urlErrors: []
    , missingErrors: []
    , summaryError: false
    }

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction (TakeSelectedGame handle) = do
    H.modify_ _ { handle = Just handle }
handleAction (TakeGeneralPlayerDetails details) =
    H.modify_ _ { generalPlayerDetailsOutput = Just details }
handleAction (TakeProfilePlayerDetails details) =
    H.modify_ \state -> state
        { profilePlayerDetailsInput = state.profilePlayerDetailsInput <#> _
            { fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , summary = details.summary
            }
        }
handleAction (TakeRegistrationDetails details) =
    H.modify_ \state -> state
        { registrationDetailsInput = state.registrationDetailsInput
            { email = details.email
            , nickname = details.nickname
            , password = details.password
            }
        }
handleAction (SetStep step) = do
    state <- H.get
    case state.step, state.handle of
        SelectGame, Just handle -> do
            game <- H.lift $ loadGame handle
            case game of
                Just game' -> H.modify_ _
                    { step = step
                    , game = Just game'
                    , profilePlayerDetailsInput = Just $ profilePlayerInput game'
                    }
                Nothing -> pure unit
        _, _ -> H.modify_ _ { step = step }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ ilk } ->
        { ilk
        , step: SelectGame
        , handle: Nothing
        , game: Nothing
        , generalPlayerDetailsOutput: Nothing
        , profilePlayerDetailsInput: Nothing
        , registrationDetailsInput: EnterRegistrationDetails.emptyInput
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }

wizard
    :: forall action slots left
    .  (Modal.Message Output -> Maybe action)
    -> HH.ComponentHTML action (wizard :: Slot | slots) (Async left)
wizard handleOutput = HH.slot
    (SProxy :: SProxy "wizard") unit
    (Modal.component component) unit handleOutput
