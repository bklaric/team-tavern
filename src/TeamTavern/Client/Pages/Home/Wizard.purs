module TeamTavern.Client.Pages.Home.Wizard where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
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
import TeamTavern.Client.Pages.Home.Wizard.SelectGame (selectGame)
import TeamTavern.Client.Pages.Home.Wizard.SelectGame as SelectGame
import TeamTavern.Client.Pages.Home.Wizard.Shared (Ilk)
import TeamTavern.Server.Game.View.SendResponse (OkContent)

type Handle = String

type Game = OkContent

data Step
    = SelectGame
    | EnterGeneralPlayerDetails
    | EnterGamePlayerDetails

type Input = { ilk :: Ilk }

type Output = {}

type State =
    { ilk :: Ilk
    , step :: Step
    , handle :: Maybe String
    , game :: Maybe Game
    , generalPlayerDetailsInput :: Maybe EnterGeneralPlayerDetails.Input
    , generalPlayerDetailsOutput :: Maybe EnterGeneralPlayerDetails.Output
    }

data Action
    = TakeSelectedGame Handle
    | TakeGeneralPlayerDetails EnterGeneralPlayerDetails.Output
    | SetStep Step

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Output) Unit

type Slots slots =
    ( selectGame :: SelectGame.Slot
    , enterGeneralPlayerDetails :: EnterGeneralPlayerDetails.Slot
    | slots )

render :: forall slots left.
    State -> HH.ComponentHTML Action (Slots slots) (Async left)
render state @ { step, ilk } =
    HH.div
    [ HP.class_ $ HH.ClassName "wide-single-form-container" ]
    case step of
        SelectGame ->
            [ selectGame { ilk, selectedHandle: state.handle } (Just <<< TakeSelectedGame)
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
            [ enterGeneralPlayerDetails Nothing
                (Just <<< TakeGeneralPlayerDetails)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HE.onClick $ const $ Just $ SetStep EnterGamePlayerDetails
                    ]
                    [ HH.text "Next" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "form-back-button"
                    , HE.onClick $ const $ Just $ SetStep SelectGame
                    ]
                    [ HH.text "Back" ]
                ]
            ]
        EnterGamePlayerDetails -> []

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

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction (TakeSelectedGame handle) = do
    H.modify_ _ { handle = Just handle }
handleAction (TakeGeneralPlayerDetails details) =
    H.modify_ _ { generalPlayerDetailsOutput = Just details }
handleAction (SetStep step) = do
    state <- H.get
    case state.step, state.handle of
        SelectGame, Just handle -> do
            game <- H.lift $ loadGame handle
            case game of
                Just game' -> H.modify_ _ { step = step, game = Just game' }
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
        , generalPlayerDetailsInput: Nothing
        , generalPlayerDetailsOutput: Nothing
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
