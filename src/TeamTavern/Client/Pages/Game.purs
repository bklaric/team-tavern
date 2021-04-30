module TeamTavern.Client.Pages.Game (Input, Slot, game) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Client.Pages.Home.ForTeams (forTeams')
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as Boarding
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Pages.Home.CallToAction (callToAction')
import TeamTavern.Client.Pages.Home.Features (features')
import TeamTavern.Client.Pages.Home.FindProfiles (findProfiles')
import TeamTavern.Client.Pages.Home.ForPlayers (forPlayers')
import TeamTavern.Client.Pages.Preboarding as Preboarding
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
import TeamTavern.Client.Snippets.ArticledNoun (indefiniteNoun)
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import TeamTavern.Routes.ViewGame as ViewGame
import Web.UIEvent.MouseEvent (MouseEvent)

type Input = { handle :: String }

data Action
    = Initialize
    | Receive Input
    | OpenPreboarding ViewGame.OkContent MouseEvent
    | OpenPlayerPreboarding ViewGame.OkContent MouseEvent
    | OpenTeamPreboarding ViewGame.OkContent MouseEvent
    | OpenPlayerProfiles String MouseEvent
    | OpenTeamProfiles String MouseEvent

data State
    = Empty { handle :: String }
    | Loaded { game :: ViewGame.OkContent }

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( viewAllPlayers :: NavigationAnchor.Slot Unit
    , viewAllTeams :: NavigationAnchor.Slot Unit
    )

render :: forall left.
    State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div [ HP.class_ $ HH.ClassName "home" ] []
render (Loaded { game: game' @ { handle, shortTitle } }) =
    HH.div [ HP.class_ $ HH.ClassName "home" ]
    [ callToAction' handle shortTitle (OpenPlayerProfiles handle) (OpenTeamProfiles handle) (OpenPreboarding game')
    , forPlayers' handle shortTitle (OpenPlayerPreboarding game')
    , forTeams' handle shortTitle (OpenTeamPreboarding game')
    , findProfiles' handle shortTitle (OpenPlayerProfiles handle) (OpenTeamProfiles handle)
    , features' handle shortTitle (OpenPreboarding game')
    ]

loadGame :: forall left. String -> Async left (Maybe ViewGame.OkContent)
loadGame handle = Async.unify do
    response <-
        Fetch.fetch_ ("/api/games/" <> handle)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just content

setMeta' :: forall monad. MonadEffect monad => String -> monad Unit
setMeta' title = setMeta (title <> " Team Finder | TeamTavern")
    ( "Find " <> title <> " players and teams looking for teammates on TeamTavern, " <> indefiniteNoun title <> " team finding platform. "
    <> "Create your own player or team profile and let them find you."
    )

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty { handle } -> do
            game' <- H.lift $ loadGame handle
            case game' of
                Just game'' -> do
                    H.put $ Loaded { game: game'' }
                    setMeta' game''.shortTitle
                Nothing -> pure unit
        _ -> pure unit
handleAction (Receive { handle }) = do
    game' <- H.lift $ loadGame handle
    case game' of
        Just game'' -> do
            H.put $ Loaded { game: game'' }
            setMeta' game''.shortTitle
        _ -> pure unit
handleAction (OpenPreboarding game' mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate (Preboarding.emptyInput Nothing (Just game')) "/preboarding/start"
handleAction (OpenPlayerPreboarding game' mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate (Preboarding.emptyInput (Just Boarding.Player) (Just game')) "/preboarding/start"
handleAction (OpenTeamPreboarding game' mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate (Preboarding.emptyInput (Just Boarding.Team) (Just game')) "/preboarding/start"
handleAction (OpenPlayerProfiles handle mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate_ $ "/games/" <> handle <> "/players"
handleAction (OpenTeamProfiles handle mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate_ $ "/games/" <> handle <> "/teams"

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

game :: forall query children left.
    Input -> HH.ComponentHTML query (game :: Slot | children) (Async left)
game input = HH.slot (SProxy :: SProxy "game") unit component input absurd
