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
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as Boarding
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Pages.Home.CallToAction (callToAction)
import TeamTavern.Client.Pages.Home.Features (features')
import TeamTavern.Client.Pages.Home.FindProfiles (findProfiles')
import TeamTavern.Client.Pages.Home.ForPlayers (forPlayers')
import TeamTavern.Client.Pages.Preboarding as Preboarding
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
import TeamTavern.Server.Game.View.SendResponse as Game

type Input = { handle :: String }

data Action
    = Initialize
    | Receive Input
    | OpenRegistration
    | OpenPlayerPreboarding Game.OkContent
    | OpenTeamPreboarding Game.OkContent
    | OpenPlayerProfiles String
    | OpenTeamProfiles String

data State
    = Empty { handle :: String }
    | Loaded { game :: Game.OkContent }

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( viewAllPlayers :: NavigationAnchor.Slot Unit
    , viewAllTeams :: NavigationAnchor.Slot Unit
    )

render :: forall left.
    State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div [ HP.class_ $ HH.ClassName "home" ] []
render (Loaded { game: game' @ { handle, title } }) =
    HH.div [ HP.class_ $ HH.ClassName "home" ]
    [ callToAction (Just title) OpenRegistration
    , forPlayers' title (OpenPlayerPreboarding game')
    , forTeams' title (OpenTeamPreboarding game')
    , findProfiles' title (OpenPlayerProfiles handle) (OpenTeamProfiles handle)
    , features' title OpenRegistration
    ]

loadGame :: forall left. String -> Async left (Maybe Game.OkContent)
loadGame handle = Async.unify do
    response <-
        Fetch.fetch_ ("/api/games/by-handle/" <> handle)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just content

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    H.liftEffect do
        setMetaTitle "Find your esports teammates | TeamTavern"
        setMetaDescription $
            "TeamTavern is an online platform for finding esports teammates. "
            <> "Choose a game, browse player and team profiles and find your ideal teammates."
        setMetaUrl
    state <- H.get
    case state of
        Empty { handle } -> do
            game' <- H.lift $ loadGame handle
            case game' of
                Just game'' -> H.put $ Loaded { game: game'' }
                Nothing -> pure unit
        _ -> pure unit
handleAction (Receive { handle }) = do
    game' <- H.lift $ loadGame handle
    case game' of
        Just game'' ->
            H.put $ Loaded { game: game'' }
        _ -> pure unit
handleAction OpenRegistration =
    navigate_ "/register"
handleAction (OpenPlayerPreboarding game') =
    navigate (Preboarding.emptyInput Boarding.Player $ Just game') "/preboarding/start"
handleAction (OpenTeamPreboarding game') =
    navigate (Preboarding.emptyInput Boarding.Team $ Just game') "/preboarding/start"
handleAction (OpenPlayerProfiles handle) =
    navigate_ $ "/games/" <> handle <> "/players"
handleAction (OpenTeamProfiles handle) =
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
