module TeamTavern.Client.Player where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Pages.Account.PlayerProfiles (playerProfiles)
import TeamTavern.Client.Pages.Account.PlayerProfiles as PlayerProfiles
import TeamTavern.Client.Pages.Account.TeamProfiles (teamProfiles)
import TeamTavern.Client.Pages.Account.TeamProfiles as TeamProfiles
import TeamTavern.Client.Pages.Account.Types (PlayerStatus(..))
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Server.Player.View.SendResponse as View
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action = Init String | Navigate String MouseEvent

data State
    = Empty
    | Player View.OkContent PlayerStatus
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( playerProfiles :: PlayerProfiles.Slot
    , teamProfiles :: TeamProfiles.Slot
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Player { nickname, about } playerStatus) = HH.div_
    [ HH.div [ HP.class_ $ ClassName "content-title" ]
        [ HH.h1 [ HP.class_ $ HH.ClassName "content-title-text" ] [ HH.text nickname ]
        , HH.div [ HP.class_ $ HH.ClassName "content-title-tabs" ]
            case playerStatus of
            SignedIn _ -> Array.singleton $
                HH.a
                [ HP.class_ $ HH.ClassName "content-title-tab"
                , HP.href $ "/account/conversations/" <> nickname
                , HE.onClick $ Just <<< Navigate ("/account/conversations/" <> nickname)
                ]
                [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
                , HH.text "Message player"
                ]
            _ -> []
        ]
    , HH.p [ HP.class_ $ HH.ClassName "content-description" ]
            [ HH.text $ "View all player and team profiles of player " <> nickname <> "." ]
    , playerProfiles nickname playerStatus (SProxy :: SProxy "playerProfiles")
    , teamProfiles nickname playerStatus (SProxy :: SProxy "teamProfiles")
    ]
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayer :: forall left. String -> Async left State
loadPlayer nickname = Async.unify do
    response <- Fetch.fetch_ ("/api/players/by-nickname/" <> nickname)
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    playerInfo <- Async.fromEffect getPlayerInfo
    case playerInfo of
        Just { nickname: nickname' } | content.nickname == nickname' -> do
            Async.fromEffect $ navigateReplace_ "/account"
            pure $ Player content SamePlayer
        Just { nickname: nickname' } ->
            pure $ Player content $ SignedIn nickname'
        Nothing -> pure $ Player content SignedOut

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init nickname') = do
    state <- H.lift $ loadPlayer nickname'
    H.put state
    let metaNickname =
            case state of
            Player { nickname } _ -> nickname
            _ -> nickname'
    H.lift $ Async.fromEffect do
        setMetaTitle $ metaNickname <> " | TeamTavern"
        setMetaDescription $ "View profiles by player " <> metaNickname <> " on TeamTavern."
        setMetaUrl
handleAction (Navigate path mouseEvent) = do
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    H.liftEffect $ navigate_ path

component :: forall query output left.
    String -> H.Component HH.HTML query String output (Async left)
component nickname = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init nickname
        , receive = Just <<< Init
        }
    }

player
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (player :: Slot Unit | children) (Async left)
player nickname = HH.slot
    (SProxy :: SProxy "player") unit (component nickname) nickname absurd
