module TeamTavern.Client.Pages.Account (Slot, ChildSlots, account) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Pages.Account.AccountHeader (accountHeader)
import TeamTavern.Client.Pages.Account.AccountHeader as AccountHeader
import TeamTavern.Client.Pages.Account.Details (details)
import TeamTavern.Client.Pages.Account.Details as Details
import TeamTavern.Client.Pages.Account.PlayerProfiles (playerProfiles)
import TeamTavern.Client.Pages.Account.PlayerProfiles as PlayerProfiles
import TeamTavern.Client.Pages.Account.TeamProfiles (teamProfiles)
import TeamTavern.Client.Pages.Account.TeamProfiles as TeamProfiles
import TeamTavern.Client.Pages.Account.Types (PlayerStatus(..), Nickname)
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate_)

data Action
    = Init
    | Receive AccountHeader.Tab

data State
    = Empty AccountHeader.Tab
    | Player Nickname AccountHeader.Tab

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( accountHeader :: AccountHeader.Slot
    , details :: Details.Slot
    , playerProfiles :: PlayerProfiles.Slot
    , teamProfiles :: TeamProfiles.Slot
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Player nickname tab) = HH.div_ $
    [ accountHeader nickname tab ]
    <>
    case tab of
    AccountHeader.Profiles ->
        [ details nickname SamePlayer (SProxy :: SProxy "details")
        , playerProfiles nickname SamePlayer (SProxy :: SProxy "playerProfiles")
        , teamProfiles nickname SamePlayer (SProxy :: SProxy "teamProfiles")
        ]
    AccountHeader.Conversations ->
        [ ]
    AccountHeader.Conversation nickname' ->
        [ ]

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    playerInfo <- H.liftEffect getPlayerInfo
    case Tuple state playerInfo of
        Tuple _ Nothing -> H.liftEffect $ navigate_ "/"
        Tuple (Empty tab) (Just { id, nickname }) -> do
            H.put $ Player nickname tab
            H.liftEffect do
                setMetaTitle "Account | TeamTavern"
                setMetaDescription $ "View your account on TeamTavern."
                setMetaUrl
        _ -> pure unit
handleAction (Receive tab) = do
    state <- H.get
    case state of
        Player nickname _ -> H.put $ Player nickname tab
        _ -> pure unit

component :: forall query output left.
    H.Component HH.HTML query AccountHeader.Tab output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init
        , receive = Just <<< Receive
        }
    }

account :: forall query children left.
    AccountHeader.Tab -> HH.ComponentHTML query (account :: Slot | children) (Async left)
account tab = HH.slot (SProxy :: SProxy "account") unit component tab absurd
