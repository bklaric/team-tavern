module TeamTavern.Client.Router (Query(..), router) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple.Nested ((/\))
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Pages.Home (home)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Pages.Privacy (privacyPolicy)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound)
import TeamTavern.Client.Shared.Slot (Slot___)

-- The history state rides along for the pages that will read it.
data Query send = ChangeRoute Foreign String send

data State
    = Empty
    | Home
    | Feed { handle :: String }
    | Post { handle :: String, id :: String }
    | PostType
    | PostGame { type_ :: String }
    | PostScreen { handle :: String, type_ :: String }
    | Matches { handle :: String, type_ :: String }
    | SignUp
    | SignIn
    | ForgotPassword
    | ResetPassword
    | ConfirmEmail
    | Renew
    | Messages
    | Conversation { conversation :: String }
    | Account
    | Privacy
    | Design
    | NotFound

type ChildSlots = (home :: Slot___)

route :: String -> State
route path =
    case split (Pattern "/") path of
    ["", ""] -> Home
    ["", "games", handle] -> Feed { handle }
    ["", "games", handle, "posts", id] -> Post { handle, id }
    ["", "post"] -> PostType
    ["", "post", type_] -> PostGame { type_ }
    ["", "games", handle, "post", type_] -> PostScreen { handle, type_ }
    ["", "games", handle, "post", type_, "live"] -> Matches { handle, type_ }
    ["", "signup"] -> SignUp
    ["", "signin"] -> SignIn
    ["", "forgot-password"] -> ForgotPassword
    ["", "reset-password"] -> ResetPassword
    ["", "confirm-email"] -> ConfirmEmail
    ["", "renew"] -> Renew
    ["", "messages"] -> Messages
    ["", "messages", conversation] -> Conversation { conversation }
    ["", "account"] -> Account
    ["", "privacy"] -> Privacy
    ["", "design"] -> Design
    _ -> NotFound

name :: State -> String
name Empty = ""
name Home = "TeamTavern"
name (Feed _) = "Feed"
name (Post _) = "Post"
name PostType = "New post"
name (PostGame _) = "New post"
name (PostScreen _) = "Post"
name (Matches _) = "Your post is live"
name SignUp = "Sign up"
name SignIn = "Sign in"
name ForgotPassword = "Forgot password"
name ResetPassword = "Reset password"
name ConfirmEmail = "Confirm email"
name Renew = "Renew post"
name Messages = "Messages"
name (Conversation _) = "Messages"
name Account = "Account"
name Privacy = "Privacy policy"
name Design = "Design"
name NotFound = "Page could not be found."

description :: String
description = "Find players and groups for your game on TeamTavern. Say who you're looking for and see who fits."

render :: ∀ action left. State -> H.ComponentHTML action ChildSlots (Async left)
render Empty = HH.div_ []
render Home = home
render Privacy = privacyPolicy
render page = placeholder $ name page

router :: ∀ input output left. Foreign -> String -> H.Component Query input output (Async left)
router _ initialPath = Hooks.component \{ queryToken } _ -> Hooks.do
    page /\ pageId <- Hooks.useState Empty

    let changeRoute path = do
            let page' = route path
            case page' of
                Home -> setMeta "TeamTavern" description
                NotFound -> do
                    appendRenderReadyNotFound
                    setMeta "Page not found | TeamTavern" description
                _ -> setMeta (name page' <> " | TeamTavern") description
            Hooks.put pageId page'

    Hooks.useLifecycleEffect do
        changeRoute initialPath
        pure Nothing

    Hooks.useQuery queryToken \(ChangeRoute _ path send) -> do
        changeRoute path
        pure $ Just send

    Hooks.pure $ render page
