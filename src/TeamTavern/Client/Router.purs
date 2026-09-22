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
import TeamTavern.Client.Components.Header (header)
import TeamTavern.Client.Pages.ConfirmEmail (confirmEmail)
import TeamTavern.Client.Pages.Design (design)
import TeamTavern.Client.Pages.ForgotPassword (forgotPassword)
import TeamTavern.Client.Pages.Home (home)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Pages.Privacy (privacyPolicy)
import TeamTavern.Client.Pages.ResetPassword (resetPassword)
import TeamTavern.Client.Pages.SignIn (signIn)
import TeamTavern.Client.Pages.SignUp (signUp)
import TeamTavern.Client.Script.Meta (setMeta, setMetaRobots)
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

type ChildSlots =
    ( header :: Slot___
    , home :: Slot___
    , signUp :: Slot___
    , signIn :: Slot___
    , forgotPassword :: Slot___
    , resetPassword :: Slot___
    , confirmEmail :: Slot___
    , design :: Slot___
    )

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

renderPage :: ∀ action left. State -> H.ComponentHTML action ChildSlots (Async left)
renderPage Empty = HH.div_ []
renderPage Home = home
renderPage SignUp = signUp
renderPage SignIn = signIn
renderPage ForgotPassword = forgotPassword
renderPage ResetPassword = resetPassword
renderPage ConfirmEmail = confirmEmail
renderPage Privacy = privacyPolicy
renderPage Design = design
renderPage page = placeholder $ name page

-- Every navigation is counted, even to the page already open, so the header
-- reads who is signed in on each.
type Visit = { page :: State, path :: String, visit :: Int }

render :: ∀ action left. Visit -> H.ComponentHTML action ChildSlots (Async left)
render { page, path, visit } = HH.div_ [ header { path, visit }, renderPage page ]

router :: ∀ input output left. Foreign -> String -> H.Component Query input output (Async left)
router _ initialPath = Hooks.component \{ queryToken } _ -> Hooks.do
    visit /\ visitId <- Hooks.useState { page: Empty, path: "", visit: 0 }

    let changeRoute path = do
            let page = route path
            case page of
                Home -> setMeta "TeamTavern" description
                NotFound -> do
                    appendRenderReadyNotFound
                    setMeta "Page not found | TeamTavern" description
                _ -> setMeta (name page <> " | TeamTavern") description
            -- The components page is a tool for building the site, not a page of it.
            setMetaRobots case page of
                Design -> "noindex"
                _ -> "index, follow"
            Hooks.modify_ visitId \{ visit: count } -> { page, path, visit: count + 1 }

    Hooks.useLifecycleEffect do
        changeRoute initialPath
        pure Nothing

    Hooks.useQuery queryToken \(ChangeRoute _ path send) -> do
        changeRoute path
        pure $ Just send

    Hooks.pure $ render visit
