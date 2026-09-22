module TeamTavern.Client.Router (Query(..), router) where

import Prelude

import Async (Async)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), split)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Header (header)
import TeamTavern.Client.Pages.ConfirmEmail (confirmEmail)
import TeamTavern.Client.Pages.Design (design)
import TeamTavern.Client.Pages.Feed (FeedCache, feed)
import TeamTavern.Client.Pages.ForgotPassword (forgotPassword)
import TeamTavern.Client.Pages.Home (home)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Pages.Post.Game (postGame)
import TeamTavern.Client.Pages.Post.Matches (matches)
import TeamTavern.Client.Pages.Post.Screen (postScreen)
import TeamTavern.Client.Pages.Post.Type (postType)
import TeamTavern.Client.Pages.Privacy (privacyPolicy)
import TeamTavern.Client.Pages.ResetPassword (resetPassword)
import TeamTavern.Client.Pages.SignIn (signIn)
import TeamTavern.Client.Pages.SignUp (signUp)
import TeamTavern.Client.Script.Meta (setMeta, setMetaRobots)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound)
import TeamTavern.Client.Shared.Slot (Slot__I, Slot___)
import Web.HTML (window)
import Web.HTML.Window (scroll)

-- The history state rides along for the pages that will read it. `popped` is
-- whether the browser went back or forward, rather than a link being followed.
data Query send = ChangeRoute Foreign String Boolean send

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
    , feed :: Slot__I Int
    , postType :: Slot__I Int
    , postGame :: Slot__I Int
    , postScreen :: Slot__I Int
    , matches :: Slot__I Int
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

renderPage :: ∀ action left. Visit -> H.ComponentHTML action ChildSlots (Async left)
renderPage { page: Feed { handle }, visit, restore, cache } = feed visit { handle, restore, cache }
renderPage { page: PostType, visit } = postType visit
renderPage { page: PostGame { type_ }, visit } = postGame visit type_
renderPage { page: PostScreen { handle, type_ }, visit } = postScreen visit { handle, type_ }
renderPage { page: Matches { handle, type_ }, visit } = matches visit { handle, type_ }
renderPage { page } = renderPage' page

renderPage' :: ∀ action left. State -> H.ComponentHTML action ChildSlots (Async left)
renderPage' Empty = HH.div_ []
renderPage' Home = home
renderPage' SignUp = signUp
renderPage' SignIn = signIn
renderPage' ForgotPassword = forgotPassword
renderPage' ResetPassword = resetPassword
renderPage' ConfirmEmail = confirmEmail
renderPage' Privacy = privacyPolicy
renderPage' Design = design
renderPage' page = placeholder $ name page

-- Every navigation is counted, even to the page already open, so the header
-- reads who is signed in on each and a feed opened again starts over. A feed
-- the browser went back or forward to is restored from the cache it keeps.
type Visit =
    { page :: State
    , path :: String
    , visit :: Int
    , restore :: Maybe FeedCache
    , cache :: Ref (Map String FeedCache)
    }

-- The path marks the page drawn for it, in the same render as the page, so a
-- test can tell when the location's page has arrived and not only the location.
render :: ∀ action left. Visit -> H.ComponentHTML action ChildSlots (Async left)
render visit =
    HH.div [ HP.attr (HH.AttrName "data-path") visit.path ]
    [ header { path: visit.path, visit: visit.visit }, renderPage visit ]

router :: ∀ input output left. Foreign -> String -> H.Component Query input output (Async left)
router _ initialPath = Hooks.component \{ queryToken } _ -> Hooks.do
    _ /\ cache <- Hooks.useRef Map.empty
    visit /\ visitId <- Hooks.useState { page: Empty, path: "", visit: 0, restore: Nothing, cache }

    let changeRoute path popped = do
            let page = route path
            case page of
                Home -> setMeta "TeamTavern" description
                NotFound -> do
                    appendRenderReadyNotFound
                    setMeta "Page not found | TeamTavern" description
                -- A feed names its game once it has it.
                Feed _ -> pure unit
                _ -> setMeta (name page <> " | TeamTavern") description
            -- The components page is a tool for building the site, not a page of it.
            setMetaRobots case page of
                Design -> "noindex"
                _ -> "index, follow"
            restore <- case page of
                Feed { handle } | popped -> liftEffect $ Ref.read cache <#> Map.lookup handle
                _ -> pure Nothing
            -- The browser leaves the scroll position alone, so a page it went
            -- back to starts at the top unless it puts its own back.
            when (popped && isNothing restore) $ liftEffect $ window >>= scroll 0 0
            Hooks.modify_ visitId \{ visit: count } -> { page, path, visit: count + 1, restore, cache }

    Hooks.useLifecycleEffect do
        changeRoute initialPath false
        pure Nothing

    Hooks.useQuery queryToken \(ChangeRoute _ path popped send) -> do
        changeRoute path popped
        pure $ Just send

    Hooks.pure $ render visit
