module TeamTavern.Client.Router (Query(..), router) where

import Prelude

import Async (Async)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Int as Int
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
import TeamTavern.Client.Pages.Account (account)
import TeamTavern.Client.Pages.ConfirmEmail (confirmEmail)
import TeamTavern.Client.Pages.Design (design)
import TeamTavern.Client.Pages.Feed (FeedCache, feed)
import TeamTavern.Client.Pages.ForgotPassword (forgotPassword)
import TeamTavern.Client.Pages.Home (home)
import TeamTavern.Client.Pages.Messages (messages)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Pages.Post.Game (postGame)
import TeamTavern.Client.Pages.Post.Matches (matches)
import TeamTavern.Client.Pages.Post.Screen (postScreen)
import TeamTavern.Client.Pages.Post.Type (postType)
import TeamTavern.Client.Pages.PostPage (postPage)
import TeamTavern.Client.Pages.Privacy (privacyPolicy)
import TeamTavern.Client.Pages.Renew (renew)
import TeamTavern.Client.Pages.ResetPassword (resetPassword)
import TeamTavern.Client.Pages.SignIn (signIn)
import TeamTavern.Client.Pages.SignUp (signUp)
import TeamTavern.Client.Script.Focus (focusStill)
import TeamTavern.Client.Script.Meta (setMeta, setMetaRobots)
import TeamTavern.Client.Script.Previous (previousOf, stampPrevious)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound)
import TeamTavern.Client.Shared.Slot (Slot__I, Slot___)
import Web.HTML (window)
import Web.HTML.Window (scroll)

-- The history state carries the path of the page the entry was opened from,
-- where the site stamped it. `popped` is whether the browser went back or
-- forward, rather than a link being followed.
data Query send = ChangeRoute Foreign String Boolean send

data State
    = Empty
    | Home
    | Feed { handle :: String }
    | Post { handle :: String, id :: Int }
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
    | Conversation { conversation :: Int }
    | Account
    | Privacy
    | Design
    | NotFound

type ChildSlots =
    ( header :: Slot___
    , home :: Slot__I Int
    , account :: Slot__I Int
    , signUp :: Slot___
    , signIn :: Slot___
    , forgotPassword :: Slot___
    , resetPassword :: Slot___
    , confirmEmail :: Slot___
    , renew :: Slot___
    , design :: Slot___
    , feed :: Slot__I Int
    , postPage :: Slot__I Int
    , postType :: Slot__I Int
    , postGame :: Slot__I Int
    , postScreen :: Slot__I Int
    , matches :: Slot__I Int
    , messages :: Slot__I Unit
    )

route :: String -> State
route path =
    case split (Pattern "/") path of
    ["", ""] -> Home
    ["", "games", handle] -> Feed { handle }
    ["", "games", handle, "posts", id] | Just id' <- Int.fromString id -> Post { handle, id: id' }
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
    ["", "messages", conversation] | Just conversation' <- Int.fromString conversation -> Conversation { conversation: conversation' }
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
description = "Find players, groups and communities for the games you play. Post once, and we'll tell you when someone new fits."

renderPage :: ∀ action left. Visit -> H.ComponentHTML action ChildSlots (Async left)
renderPage { page: Feed { handle }, visit, restore, cache } = feed visit { handle, restore, cache }
renderPage { page: Post { handle, id }, visit, previous } =
    postPage visit { handle, id, feedBehind: previous == Just ("/games/" <> handle) }
renderPage { page: Home, visit } = home visit
renderPage { page: Account, visit } = account visit
renderPage { page: PostType, visit } = postType visit
renderPage { page: PostGame { type_ }, visit } = postGame visit type_
renderPage { page: PostScreen { handle, type_ }, visit } = postScreen visit { handle, type_ }
renderPage { page: Matches { handle, type_ }, visit } = matches visit { handle, type_ }
renderPage { page: Messages, visit } = messages { conversation: Nothing, visit }
renderPage { page: Conversation { conversation }, visit } = messages { conversation: Just conversation, visit }
renderPage { page } = renderPage' page

renderPage' :: ∀ action left. State -> H.ComponentHTML action ChildSlots (Async left)
renderPage' Empty = HH.div_ []
renderPage' SignUp = signUp
renderPage' SignIn = signIn
renderPage' ForgotPassword = forgotPassword
renderPage' ResetPassword = resetPassword
renderPage' ConfirmEmail = confirmEmail
renderPage' Renew = renew
renderPage' Privacy = privacyPolicy
renderPage' Design = design
renderPage' page = placeholder $ name page

-- Every navigation is counted, even to the page already open, so the header
-- reads who is signed in on each and a feed opened again starts over. A feed
-- the browser went back or forward to is restored from the cache it keeps.
-- `previous` is the path the browser's Back returns to, where the site knows it.
type Visit =
    { page :: State
    , path :: String
    , visit :: Int
    , restore :: Maybe FeedCache
    , cache :: Ref (Map String FeedCache)
    , previous :: Maybe String
    }

-- The path marks the page drawn for it, in the same render as the page, so a
-- test can tell when the location's page has arrived and not only the location.
-- The page is the document's main content, which the header's skip link and a
-- link to another page focus.
render :: ∀ action left. Visit -> H.ComponentHTML action ChildSlots (Async left)
render visit =
    HH.div [ HP.attr (HH.AttrName "data-path") visit.path ]
    [ header { path: visit.path, visit: visit.visit }
    , HH.main [ HP.id "content", HP.tabIndex (-1) ] [ renderPage visit ]
    ]

router :: ∀ input output left. Foreign -> String -> H.Component Query input output (Async left)
router initialState initialPath = Hooks.component \{ queryToken } _ -> Hooks.do
    _ /\ cache <- Hooks.useRef Map.empty
    visit /\ visitId <- Hooks.useState
        { page: Empty, path: "", visit: 0, restore: Nothing, cache, previous: Nothing }

    let changeRoute state path popped = do
            let page = route path
            case page of
                Home -> setMeta "TeamTavern: LFG for players, groups and communities" description
                NotFound -> do
                    appendRenderReadyNotFound
                    setMeta "Page not found | TeamTavern" description
                -- A feed and a post name themselves once they have their game, and
                -- a conversation once it has its post.
                Feed _ -> pure unit
                Post _ -> pure unit
                Conversation _ -> pure unit
                _ -> setMeta (name page <> " | TeamTavern") description
            -- A link stamps the entry it opens with the page it left, which the
            -- entry keeps through a reload and a trip back and forth. A link to
            -- the open page replaces its entry, which keeps the stamp it had.
            left <- Hooks.get visitId
            previous <-
                if popped || left.path == "" then pure $ previousOf state
                else if left.path == path then do
                    for_ left.previous $ liftEffect <<< stampPrevious
                    pure left.previous
                else do
                    liftEffect $ stampPrevious left.path
                    pure $ Just left.path
            -- Search gets the pages anyone comes to read. The rest are forms, a
            -- player's own pages and tools, and a page stays out of search until
            -- it is named here. A post takes itself out once it has expired.
            setMetaRobots case page of
                Home -> "index, follow"
                Feed _ -> "index, follow"
                Post _ -> "index, follow"
                Privacy -> "index, follow"
                _ -> "noindex"
            restore <- case page of
                Feed { handle } | popped -> liftEffect $ Ref.read cache <#> Map.lookup handle
                _ -> pure Nothing
            -- The browser leaves the scroll position alone, so a page it went
            -- back to starts at the top unless it puts its own back.
            when (popped && isNothing restore) $ liftEffect $ window >>= scroll 0 0
            Hooks.modify_ visitId \{ visit: count } -> { page, path, visit: count + 1, restore, cache, previous }
            -- A screen reader goes on reading the page a link left unless the
            -- focus moves to the new one. Back and Forward, and the first page,
            -- leave the focus to the browser.
            unless (popped || left.path == "") $ liftEffect $ focusStill "#content"

    Hooks.useLifecycleEffect do
        changeRoute initialState initialPath false
        pure Nothing

    Hooks.useQuery queryToken \(ChangeRoute state path popped send) -> do
        changeRoute state path popped
        pure $ Just send

    Hooks.pure $ render visit
