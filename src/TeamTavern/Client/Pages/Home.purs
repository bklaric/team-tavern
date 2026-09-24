module TeamTavern.Client.Pages.Home (home) where

import Prelude

import Async (Async)
import Async as Async
import Control.Parallel (parallel, sequential)
import Data.Array (filter, length, notElem)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (Size(..), Weight(..), buttonLink)
import TeamTavern.Client.Components.Card (Viewer, ownCard)
import TeamTavern.Client.Components.CoverGrid (coverGrid, feedPath)
import TeamTavern.Client.Components.OwnPostStatus (ownPostStatus, renewDue)
import TeamTavern.Client.Components.Toast (toasts, useToast)
import TeamTavern.Client.Components.TypeCards (typeCards)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Feed.Description (storeDescription)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.QueryParams (getQueryParam, removeQueryParam)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyUnavailable)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPath, fetchSimple)
import TeamTavern.Client.Shared.Renew (renew, renewFailed) as Renew
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Game.ViewGames as ViewGames
import TeamTavern.Routes.Post.ViewOwnPosts (OwnPost, ViewOwnPosts)
import Type.Proxy (Proxy(..))

-- A game the player has posts in, with what its cards need to read them.
type OwnGame = { game :: ViewGame.OkContent, posts :: Array OwnPost }

data Page
    = Loading
    | Start ViewGames.OkContent
    | Posts { games :: ViewGames.OkContent, own :: Array OwnGame, viewer :: Viewer }
    | Failed String

lead :: String
lead = "Find players, groups and communities for the games you play. Post once, and we'll tell you when someone new fits."

-- Everything the page shows, or why it can't. Signed out, the player's posts
-- are none.
load :: ∀ left. Async left Page
load = do
    Tuple games own <- sequential $ Tuple
        <$> parallel (Async.attempt $ fetchSimple (Proxy :: _ ViewGames))
        <*> parallel (Async.attempt $ fetchSimple (Proxy :: _ ViewOwnPosts))
    let own' = hush own >>= onMatch { ok: Just, notAuthorized: const $ Just [] } (const Nothing)
    case hush games >>= onMatch { ok: Just } (const Nothing), own' of
        Nothing, _ -> pure $ Failed "There has been an error loading the games."
        _, Nothing -> pure $ Failed "There has been an error loading your posts."
        Just games', Just [] -> pure $ Start games'
        Just games', Just own'' -> do
            loaded <- sequential $ own'' # traverse \{ handle, posts } ->
                parallel $ Async.attempt (fetchPath (Proxy :: _ ViewGame) { handle }) <#> \game ->
                    hush game >>= onMatch { ok: Just } (const Nothing) <#> { game: _, posts }
            now' <- liftEffect now
            timezone <- getClientTimezone
            pure case sequence loaded of
                Nothing -> Failed "There has been an error loading your posts."
                Just own''' -> Posts { games: games', own: own''', viewer: { now: now', timezone } }

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    page /\ pageId <- Hooks.useState Loading
    { toast, showToast, dismissToast } <- useToast

    let show' page' = do
            case page' of
                Failed _ -> appendRenderReadyUnavailable
                Posts _ -> setMeta "Your posts | TeamTavern" lead
                _ -> pure unit
            Hooks.put pageId page'

    Hooks.useLifecycleEffect do
        -- Deleting an account lands here, signed out.
        deleted <- getQueryParam "account" <#> eq (Just "deleted")
        when deleted do
            removeQueryParam "account"
            showToast { text: "Your account is deleted.", action: Nothing }
        void $ Hooks.fork $ H.lift load >>= show'
        pure Nothing

    let renew :: String -> OwnPost -> HookM (Async left) Unit
        renew handle { post } = void $ Hooks.fork do
            renewed <- H.lift $ Renew.renew handle post
            case renewed of
                Nothing -> showToast { text: Renew.renewFailed, action: Nothing }
                Just text -> do
                    H.lift load >>= show'
                    showToast { text, action: Nothing }

        gamesSection title games =
            HH.section [ HS.class_ "home-games", HPA.labelledBy "games-title" ]
            [ HH.h2 [ HP.id "games-title" ] [ HH.text title ]
            , coverGrid { games, href: feedPath, mark: const Nothing }
            ]

        ownPost viewer game own@{ post, owner, description } = let
            feed = feedPath game.handle
            in
            Tuple (show post.id) $ ownCard
                { game
                , viewer
                , post
                , status: ownPostStatus
                    { now: viewer.now
                    , expires: owner.expires
                    , conversations: owner.conversations
                    , unread: owner.unread
                    , conversation: owner.conversation
                    , reveals: owner.reveals
                    , onOpen: navigateWithEvent_
                    }
                , renewDue: renewDue viewer.now owner.expires
                -- The feed opens with the description the post makes.
                , onFits: \event -> do
                    liftEffect $ storeDescription game.handle post.type description
                    navigateWithEvent_ feed event
                , onRenew: renew game.handle own
                }

        -- The cover stands in for the game's name, which is there for screen
        -- readers, and for a phone, where the cover is too small to read.
        gameSection viewer { game, posts } = let
            feed = feedPath game.handle
            headingId = "game-" <> game.handle
            in
            Tuple game.handle $ HH.section [ HS.class_ "home-game", HPA.labelledBy headingId ]
            [ HH.h2 [ HS.class_ "home-game-heading", HP.id headingId ]
                [ HH.a [ HS.class_ "home-game-cover", HP.href feed, HE.onClick $ navigateWithEvent_ feed ]
                    [ HH.img [ HP.src $ "/images/games/" <> game.handle <> ".webp", HP.alt "" ]
                    , HH.span [ HS.class_ "home-game-name" ] [ HH.text game.title ]
                    ]
                ]
            , HK.div [ HS.class_ "home-game-posts" ] $
                (ownPost viewer game <$> posts)
                <> if length posts < 3
                    then
                        [ Tuple "new" $ buttonLink Text Small ("/post?game=" <> game.handle)
                            [ Icons.plus, HH.text $ "New " <> game.title <> " post" ]
                        ]
                    else []
            ]

    Hooks.pure case page of
        Loading -> HH.div [ HS.class_ "home" ] []
        Failed message -> HH.div [ HS.class_ "home" ] [ HH.p_ [ HH.text message ] ]
        Start games ->
            HH.div [ HS.class_ "home" ]
            [ HH.section [ HS.class_ "home-start", HPA.labelledBy "start-title" ]
                [ HH.h1 [ HP.id "start-title" ] [ HH.text "What are you posting?" ]
                , HH.p [ HS.class_ "home-lead" ] [ HH.text lead ]
                , typeCards { href: \type_ -> "/post/" <> type_, note: const Nothing }
                ]
            , gamesSection "Or browse a game" games
            , toasts toast dismissToast
            ]
        -- Games keep the catalogue's order, so renewing a post doesn't move it.
        Posts { games, own, viewer } -> let
            posted = own <#> _.game.handle
            others = games # filter \{ handle } -> notElem handle posted
            in
            HK.div [ HS.class_ "home" ] $
            [ Tuple "title" $ HH.h1_ [ HH.text "Your posts" ] ]
            <> (gameSection viewer <$> own)
            <> (if length others == 0 then [] else [ Tuple "others" $ gamesSection "Other games" others ])
            <> [ Tuple "toasts" $ toasts toast dismissToast ]

home :: ∀ action slots left. Int -> H.ComponentHTML action (home :: Slot__I Int | slots) (Async left)
home visit = HH.slot_ (Proxy :: _ "home") visit component unit
