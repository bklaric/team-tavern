module TeamTavern.Client.Pages.Post.Matches (matches) where

import Prelude

import Async (Async)
import Async as Async
import Control.Alt ((<|>))
import Data.Array (elem, filter, find, init, last, length, null, snoc, take)
import Data.Either (hush)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Card (Place(..), Viewer, card, tierOf)
import TeamTavern.Client.Components.ContactPanel (contactPanel, markMessaged, useContactPanel)
import TeamTavern.Client.Components.Flow (flowLead)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Feed.Description (storeDescription)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Script.Expand (toggleCard)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigateWithEvent_)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPath, fetchPathBody)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Feed.ViewFeed (ViewFeed)
import TeamTavern.Routes.Feed.ViewOwnDescriptions (ViewOwnDescriptions)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Description (Description)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

type Input = { handle :: String, type_ :: String }

-- | `fits` are the active posts that fit the new one, all of them; `posts`
-- | the first batch, which holds the closest when nothing fits.
type Loaded =
    { game :: ViewGame.OkContent
    , description :: Description
    , fits :: Array CardRow
    , fitCount :: Int
    , posts :: Array CardRow
    }

data Screen = Loading | Ready Loaded | Failed

type State =
    { screen :: Screen
    , updated :: Boolean
    , viewer :: Maybe Viewer
    , expanded :: Array Int
    }

fits :: CardRow -> Boolean
fits post = not post.expired && not post.own && tierOf post == 0

plural :: String -> String
plural "community" = "communities"
plural type_ = type_ <> "s"

counted :: Int -> String -> String
counted count type_ = show count <> " " <> if count == 1 then type_ else plural type_

-- A player post is fitted by groups, communities and players, each counted; a
-- group or a community by players (brief 6, step 5).
fitsSentence :: String -> Int -> Array CardRow -> String
fitsSentence type_ count fitting
    | type_ /= "player" =
        counted count "player" <> " " <> (if count == 1 then "fits" else "fit") <> " your " <> type_ <> " right now"
    | otherwise = let
        words =
            [ "group", "community", "player" ]
            <#> (\kind -> kind /\ length (filter (_.type >>> eq kind) fitting))
            # filter (\(_ /\ n) -> n > 0)
            <#> \(kind /\ n) -> counted n kind
        list = case init words, last words of
            Just rest, Just final | not null rest -> joinWith ", " rest <> " and " <> final
            _, _ -> joinWith "" words
        in
        list <> " " <> (if count == 1 then "fits" else "fit") <> " you right now"

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { handle, type_ } -> Hooks.do
    state /\ stateId <- Hooks.useState
        ({ screen: Loading, updated: false, viewer: Nothing, expanded: [] } :: State)
    { panel, openPanel } <- useContactPanel \id time ->
        Hooks.modify_ stateId \state' -> case state'.screen of
            Ready loaded -> state'
                { screen = Ready loaded
                    { fits = markMessaged id time <$> loaded.fits
                    , posts = markMessaged id time <$> loaded.posts
                    }
                }
            _ -> state'

    let postPath = "/games/" <> handle <> "/post/" <> type_
        set = Hooks.modify_ stateId

    Hooks.useLifecycleEffect do
        now' <- liftEffect now
        timezone <- getClientTimezone
        updated <- getQueryParam "updated" <#> isJust
        set _ { viewer = Just { now: now', timezone }, updated = updated }

        void $ Hooks.fork do
            game <- H.lift $ Async.attempt (fetchPath (Proxy :: _ ViewGame) { handle })
                <#> (hush >=> onMatch { ok: Just } (const Nothing))
            own <- H.lift $ Async.attempt (fetchPath (Proxy :: _ ViewOwnDescriptions) { handle })
                <#> (hush >=> onMatch { ok: Just } (const Nothing))
            case game, own <#> find (_.type >>> eq type_) of
                Just game', Just (Just { description }) -> do
                    let description' = description { timezone = description.timezone <|> Just timezone }
                        batch cursor = H.lift $ Async.attempt
                            (fetchPathBody (Proxy :: _ ViewFeed) { handle } { description: description', showing: [], cursor })
                            <#> (hush >=> onMatch { ok: Just } (const Nothing))
                        -- The fits come first, so the batches are followed
                        -- only while they still hold fits.
                        follow loaded cursor = do
                            next <- batch cursor
                            case next of
                                Nothing -> pure Nothing
                                Just result -> let
                                    loaded' = loaded <> result.posts
                                    in
                                    if result.more && maybe false fits (last result.posts)
                                    then follow loaded' result.cursor
                                    else pure $ Just { posts: loaded', count: result.tiers.fits }
                    result <- follow [] Nothing
                    case result of
                        Just { posts, count } -> set _
                            { screen = Ready
                                { game: game'
                                , description: description'
                                , fits: filter fits posts
                                , fitCount: count
                                , posts
                                }
                            }
                        Nothing -> set _ { screen = Failed }
                Just _, Just Nothing -> navigateReplace_ postPath
                _, _ -> set _ { screen = Failed }
        pure Nothing

    let toggle id (event :: MouseEvent) = toggleCard event $ set \state' -> state'
            { expanded = if elem id state'.expanded then filter (notEq id) state'.expanded else snoc state'.expanded id }

        cardOf game viewer post = card
            { game
            , viewer
            , post
            , marked: true
            , expanded: elem post.id state.expanded
            , place: Listed
            , onToggle: toggle post.id
            -- Only a player signed in has published a post.
            , onContact: openPanel { signedIn: true, game } post
            , onEdit: pure unit
            , onRenew: pure unit
            }

        -- See all opens the feed with the new post as its description
        -- (brief 11.2).
        seeAll description =
            HH.a
            [ HS.class_ "button button-primary"
            , HP.href feedPath
            , HE.onClick \event -> do
                liftEffect $ storeDescription handle type_ description
                navigateWithEvent_ feedPath event
            ]
            [ HH.text "See all" ]
            where
            feedPath = "/games/" <> handle

    Hooks.pure case state.screen, state.viewer of
        Failed, _ -> placeholder "There has been an error loading your matches."
        Ready loaded, Just viewer -> let
            shown = take 3 if null loaded.fits then filter (\post -> not post.expired && not post.own) loaded.posts else loaded.fits
            noun = if type_ == "player" then "post" else type_
            in
            HH.div [ HS.class_ "flow" ] $
            [ HH.div [ HS.class_ "live-heading" ]
                [ Icons.partyPopper
                , HH.h1_ [ HH.text if state.updated then "Your post is updated" else "Your post is live" ]
                ]
            , HH.h2_ [ HH.text
                if null loaded.fits then "Nobody fits your " <> noun <> " yet"
                else fitsSentence type_ loaded.fitCount loaded.fits
                ]
            ]
            <> (if null loaded.fits
                then [ flowLead $ (if null shown then "" else "These come closest. ") <> "We'll email you when someone fits." ]
                else [])
            <> (if null shown then []
                else [ HK.div [ HS.class_ "feed-stack" ] $ shown <#> \post -> show post.id /\ cardOf loaded.game viewer post ])
            <> [ HH.div [ HS.class_ "flow-actions" ] [ seeAll loaded.description ] ]
            <> maybe [] (\panel' -> [ contactPanel viewer.now panel' ]) panel
        _, _ -> HH.div [ HS.class_ "flow" ] []

matches :: ∀ action slots left.
    Int -> Input -> H.ComponentHTML action (matches :: Slot__I Int | slots) (Async left)
matches visit input = HH.slot_ (Proxy :: _ "matches") visit component input
