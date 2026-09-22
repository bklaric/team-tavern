module TeamTavern.Client.Pages.Post.Game (postGame) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (elem, find)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (Size(..), Weight(..), buttonLink)
import TeamTavern.Client.Components.CoverGrid (coverGrid)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Game.ViewGames as ViewGames
import TeamTavern.Routes.Player.ViewMe (ViewMe)
import TeamTavern.Routes.Player.ViewMe as ViewMe
import Type.Proxy (Proxy(..))

type State =
    { games :: ViewGames.OkContent
    , me :: Maybe ViewMe.OkContent
    }

typeTitle :: String -> Maybe String
typeTitle = case _ of
    "player" -> Just "Player"
    "group" -> Just "Group"
    "community" -> Just "Community"
    _ -> Nothing

-- The second step of posting (brief 6, step 2): the cover grid, marking the
-- games the player already has a post of this type in.
component :: ∀ query output left. H.Component query String output (Async left)
component = Hooks.component \_ type_ -> Hooks.do
    state /\ stateId <- Hooks.useState ({ games: [], me: Nothing } :: State)

    Hooks.useLifecycleEffect do
        signedIn <- hasPlayerIdCookie
        case typeTitle type_ of
            Nothing -> appendRenderReadyNotFound
            Just _ -> void $ Hooks.fork do
                games <- H.lift $ Async.attempt (fetchSimple (Proxy :: _ ViewGames))
                    <#> (hush >=> onMatch { ok: Just } (const Nothing))
                me <- if not signedIn then pure Nothing else
                    H.lift $ Async.attempt (fetchSimple (Proxy :: _ ViewMe))
                    <#> (hush >=> onMatch { ok: Just } (const Nothing))
                Hooks.modify_ stateId _ { games = games # maybe [] identity, me = me }
        pure Nothing

    let mine handle = state.me # maybe false
            (_.games >>> find (_.handle >>> eq handle) >>> maybe false (_.types >>> elem type_))

    Hooks.pure case typeTitle type_ of
        Nothing -> placeholder "Page could not be found."
        Just title ->
            HH.div [ HS.class_ "flow" ]
            [ HH.div [ HS.class_ "step-context" ]
                [ HH.strong_ [ HH.text $ title <> " post" ]
                , buttonLink Text Small "/post" [ HH.text "Change type" ]
                ]
            , HH.h1_ [ HH.text "Which game?" ]
            , coverGrid
                { games: state.games
                , href: \handle -> "/games/" <> handle <> "/post/" <> type_
                , mark: \handle -> if mine handle then Just "Your post" else Nothing
                }
            ]

postGame :: ∀ action slots left.
    Int -> String -> H.ComponentHTML action (postGame :: Slot__I Int | slots) (Async left)
postGame visit type_ = HH.slot_ (Proxy :: _ "postGame") visit component type_
