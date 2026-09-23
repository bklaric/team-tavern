module TeamTavern.Client.Pages.Post.Type (postType) where

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
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.TypeCards (typeCards)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Client.Shared.Me (fetchMe)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Game.ViewGames as ViewGames
import TeamTavern.Routes.Player.ViewMe as ViewMe
import Type.Proxy (Proxy(..))

type State =
    { handle :: Maybe String
    , games :: ViewGames.OkContent
    , me :: Maybe ViewMe.OkContent
    }

-- The first step of posting (brief 6, step 1). Opened from a game's pages it
-- knows the game, `?game=`, and goes straight to the post screen.
component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState ({ handle: Nothing, games: [], me: Nothing } :: State)

    Hooks.useLifecycleEffect do
        handle <- getQueryParam "game"
        Hooks.modify_ stateId _ { handle = handle }
        void $ Hooks.fork do
            games <- H.lift $ Async.attempt (fetchSimple (Proxy :: _ ViewGames))
                <#> (hush >=> onMatch { ok: Just } (const Nothing))
            me <- H.lift fetchMe
            Hooks.modify_ stateId _ { games = games # maybe [] identity, me = me }
        pure Nothing

    let game = state.handle >>= \handle -> state.games # find (_.handle >>> eq handle)
        mine handle type_ = state.me # maybe false
            (_.games >>> find (_.handle >>> eq handle) >>> maybe false (_.types >>> elem type_))

    Hooks.pure $
        HH.div [ HS.class_ "flow" ] $
        ( game # maybe [] \{ handle, title } ->
            [ HH.div [ HS.class_ "step-context" ]
                [ HH.img [ HP.src $ "/images/games/" <> handle <> ".webp", HP.alt "" ]
                , HH.strong_ [ HH.text title ]
                ]
            ]
        )
        <>
        [ HH.h1_ [ HH.text "What are you posting?" ]
        , typeCards
            { href: \type_ -> case state.handle of
                Just handle -> "/games/" <> handle <> "/post/" <> type_
                Nothing -> "/post/" <> type_
            , note: \type_ -> game >>= \{ handle, title } ->
                if mine handle type_ then Just $ "You have one for " <> title else Nothing
            }
        , HH.p [ HS.class_ "muted" ] [ HH.text "Just looking? Open a game from Games to browse its feed." ]
        ]

postType :: ∀ action slots left.
    Int -> H.ComponentHTML action (postType :: Slot__I Int | slots) (Async left)
postType visit = HH.slot_ (Proxy :: _ "postType") visit component unit
