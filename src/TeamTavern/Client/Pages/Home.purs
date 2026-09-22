module TeamTavern.Client.Pages.Home (home) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.CoverGrid (coverGrid)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyUnavailable)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Game.ViewGames as ViewGames
import Type.Proxy (Proxy(..))

data Games = Loading | Loaded ViewGames.OkContent | Failed

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    games /\ gamesId <- Hooks.useState Loading

    Hooks.useLifecycleEffect do
        let failed = appendRenderReadyUnavailable *> Hooks.put gamesId Failed
        result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ViewGames)
        case result of
            Left _ -> failed
            Right response -> response # onMatch
                { ok: Hooks.put gamesId <<< Loaded }
                (const failed)
        pure Nothing

    Hooks.pure $
        HH.div [ HS.class_ "placeholder" ]
        [ HH.h1_ [ HH.text "TeamTavern" ]
        , HH.h2_ [ HH.text "Browse a game" ]
        , case games of
            Loading -> HH.div_ []
            Loaded games' -> coverGrid { games: games', mark: const Nothing }
            Failed -> HH.p_ [ HH.text "There has been an error loading the games." ]
        ]

home :: ∀ action slots left. H.ComponentHTML action (home :: Slot___ | slots) (Async left)
home = HH.slot_ (Proxy :: _ "home") unit component unit
