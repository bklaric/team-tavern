module TeamTavern.Client.Pages.Home (home) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyUnavailable)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Game.ViewGames as ViewGames
import Type.Proxy (Proxy(..))

data Games = Loading | Loaded ViewGames.OkContent | Failed

cover :: ∀ slots m. MonadEffect m =>
    ViewGames.OkGameContent -> HH.HTML slots (Hooks.HookM m Unit)
cover { handle, title } =
    HH.a [ HS.class_ "cover", HP.href path, HE.onClick $ navigateWithEvent_ path ]
    [ HH.img [ HP.src $ "/images/games/" <> handle <> ".webp", HP.alt title ] ]
    where
    path = "/games/" <> handle

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
            Loaded games' -> HH.div [ HS.class_ "cover-grid" ] $ games' <#> cover
            Failed -> HH.p_ [ HH.text "There has been an error loading the games." ]
        ]

home :: ∀ action slots left. H.ComponentHTML action (home :: Slot___ | slots) (Async left)
home = HH.slot_ (Proxy :: _ "home") unit component unit
