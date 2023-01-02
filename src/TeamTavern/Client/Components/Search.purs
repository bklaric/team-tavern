module TeamTavern.Client.Components.Search where

import Prelude

import Async (attempt, fromEffect)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Data.Variant (match, onMatch)
import Effect.Class.Console (log, logShow)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorClassed)
import TeamTavern.Client.Components.Popover (popover, popoverItem, stopMouseEventPropagation, usePopover)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Shared.Fetch (fetchPathQuery, fetchQuery)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Search (Search)
import TeamTavern.Routes.Search as Search
import Type.Proxy (Proxy(..))

component = Hooks.component \_ _ -> Hooks.do
    -- focused /\ focusedId <- Hooks.useState false
    shown /\ shownId <- usePopover
    searchResult /\ searchResultId <- Hooks.useState (Nothing :: Maybe Search.OkContent)
    let onSearchInput query =
            if query == ""
            then Hooks.put searchResultId Nothing
            else do
                result <- lift $ attempt $ fetchQuery (Proxy :: _ Search) { query }
                foldMap (onMatch { ok: Hooks.put searchResultId <<< Just } (const $ pure unit)) result
    Hooks.pure $
        popover
        (shown)
        [ HH.input
            [ HS.class_ "search-input"
            , HP.type_ HP.InputText
            , HP.placeholder "Search players and teams"
            , HE.onValueInput onSearchInput
            , HE.onClick stopMouseEventPropagation
            , HE.onFocus $ const $ Hooks.put shownId true
            ]
        ]
        [ popoverItem (const $ lift $ navigate_ "/players/Houndolon") [ HH.text "Houndolon"]
        , navigationAnchorClassed (Proxy :: _ "houndolon") { class_: "popover-item", path: "/players/Houndolon", content: HH.text "Houndolon" }
        ]

search = HH.slot_ (Proxy :: _ "search") unit component unit
