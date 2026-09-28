module TeamTavern.Client.Pages.Guides (guides) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Pages.Document (document, link, longDate)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyUnavailable)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Guide.ViewGuides (ViewGuides)
import TeamTavern.Routes.Guide.ViewGuides as ViewGuides
import Type.Proxy (Proxy(..))

data Page = Loading | Shown ViewGuides.OkContent | Failed

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    page /\ pageId <- Hooks.useState Loading

    Hooks.useLifecycleEffect do
        void $ Hooks.fork do
            result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ViewGuides)
            case hush result >>= onMatch { ok: Just } (const Nothing) of
                Just guides' -> Hooks.put pageId $ Shown guides'
                Nothing -> do
                    appendRenderReadyUnavailable
                    Hooks.put pageId Failed
        pure Nothing

    Hooks.pure $ document { title: "Guides", updated: Nothing } $
        [ HH.p_ [ HH.text "How to find people to play with: joining a team, making one, and the team formats each game plays." ] ]
        <> case page of
            Loading -> []
            Failed -> [ HH.p_ [ HH.text "There has been an error loading the guides." ] ]
            Shown guides' ->
                [ HH.ul [ HS.class_ "guide-list" ] $ guides' <#> \{ slug, heading, description, updated } ->
                    HH.li_
                    [ HH.h2_ [ link ("/guides/" <> slug) heading ]
                    , HH.p_ [ HH.text description ]
                    , HH.p [ HS.class_ "guide-updated" ] [ HH.text $ "Updated " <> longDate updated ]
                    ]
                ]

guides :: ∀ action slots left. Int -> H.ComponentHTML action (guides :: Slot__I Int | slots) (Async left)
guides visit = HH.slot_ (Proxy :: _ "guides") visit component unit
