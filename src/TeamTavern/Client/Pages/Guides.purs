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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Ads as Ads
import TeamTavern.Client.Pages.Document (document, link, longDate)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyUnavailable)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.Cover (smallCoverPath)
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

    Hooks.pure $ Ads.around $ document { title: "Guides", updated: Nothing } $
        [ HH.p_ [ HH.text "How to find people to play with: joining a team, making one, and the team formats each game plays." ] ]
        <> case page of
            Loading -> []
            Failed -> [ HH.p_ [ HH.text "There has been an error loading the guides." ] ]
            Shown guides' ->
                [ HH.ul [ HS.class_ "guide-list" ] $ guides' <#> \{ slug, heading, description, updated, games } ->
                    let path = "/guides/" <> slug in
                    HH.li_
                    -- The cover leads to the guide too, for a pointer. The
                    -- title is the guide's one link to a keyboard or a screen
                    -- reader.
                    [ HH.a
                        [ HS.class_ "guide-list-cover", HP.href path, HP.tabIndex (-1), HPA.hidden "true"
                        , HE.onClick $ navigateWithEvent_ path
                        ]
                        [ case games of
                            [ { handle } ] -> HH.img
                                [ HP.src $ smallCoverPath handle, HP.alt "", HP.width 400, HP.height 600 ]
                            _ -> HH.span [ HS.class_ "guide-list-logo" ]
                                [ HH.img [ HP.src "/logo-mark.svg", HP.alt "" ] ]
                        ]
                    , HH.h2_ [ link path heading ]
                    , HH.p [ HS.class_ "guide-updated" ] [ HH.text $ "Updated " <> longDate updated ]
                    , HH.p_ [ HH.text description ]
                    ]
                ]

guides :: ∀ action slots left. Int -> H.ComponentHTML action (guides :: Slot__I Int | slots) (Async left)
guides visit = HH.slot_ (Proxy :: _ "guides") visit component unit
