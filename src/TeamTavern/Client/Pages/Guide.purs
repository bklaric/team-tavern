module TeamTavern.Client.Pages.Guide (guide) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, indexOf', length, splitAt)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Ads as Ads
import TeamTavern.Client.Pages.Document (document, longDate)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Script.LocalTimes (showLocalTimes)
import TeamTavern.Client.Script.Meta (setCoverImage, setGuideData, setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound, appendRenderReadyUnavailable)
import TeamTavern.Client.Script.Scroll (scrollToFragment)
import TeamTavern.Client.Shared.Fetch (expecting, fetchPath)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.Cover (coverPath)
import TeamTavern.Routes.Guide.ViewGuide (ViewGuide)
import TeamTavern.Routes.Guide.ViewGuide as ViewGuide
import Type.Proxy (Proxy(..))
import Web.DOM.Element (closest, fromEventTarget, getAttribute)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (target)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Page = Loading | Shown ViewGuide.OkContent | Missing | Failed

-- The guide's text comes as HTML, so its links to the site's own pages are
-- followed here, as every other in-app link follows itself.
followLink :: ∀ m. MonadEffect m => MouseEvent -> m Unit
followLink event = do
    path <- liftEffect case target (toEvent event) >>= fromEventTarget of
        Nothing -> pure Nothing
        Just element -> closest (QuerySelector "a[href^='/']:not([href^='//'])") element
            >>= traverse (getAttribute "href") <#> join
    for_ path \path' -> navigateWithEvent_ path' event

-- The guide's text, split where its units go: after the summary it opens
-- with, and two sections further on. Its elements are the document column's
-- own, so the parts and the units between them read as one column.
parts :: String -> Array String
parts html = case indexOf (Pattern summaryEnd) html of
    Nothing -> [ html ]
    Just at -> let
        { before: opening, after: rest } = splitAt (at + length summaryEnd) html
        in
        case thirdSection rest of
            Nothing -> [ opening, rest ]
            Just at' -> let { before: middle, after: closing } = splitAt at' rest in [ opening, middle, closing ]
    where
    summaryEnd = "</blockquote>"
    heading = Pattern "<h2"
    thirdSection rest = indexOf heading rest
        >>= \first -> indexOf' heading (first + 1) rest
        >>= \second -> indexOf' heading (second + 1) rest

component :: ∀ query output left. H.Component query String output (Async left)
component = Hooks.component \_ slug -> Hooks.do
    page /\ pageId <- Hooks.useState Loading

    Hooks.useLifecycleEffect do
        void $ Hooks.fork do
            result <- H.lift $ Async.attempt $ fetchPath (expecting [ "notFound" ] (Proxy :: _ ViewGuide)) { slug }
            let failed = do
                    appendRenderReadyUnavailable
                    Hooks.put pageId Failed
            case result of
                Right response -> response # onMatch
                    { ok: \guide' -> do
                        setMeta guide'.title guide'.description
                        -- A guide to one game shares that game's cover, and
                        -- the rest keep the logo the router has set.
                        image <- case guide'.games of
                            [ game ] -> do
                                setCoverImage game
                                pure $ coverPath game.handle
                            _ -> pure "/logo-512.png"
                        setGuideData
                            { path: "/guides/" <> slug
                            , heading: guide'.heading
                            , description: guide'.description
                            , image
                            , published: guide'.published
                            , updated: guide'.updated
                            }
                        Hooks.put pageId $ Shown guide'
                        showLocalTimes
                        scrollToFragment
                    , notFound: const do
                        appendRenderReadyNotFound
                        setMeta "Page not found | TeamTavern" ""
                        Hooks.put pageId Missing
                    }
                    (const failed)
                Left _ -> failed
        pure Nothing

    Hooks.pure case page of
        Loading -> HH.div_ []
        Missing -> placeholder "Page could not be found."
        Failed -> placeholder "There has been an error loading the guide."
        Shown { heading, updated, html } -> let
            text part = HH.div [ HS.class_ "guide", HP.prop (HH.PropName "innerHTML") part, HE.onClick followLink ] []
            in
            Ads.around $ document { title: heading, updated: Just $ longDate updated }
            case parts html of
                [ opening, middle, closing ] ->
                    [ text opening, Ads.spot Ads.First, text middle, Ads.spot Ads.Second, text closing ]
                [ opening, rest ] -> [ text opening, Ads.spot Ads.First, text rest ]
                _ -> [ text html ]

guide :: ∀ action slots left. Int -> String -> H.ComponentHTML action (guide :: Slot__I Int | slots) (Async left)
guide visit slug = HH.slot_ (Proxy :: _ "guide") visit component slug
