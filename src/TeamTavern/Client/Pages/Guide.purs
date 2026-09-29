module TeamTavern.Client.Pages.Guide (guide) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Pages.Document (document, longDate)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Script.Meta (setGuideData, setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound, appendRenderReadyUnavailable)
import TeamTavern.Client.Script.Scroll (scrollToFragment)
import TeamTavern.Client.Shared.Fetch (expecting, fetchPath)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
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
                        setGuideData
                            { path: "/guides/" <> slug
                            , heading: guide'.heading
                            , description: guide'.description
                            , published: guide'.published
                            , updated: guide'.updated
                            }
                        Hooks.put pageId $ Shown guide'
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
        Shown { heading, updated, html } ->
            document { title: heading, updated: Just $ longDate updated }
            [ HH.div [ HS.class_ "guide", HP.prop (HH.PropName "innerHTML") html, HE.onClick followLink ] [] ]

guide :: ∀ action slots left. Int -> String -> H.ComponentHTML action (guide :: Slot__I Int | slots) (Async left)
guide visit slug = HH.slot_ (Proxy :: _ "guide") visit component slug
