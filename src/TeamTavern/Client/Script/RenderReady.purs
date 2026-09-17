module TeamTavern.Client.Script.RenderReady where

import Prelude

import Data.Maybe (fromJust)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document as Document
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (head)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

-- The prerenderer answers a bot with the status this tag names instead of 200.
appendRenderReadyStatus :: forall m. Bind m => MonadEffect m => String -> m Unit
appendRenderReadyStatus status = do
    statusMeta <-
        window
        >>= document
        <#> HTMLDocument.toNode
        <#> (Document.fromNode >>> unsafePartial fromJust)
        >>= Document.createElement "meta"
        # liftEffect
    statusMeta # setAttribute "name" "renderready-status-code" # liftEffect
    statusMeta # setAttribute "content" status # liftEffect
    window >>= document >>= head
        <#> (unsafePartial fromJust >>> HTMLElement.toNode)
        >>= appendChild (statusMeta # Element.toNode) # liftEffect

appendRenderReadyNotFound :: forall m. Bind m => MonadEffect m => m Unit
appendRenderReadyNotFound = appendRenderReadyStatus "404"

-- A page that failed to load is answered 503, so a crawler keeps what it has and comes back
-- rather than indexing the error message.
appendRenderReadyUnavailable :: forall m. Bind m => MonadEffect m => m Unit
appendRenderReadyUnavailable = appendRenderReadyStatus "503"
