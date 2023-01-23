module TeamTavern.Client.Script.Rendertron where

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

appendRendetronStatus :: forall m. Bind m => MonadEffect m => String -> m Unit
appendRendetronStatus status = do
    statusMeta <-
        window
        >>= document
        <#> HTMLDocument.toNode
        <#> (Document.fromNode >>> unsafePartial fromJust)
        >>= Document.createElement "meta"
        # liftEffect
    statusMeta # setAttribute "name" "render:status_code" # liftEffect
    statusMeta # setAttribute "content" status # liftEffect
    window >>= document >>= head
        <#> (unsafePartial fromJust >>> HTMLElement.toNode)
        >>= appendChild (statusMeta # Element.toNode) # liftEffect

appendRendetronNotFound :: forall m. Bind m => MonadEffect m => m Unit
appendRendetronNotFound = appendRendetronStatus "404"
