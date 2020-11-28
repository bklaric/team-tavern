module TeamTavern.Client.Script.Meta where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle, toNonElementParentNode)
import Web.HTML.HTMLLinkElement (setHref)
import Web.HTML.HTMLLinkElement as LinkElement
import Web.HTML.HTMLMetaElement (setContent)
import Web.HTML.HTMLMetaElement as MetaElement
import Web.HTML.Location (href)
import Web.HTML.Window (document, location)

setMetaContent :: String -> String -> Effect Unit
setMetaContent content id = do
    metaElement <- window >>= document <#> toNonElementParentNode >>= getElementById id
    case MetaElement.fromElement =<< metaElement of
        Just metaElement' -> setContent content metaElement'
        Nothing -> pure unit

setMetaTitle :: String -> Effect Unit
setMetaTitle title = do
    window >>= document >>= setTitle title
    setMetaContent title "meta-twitter-title"
    setMetaContent title "meta-og-title"

setMetaDescription :: String -> Effect Unit
setMetaDescription description = do
    setMetaContent description "meta-description"
    setMetaContent description "meta-twitter-description"
    setMetaContent description "meta-og-description"

setCanonicalUrl :: String -> Effect Unit
setCanonicalUrl url = do
    canonicalElement <- window >>= document <#> toNonElementParentNode >>= getElementById "canonical-url"
    case LinkElement.fromElement =<< canonicalElement of
        Just canonicalElement' -> setHref url canonicalElement'
        Nothing -> pure unit

setMetaUrl :: Effect Unit
setMetaUrl = do
    url <- window >>= location >>= href
    setMetaContent url "meta-og-url"
    setCanonicalUrl url

setMeta :: forall monad. MonadEffect monad => String -> String -> monad Unit
setMeta title description = liftEffect do
    setMetaTitle title
    setMetaDescription description
    setMetaUrl
