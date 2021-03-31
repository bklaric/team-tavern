module TeamTavern.Client.Script.Meta where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import TeamTavern.Client.Script.Analytics (registerPageView)
import TeamTavern.Client.Script.ReloadAds (reloadAds)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle, toNonElementParentNode)
import Web.HTML.HTMLLinkElement (setHref)
import Web.HTML.HTMLLinkElement as LinkElement
import Web.HTML.HTMLMetaElement (setContent)
import Web.HTML.HTMLMetaElement as MetaElement
import Web.HTML.Location (origin, pathname)
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

setLink :: String -> String -> Effect Unit
setLink id url = do
    link <- window >>= document <#> toNonElementParentNode >>= getElementById id
    case LinkElement.fromElement =<< link of
        Just link' -> setHref url link'
        Nothing -> pure unit

setMetaUrl :: Effect Unit
setMetaUrl = do
    origin' <- window >>= location >>= origin
    pathname' <- window >>= location >>= pathname
    let url = origin' <> pathname'
    setMetaContent url "meta-og-url"
    setLink "canonical-url" url
    setLink "hreflang-en" url
    setLink "hreflang-default" url

setMeta :: forall monad. MonadEffect monad => String -> String -> monad Unit
setMeta title description = liftEffect do
    setMetaTitle title
    setMetaDescription description
    setMetaUrl
    reloadAds
    registerPageView
