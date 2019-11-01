module TeamTavern.Client.Script.Meta where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle, toNonElementParentNode)
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

setMetaUrl :: Effect Unit
setMetaUrl = do
    url <- window >>= location >>= href
    setMetaContent url "meta-og-url"
