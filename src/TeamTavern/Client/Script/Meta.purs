module TeamTavern.Client.Script.Meta where

import Prelude

import Data.Array (mapWithIndex)
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
import Web.HTML.Location (origin, pathname)
import Web.HTML.Window (document, location)
import Yoga.JSON (class WriteForeign, write, writeJSON)

foreign import setStructuredData_ :: String -> Effect Unit

foreign import clearStructuredData_ :: Effect Unit

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

setMetaRobots :: ∀ monad. MonadEffect monad => String -> monad Unit
setMetaRobots content = liftEffect $ setMetaContent content "meta-robots"

setMeta :: ∀ monad. MonadEffect monad => String -> String -> monad Unit
setMeta title description = liftEffect do
    setMetaTitle title
    setMetaDescription description
    setMetaUrl

-- A page without JSON-LD has no script for it at all, since an empty one is
-- unparsable to a search engine.
setStructuredData :: ∀ monad record. MonadEffect monad => WriteForeign record => record -> monad Unit
setStructuredData = liftEffect <<< setStructuredData_ <<< writeJSON

clearStructuredData :: ∀ monad. MonadEffect monad => monad Unit
clearStructuredData = liftEffect clearStructuredData_

-- The site's logo is the square PNG, since search engines take no SVG logo.
setSiteData :: ∀ monad. MonadEffect monad => String -> monad Unit
setSiteData description = liftEffect do
    origin' <- window >>= location >>= origin
    let home = origin' <> "/"
    setStructuredData
        { "@context": "https://schema.org"
        , "@graph":
            [ write
                { "@type": "Organization"
                , "@id": home <> "#organization"
                , name: "TeamTavern"
                , url: home
                , logo: origin' <> "/logo-512.png"
                }
            , write
                { "@type": "WebSite"
                , "@id": home <> "#website"
                , name: "TeamTavern"
                , url: home
                , description
                , publisher: { "@id": home <> "#organization" }
                }
            ]
        }

-- | The trail from the home page, which comes first, to the page itself, which
-- | comes last.
setBreadcrumbs :: ∀ monad. MonadEffect monad => Array { name :: String, path :: String } -> monad Unit
setBreadcrumbs crumbs = liftEffect do
    origin' <- window >>= location >>= origin
    setStructuredData
        { "@context": "https://schema.org"
        , "@type": "BreadcrumbList"
        , itemListElement: [ { name: "Home", path: "/" } ] <> crumbs # mapWithIndex \index { name, path } ->
            { "@type": "ListItem", position: index + 1, name, item: origin' <> path }
        }
