module TeamTavern.Client.Script.Meta where

import Prelude

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import TeamTavern.Client.Snippets.Cover (coverPath)
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

-- | The image a shared link shows, at a path on the site's origin. Its size
-- | lets a preview lay out the image before it has fetched it.
setMetaImage :: ∀ monad. MonadEffect monad =>
    { path :: String, alt :: String, width :: Int, height :: Int, type_ :: String } -> monad Unit
setMetaImage { path, alt, width, height, type_ } = liftEffect do
    origin' <- window >>= location >>= origin
    setMetaContent (origin' <> path) "meta-twitter-image"
    setMetaContent (origin' <> path) "meta-og-image"
    setMetaContent alt "meta-twitter-image-alt"
    setMetaContent alt "meta-og-image-alt"
    setMetaContent (show width) "meta-og-image-width"
    setMetaContent (show height) "meta-og-image-height"
    setMetaContent type_ "meta-og-image-type"

setLogoImage :: ∀ monad. MonadEffect monad => monad Unit
setLogoImage = setMetaImage
    { path: "/logo-512.png", alt: "TeamTavern logo", width: 512, height: 512, type_: "image/png" }

-- | A game's cover, for its feed and its posts, at the size `build-covers.mjs`
-- | holds every cover to.
setCoverImage :: ∀ monad fields. MonadEffect monad => { handle :: String, title :: String | fields } -> monad Unit
setCoverImage { handle, title } = setMetaImage
    { path: coverPath handle, alt: title <> " cover", width: 600, height: 900, type_: "image/webp" }

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

organizationId :: String -> String
organizationId origin' = origin' <> "/#organization"

-- The site's logo is the square PNG, since search engines take no SVG logo.
organization :: String -> Foreign
organization origin' = write
    { "@type": "Organization"
    , "@id": organizationId origin'
    , name: "TeamTavern"
    , url: origin' <> "/"
    , logo: origin' <> "/logo-512.png"
    }

setSiteData :: ∀ monad. MonadEffect monad => String -> monad Unit
setSiteData description = liftEffect do
    origin' <- window >>= location >>= origin
    let home = origin' <> "/"
    setStructuredData
        { "@context": "https://schema.org"
        , "@graph":
            [ organization origin'
            , write
                { "@type": "WebSite"
                , "@id": home <> "#website"
                , name: "TeamTavern"
                , url: home
                , description
                , publisher: { "@id": organizationId origin' }
                }
            ]
        }

type Crumb = { name :: String, path :: String }

-- The trail from the home page, which comes first, to the page itself, which
-- comes last.
breadcrumbItems :: String -> Array Crumb -> Array Foreign
breadcrumbItems origin' crumbs =
    [ { name: "Home", path: "/" } ] <> crumbs # mapWithIndex \index { name, path } ->
        write { "@type": "ListItem", position: index + 1, name, item: origin' <> path }

setBreadcrumbs :: ∀ monad. MonadEffect monad => Array Crumb -> monad Unit
setBreadcrumbs crumbs = liftEffect do
    origin' <- window >>= location >>= origin
    setStructuredData
        { "@context": "https://schema.org"
        , "@type": "BreadcrumbList"
        , itemListElement: breadcrumbItems origin' crumbs
        }

-- | A guide is an article by TeamTavern itself, which names no one person, on
-- | the trail from the home page through the guides. The dates are ISO.
setGuideData :: ∀ monad. MonadEffect monad =>
    { path :: String, heading :: String, description :: String, published :: String, updated :: String }
    -> monad Unit
setGuideData { path, heading, description, published, updated } = liftEffect do
    origin' <- window >>= location >>= origin
    setStructuredData
        { "@context": "https://schema.org"
        , "@graph":
            [ organization origin'
            , write
                { "@type": "Article"
                , "@id": origin' <> path <> "#article"
                , headline: heading
                , description
                , image: origin' <> "/logo-512.png"
                , datePublished: published
                , dateModified: updated
                , mainEntityOfPage: origin' <> path
                , author: { "@id": organizationId origin' }
                , publisher: { "@id": organizationId origin' }
                }
            , write
                { "@type": "BreadcrumbList"
                , itemListElement: breadcrumbItems origin'
                    [ { name: "Guides", path: "/guides" }, { name: heading, path } ]
                }
            ]
        }
