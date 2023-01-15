module TeamTavern.Client.Script.ReloadAds (reloadAds) where

import Prelude

import Data.Foldable (foldMap)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM.ChildNode (remove)
import Web.DOM.Element (toChildNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

foreign import reloadAdsImpl :: Effect Unit

-- If we don't delete the wrapper after page navigation, CSS for sticky ads isn't applied.
-- This celtra-reveal-wrapper thing is something they add around sticky ads.
-- So we get a structure like: div#celtra-reveal-wrapper > div.sticky-leaderboard > the actual ad.
removeWrapper :: Effect Unit
removeWrapper = do
    wrapper <- window >>= document <#> toNonElementParentNode >>= getElementById "celtra-reveal-wrapper"
    foldMap (toChildNode >>> remove) wrapper

reloadAds :: ∀ monad. MonadEffect monad => monad Unit
reloadAds = liftEffect do
    removeWrapper
    reloadAdsImpl
