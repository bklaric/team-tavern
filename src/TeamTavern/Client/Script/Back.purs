module TeamTavern.Client.Script.Back (authPath, currentBack, readBack) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array (elem, head)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.String (Pattern(..), split, stripPrefix)
import Effect.Class (class MonadEffect, liftEffect)
import JSURI (decodeURIComponent, encodeURIComponent)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import Web.HTML (window)
import Web.HTML.Location (pathname, search)
import Web.HTML.Window (location)

-- The account pages send the player back where they came from once they are
-- done, to the path carried in `?back=`.

accountPaths :: Array String
accountPaths = ["/signup", "/signin", "/forgot-password", "/reset-password", "/confirm-email"]

isAccountPath :: String -> Boolean
isAccountPath path =
    split (Pattern "?") path # head # fromMaybe path # flip elem accountPaths

-- Only a path on this site, so the parameter can't send the player elsewhere,
-- and never an account page, which would only send them on again.
safeBack :: String -> String
safeBack back
    | isJust (stripPrefix (Pattern "/") back)
    , not $ isJust (stripPrefix (Pattern "//") back)
    , not $ isAccountPath back = back
    | otherwise = "/"

-- | Where the account page being shown returns to, the home page by default.
readBack :: ∀ effect. MonadEffect effect => effect String
readBack = getQueryParam "back" <#> bindFlipped decodeURIComponent <#> maybe "/" safeBack

-- | Where an account page opened from the current page returns to: the current
-- | page, or, from an account page, where that one returns to.
currentBack :: ∀ effect. MonadEffect effect => effect String
currentBack = do
    location' <- window >>= location # liftEffect
    path <- pathname location' # liftEffect
    query <- search location' # liftEffect
    if isAccountPath path then readBack else pure $ safeBack $ path <> query

-- | The account page at the path, returning to back.
authPath :: String -> String -> String
authPath path "/" = path
authPath path back = path <> "?back=" <> fromMaybe "" (encodeURIComponent back)
