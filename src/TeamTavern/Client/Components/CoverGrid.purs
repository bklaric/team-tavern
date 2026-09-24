module TeamTavern.Client.Components.CoverGrid (coverGrid, feedPath) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

-- | Where a cover leads when it opens its game's feed.
feedPath :: String -> String
feedPath handle = "/games/" <> handle

-- Every game's cover, each leading where `href` says for its handle, with a
-- note on the covers that have one, such as the header's "Your post".
coverGrid :: ∀ w m game. MonadEffect m =>
    { games :: Array { handle :: String, title :: String | game }
    , href :: String -> String
    , mark :: String -> Maybe String
    }
    -> HH.HTML w (m Unit)
coverGrid { games, href, mark } =
    HH.div [ HS.class_ "cover-grid" ] $ games <#> \{ handle, title } -> let
        path = href handle
        in
        HH.a [ HS.class_ "cover", HP.href path, HE.onClick $ navigateWithEvent_ path ] $
        [ HH.img [ HP.src $ "/images/games/" <> handle <> ".webp", HP.alt title, HP.width 600, HP.height 900 ] ]
        <> maybe [] (\mark' -> [ HH.span [ HS.class_ "cover-mark" ] [ HH.text mark' ] ]) (mark handle)
