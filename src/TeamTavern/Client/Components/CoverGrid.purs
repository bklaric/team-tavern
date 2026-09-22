module TeamTavern.Client.Components.CoverGrid (coverGrid) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

type Game = { handle :: String, title :: String }

-- Every game's cover, each opening that game's feed, with a note on the covers
-- that have one, such as the header's "Your post".
coverGrid :: ∀ w m. MonadEffect m =>
    { games :: Array Game, mark :: String -> Maybe String } -> HH.HTML w (m Unit)
coverGrid { games, mark } =
    HH.div [ HS.class_ "cover-grid" ] $ games <#> \{ handle, title } -> let
        path = "/games/" <> handle
        in
        HH.a [ HS.class_ "cover", HP.href path, HE.onClick $ navigateWithEvent_ path ] $
        [ HH.img [ HP.src $ "/images/games/" <> handle <> ".webp", HP.alt title ] ]
        <> maybe [] (\mark' -> [ HH.span [ HS.class_ "cover-mark" ] [ HH.text mark' ] ]) (mark handle)
