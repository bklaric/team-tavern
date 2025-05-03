module TeamTavern.Client.Components.Ads where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

billboard :: ∀ slots action. HH.HTML slots action
billboard = HH.div [HP.id "slot-1", HS.class_ "ad"] []

doubleMpu :: ∀ slots action. HH.HTML slots action
doubleMpu = HH.div [HP.id "slot-2", HS.class_ "ad"] []

leaderboard :: ∀ slots action. HH.HTML slots action
leaderboard = HH.div [HP.id "slot-3", HS.class_ "ad"] []

mobileBanner :: ∀ slots action. HH.HTML slots action
mobileBanner = HH.div [HP.id "slot-4", HS.class_ "ad"] []

mobileMpu :: ∀ slots action. HH.HTML slots action
mobileMpu = HH.div [HP.id "slot-5", HS.class_ "ad"] []

mpu :: ∀ slots action. HH.HTML slots action
mpu = HH.div [HP.id "slot-6", HS.class_ "ad"] []

skyscrapper :: ∀ slots action. HH.HTML slots action
skyscrapper = HH.div [HP.id "slot-7", HS.class_ "ad"] []

desktopTakeover :: ∀ slots action. HH.HTML slots action
desktopTakeover = HH.div [HP.id "slot-8", HS.class_ "ad"] []

mobileTakeover :: ∀ slots action. HH.HTML slots action
mobileTakeover = HH.div [HP.id "slot-9", HS.class_ "ad"] []

video :: ∀ slots action. HH.HTML slots action
video = HH.div [HP.id "slot-10", HS.class_ "ad"] []

-- Utils

insertAdsInMiddle :: ∀ slots action.
    Array (HH.HTML slots action) -> Array (HH.HTML slots action)
insertAdsInMiddle array =
    -- Try to insert after the third element.
    case Array.insertAt 3 billboard array >>= Array.insertAt 4 mobileTakeover of
    Nothing -> array
    Just arrayWithAds ->
    -- Try to insert after the sixth element, which is now the eight.
        case Array.insertAt 8 leaderboard arrayWithAds >>= Array.insertAt 9 mobileMpu of
        Nothing -> arrayWithAds
        Just arrayWithMoreAds -> arrayWithMoreAds

videoIfWideEnough :: forall slots13 action14. Int -> Array (HH.HTML slots13 action14)
videoIfWideEnough windowWidth = guard (windowWidth >= 1000) [video]
