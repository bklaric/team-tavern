module TeamTavern.Client.Components.Ads where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

-- Desktop

descriptionLeaderboard :: ∀ slots action. HH.HTML slots action
descriptionLeaderboard = HH.div [HP.id "nn_lb2", HS.class_ "ad"] []

stickyLeaderboard :: ∀ slots action. HH.HTML slots action
stickyLeaderboard = HH.div [HP.id "nn_lb1", HS.class_ "sticky-leaderboard"] []

mpu :: ∀ slots action. HH.HTML slots action
mpu = HH.div [HP.id "nn_mpu1", HS.class_ "ad"] []

filtersMpu :: ∀ slots action. HH.HTML slots action
filtersMpu = HH.div [HP.id "nn_mpu1", HS.class_ "filters-mpu"] []

skinLeft :: ∀ slots action. HH.HTML slots action
skinLeft = HH.div [HP.id "nn_skinl", HS.class_ ""] []

skinRight :: ∀ slots action. HH.HTML slots action
skinRight = HH.div [HP.id "nn_skinr", HS.class_ ""] []

-- Mobile

mobileDescriptionLeaderboard :: ∀ slots action. HH.HTML slots action
mobileDescriptionLeaderboard = HH.div [HP.id "nn_mobile_lb2", HS.class_ "ad"] []

mobileStickyLeaderboard :: ∀ slots action. HH.HTML slots action
mobileStickyLeaderboard = HH.div [HP.id "nn_mobile_lb1_sticky", HS.class_ "sticky-leaderboard"] []

mobileMpu :: ∀ slots action. HH.HTML slots action
mobileMpu = HH.div [HP.id "nn_mobile_mpu1", HS.class_ "ad"] []

-- Both

descriptionLeaderboards :: ∀ slots action. Array (HH.HTML slots action)
descriptionLeaderboards = [descriptionLeaderboard, mobileDescriptionLeaderboard]

stickyLeaderboards :: ∀ slots action. Array (HH.HTML slots action)
stickyLeaderboards = [stickyLeaderboard, mobileStickyLeaderboard]

mpus :: ∀ slots action. Array (HH.HTML slots action)
mpus = [mpu, mobileMpu]

player :: ∀ slots action. HH.HTML slots action
player = HH.div [HP.id "nn_player"] []

-- Utils

insertMpusInMiddleOrAtEnd :: ∀ slots action.
    Array (HH.HTML slots action) -> Array (HH.HTML slots action)
insertMpusInMiddleOrAtEnd array =
    -- Try to insert after the third element.
    case Array.insertAt 3 mpu array >>= Array.insertAt 4 mobileMpu of
    Nothing -> Array.snoc (Array.snoc array mpu) mobileMpu
    Just arrayWithAds -> arrayWithAds

insertMobileMpuInMiddleOrAtEnd :: ∀ slots action.
    Array (HH.HTML slots action) -> Array (HH.HTML slots action)
insertMobileMpuInMiddleOrAtEnd array =
    -- Try to insert after the third element.
    case Array.insertAt 3 mobileMpu array of
    Nothing -> Array.snoc array mobileMpu
    Just arrayWithAds -> arrayWithAds
