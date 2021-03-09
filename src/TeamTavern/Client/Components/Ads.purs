module TeamTavern.Client.Components.Ads where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (pageSize)

-- Desktop

descriptionLeaderboard :: forall slots action. HH.HTML slots action
descriptionLeaderboard = HH.div [ HP.id_ "nn_lb1", HS.class_ "description-leaderboard" ] []

sectionLeaderboard :: forall slots action. HH.HTML slots action
sectionLeaderboard = HH.div [ HP.id_ "nn_lb2", HS.class_ "section-leaderboard" ] []

profileSectionsWithLeaderboard :: forall slots action.
    Array (HH.HTML slots action) -> Array (HH.HTML slots action)
profileSectionsWithLeaderboard profileSections =
    case Array.insertAt (pageSize / 2) sectionLeaderboard profileSections of
    Just profileSections' -> profileSections'
    Nothing -> Array.snoc profileSections sectionLeaderboard

filtersMpu :: forall slots action. HH.HTML slots action
filtersMpu = HH.div [ HP.id_ "nn_mpu1", HS.class_ "filters-mpu" ] []

-- Mobile

mobileStickyLeaderboard :: forall slots action. HH.HTML slots action
mobileStickyLeaderboard = HH.div [ HP.id_ "nn_mobile_lb1_sticky" ] []

mobileDescriptionLeaderboard :: forall slots action. HH.HTML slots action
mobileDescriptionLeaderboard = HH.div [ HP.id_ "nn_mobile_lb2", HS.class_ "description-leaderboard" ] []
