module TeamTavern.Client.Components.Ads where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

-- Desktop

descriptionLeaderboard :: ∀ slots action. HH.HTML slots action
descriptionLeaderboard = HH.div [ HP.id "nn_lb2", HS.class_ "description-leaderboard" ] []

stickyLeaderboard :: ∀ slots action. HH.HTML slots action
stickyLeaderboard = HH.div [ HP.id "nn_lb1", HS.class_ "sticky-leaderboard" ] []

filtersMpu :: ∀ slots action. HH.HTML slots action
filtersMpu = HH.div [ HP.id "nn_mpu1", HS.class_ "filters-mpu" ] []

-- Mobile

mobileDescriptionLeaderboard :: ∀ slots action. HH.HTML slots action
mobileDescriptionLeaderboard = HH.div [ HP.id "nn_mobile_lb2", HS.class_ "description-leaderboard" ] []

mobileStickyLeaderboard :: ∀ slots action. HH.HTML slots action
mobileStickyLeaderboard = HH.div [ HP.id "nn_mobile_lb1_sticky", HS.class_ "sticky-leaderboard" ] []

-- Both

descriptionLeaderboards :: ∀ slots action. Array (HH.HTML slots action)
descriptionLeaderboards = [ descriptionLeaderboard, mobileDescriptionLeaderboard ]

stickyLeaderboards :: ∀ slots action. Array (HH.HTML slots action)
stickyLeaderboards = [ stickyLeaderboard, mobileStickyLeaderboard ]
