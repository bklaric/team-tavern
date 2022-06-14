module TeamTavern.Client.Components.Ads where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

-- Desktop

descriptionLeaderboard :: forall slots action. HH.HTML slots action
descriptionLeaderboard = HH.div [ HP.id "nn_lb2", HS.class_ "description-leaderboard" ] []

stickyLeaderboard :: forall slots action. HH.HTML slots action
stickyLeaderboard = HH.div [ HP.id "nn_lb1", HS.class_ "sticky-leaderboard" ] []

filtersMpu :: forall slots action. HH.HTML slots action
filtersMpu = HH.div [ HP.id "nn_mpu1", HS.class_ "filters-mpu" ] []

-- Mobile

mobileDescriptionLeaderboard :: forall slots action. HH.HTML slots action
mobileDescriptionLeaderboard = HH.div [ HP.id "nn_mobile_lb2", HS.class_ "description-leaderboard" ] []

mobileStickyLeaderboard :: forall slots action. HH.HTML slots action
mobileStickyLeaderboard = HH.div [ HP.id "nn_mobile_lb1_sticky", HS.class_ "sticky-leaderboard" ] []

-- Both

descriptionLeaderboards :: forall slots action. Array (HH.HTML slots action)
descriptionLeaderboards = [ descriptionLeaderboard, mobileDescriptionLeaderboard ]

stickyLeaderboards :: forall slots action. Array (HH.HTML slots action)
stickyLeaderboards = [ stickyLeaderboard, mobileStickyLeaderboard ]
