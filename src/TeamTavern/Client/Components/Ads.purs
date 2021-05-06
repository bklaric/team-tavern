module TeamTavern.Client.Components.Ads where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

-- Desktop

descriptionLeaderboard :: forall slots action. HH.HTML slots action
descriptionLeaderboard = HH.div [ HP.id_ "nn_lb1", HS.class_ "description-leaderboard" ] []

stickyLeaderboard :: forall slots action. HH.HTML slots action
stickyLeaderboard = HH.div [ HP.id_ "nn_lb2", HS.class_ "sticky-leaderboard" ] []

filtersMpu :: forall slots action. HH.HTML slots action
filtersMpu = HH.div [ HP.id_ "nn_mpu1", HS.class_ "filters-mpu" ] []

-- Mobile

mobileStickyLeaderboard :: forall slots action. HH.HTML slots action
mobileStickyLeaderboard = HH.div [ HP.id_ "nn_mobile_lb2", HS.class_ "sticky-leaderboard" ] []

mobileDescriptionLeaderboard :: forall slots action. HH.HTML slots action
mobileDescriptionLeaderboard = HH.div [ HP.id_ "nn_mobile_lb2", HS.class_ "description-leaderboard" ] []

-- Both

stickyLeaderboards :: forall t16 t17. Array (HH.HTML t17 t16)
stickyLeaderboards = [ stickyLeaderboard, mobileStickyLeaderboard ]
