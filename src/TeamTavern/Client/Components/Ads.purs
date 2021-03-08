module TeamTavern.Client.Components.Ads where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

descriptionLeaderboard :: forall slots action. HH.HTML slots action
descriptionLeaderboard = HH.div [ HP.id_ "nn_lb1", HS.class_ "description-leaderboard" ] []

leaderboard2 :: forall slots action. HH.HTML slots action
leaderboard2 = HH.div [ HP.id_ "nn_lb2" ] []

midPageUnit1 :: forall slots action. HH.HTML slots action
midPageUnit1 = HH.div [ HP.id_ "nn_mpu1" ] []

stickyMobileLeaderboard :: forall slots action. HH.HTML slots action
stickyMobileLeaderboard = HH.div [ HP.id_ "nn_mobile_lb1_sticky" ] []

mobileDescriptionLeaderboard :: forall slots action. HH.HTML slots action
mobileDescriptionLeaderboard = HH.div [ HP.id_ "nn_mobile_lb2", HS.class_ "description-leaderboard" ] []
