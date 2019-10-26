module TeamTavern.Client.Home where

import Prelude

import Async (Async)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON (read)
import TeamTavern.Client.Home.Games (games)
import TeamTavern.Client.Home.Games as Games
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.RegisterForm as RegisterForm
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.Components.Welcome (welcome)
import TeamTavern.Client.Components.WelcomeBanner (welcomeBanner)
import TeamTavern.Client.Components.WelcomeBanner as WelcomeBanner
import TeamTavern.Client.Game (game)
import TeamTavern.Client.Game as Game
import TeamTavern.Client.Home.CallToAction (callToAction)
import TeamTavern.Client.Player (player)
import TeamTavern.Client.Player as Player
import TeamTavern.Client.Register (register)
import TeamTavern.Client.Register as Register
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.SignIn (signIn)

home = HH.div_ [ topBar, callToAction, games ]
