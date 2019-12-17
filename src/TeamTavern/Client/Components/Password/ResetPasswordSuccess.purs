module TeamTavern.Client.Components.Password.ResetPasswordSuccess where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor

type ChildSlots slots =
    ( signInAnchor :: NavigationAnchor.Slot Unit
    | slots
    )

resetPasswordSuccess :: forall action monad slots. MonadEffect monad =>
    H.ComponentHTML action (ChildSlots slots) monad
resetPasswordSuccess =
    HH.div [ HP.class_ $ HH.ClassName "welcome-container" ] $ pure $
    HH.div [ HP.class_ $ ClassName "welcome"] $
    [ HH.h1 [ HP.class_ $ HH.ClassName "welcome-heading" ]
        [ HH.text $ "Password successfully reset!" ]
    , HH.p [ HP.class_ $ HH.ClassName "welcome-text" ]
        [ HH.text "Your TeamTavern account password has been successfully reset. "
        , HH.text "You can now sign in using the new password."
        ]
    , navigationAnchor (SProxy :: SProxy "signInAnchor")
        { path: "/signin", content: HH.text "Sign in" }
    ]
