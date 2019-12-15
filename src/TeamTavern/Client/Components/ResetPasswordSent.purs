module TeamTavern.Client.Components.ResetPasswordSent where

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
    ( homeAnchor :: NavigationAnchor.Slot Unit
    | slots
    )

resetPasswordSent
    :: forall action monad slots
    .  MonadEffect monad
    => { email :: String }
    -> H.ComponentHTML action (ChildSlots slots) monad
resetPasswordSent { email } =
    HH.div [ HP.class_ $ HH.ClassName "welcome-container" ] $ pure $
    HH.div [ HP.class_ $ ClassName "welcome"] $
    [ HH.h1 [ HP.class_ $ HH.ClassName "welcome-heading" ]
        [ HH.text $ "Password reset email sent!" ]
    , HH.p [ HP.class_ $ HH.ClassName "welcome-text" ]
        [ HH.text $ "A password reset link has been sent to "
        , HH.strong_ [ HH.text email ]
        , HH.text ". Open it to reset your TeamTavern account password."
        ]
    , navigationAnchor (SProxy :: SProxy "homeAnchor")
        { path: "/", content: HH.text "Back to home page" }
    ]
