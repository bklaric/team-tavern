module TeamTavern.Client.Pages.ResetPasswordSent where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))

resetPasswordSent :: forall action monad. MonadEffect monad =>
    {email :: String} -> H.ComponentHTML action _ monad
resetPasswordSent {email} =
    HH.div [HS.class_ "welcome-container"] $ pure $
    HH.div [HS.class_ "welcome"] $
    [ HH.h1 [HS.class_ "welcome-heading"] [HH.text $ "Password reset email sent!"]
    , HH.p [HS.class_ "welcome-text"]
        [ HH.text $ "A password reset link has been sent to "
        , HH.strong_ [HH.text email]
        , HH.text ". Open it to reset your TeamTavern account password."
        ]
    , navigationAnchor (Proxy :: _ "homeAnchor")
        {path: "/", content: HH.text "Back to home page"}
    ]
