module TeamTavern.Client.Pages.ResetPasswordSuccess where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))

resetPasswordSuccess :: forall action monad. MonadEffect monad =>
    H.ComponentHTML action _ monad
resetPasswordSuccess =
    HH.div [HS.class_ "welcome-container"] $ pure $
    HH.div [HS.class_ "welcome"] $
    [ HH.h1 [HS.class_ "welcome-heading"] [HH.text $ "Password successfully reset!"]
    , HH.p [HS.class_ "welcome-text"]
        [ HH.text $ "Your TeamTavern account password has been successfully reset. "
            <> "You can now sign in using the new password."
        ]
    , navigationAnchor (Proxy :: _ "signInAnchor")
        {path: "/signin", content: HH.text "Sign in"}
    ]
