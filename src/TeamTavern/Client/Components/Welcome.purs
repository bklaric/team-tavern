module TeamTavern.Client.Components.Welcome where

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

welcome
    :: forall action monad slots
    .  MonadEffect monad
    => { email :: String, nickname :: String, emailSent :: Boolean }
    -> H.ComponentHTML action (ChildSlots slots) monad
welcome { email, nickname, emailSent } =
    HH.div [ HP.class_ $ ClassName "welcome"] $
    [ HH.h3_ [ HH.text $ "Welcome to TeamTavern, " <> nickname <> "!" ] ]
    <> if emailSent
        then
            [ HH.p_
                [ HH.text $ "A registration email has been sent to "
                , HH.strong_ [ HH.text email ]
                , HH.text ". Please verify your email address before signin in."
                ]
            , navigationAnchor (SProxy :: SProxy "homeAnchor")
                { path: "/", content: HH.text "Back to home page" }
            ]
        else
            [ HH.p_
                [ HH.text $ "Unfortunately, we're having some issues sending "
                    <> "a registration email to "
                , HH.strong_ [ HH.text email ]
                , HH.text ". Please contact "
                , HH.strong_ [ HH.text "admin@teamtavern.net" ]
                , HH.text " to verify your email address."
                ]
            , navigationAnchor (SProxy :: SProxy "homeAnchor")
                { path: "/", content: HH.text "Back to home page" }
            ]
