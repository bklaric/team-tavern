module TeamTavern.Client.Components.Welcome where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor

type Input =
    { email :: String
    , nickname :: String
    , emailSent :: Boolean
    , profile :: Maybe { title :: String }
    }

type ChildSlots slots =
    ( homeAnchor :: NavigationAnchor.Slot Unit
    | slots
    )

welcome :: forall action monad slots. MonadEffect monad =>
    Input -> H.ComponentHTML action (ChildSlots slots) monad
welcome { email, nickname, emailSent, profile } =
    HH.div [ HP.class_ $ HH.ClassName "welcome-container" ] $ pure $
    HH.div [ HP.class_ $ ClassName "welcome"] $
    [ HH.h1 [ HP.class_ $ HH.ClassName "welcome-heading" ] [ HH.text $ "Welcome to TeamTavern, " <> nickname <> "!" ] ]
    <>
    case profile of
    Just { title } -> Array.singleton $
        HH.p [ HP.class_ $ HH.ClassName "welcome-text" ]
        [ HH.text $ """Your account and your """ <> title <> """ player profile
            have been successfully created. They will be visible to other
            players once you confirm your email address."""
        ]
    Nothing -> []
    <>
    case emailSent of
    true -> Array.singleton $
        HH.p [ HP.class_ $ HH.ClassName "welcome-text" ]
        [ HH.text $ "A confirmation email has been sent to "
        , HH.strong_ [ HH.text email ]
        , HH.text ". Please use it to sign in and confirm your email address."
        ]
    false -> Array.singleton $
        HH.p [ HP.class_ $ HH.ClassName "welcome-text" ]
        [ HH.text $ "Unfortunately, we're having some issues sending "
            <> "a registration email to "
        , HH.strong_ [ HH.text email ]
        , HH.text ". Please contact "
        , HH.strong_ [ HH.text "admin@teamtavern.net" ]
        , HH.text " to verify your email address."
        ]
    <>
    [ navigationAnchor (SProxy :: SProxy "homeAnchor")
        { path: "/", content: HH.text "Back to home page" }
    ]
