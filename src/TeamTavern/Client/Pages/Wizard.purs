module TeamTavern.Client.Pages.Wizard where

import Prelude

import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input = { nickname :: String }

type Slot = H.Slot (Const Void) Void Unit

render :: forall slots action. Input -> HH.HTML slots action
render { nickname } = HH.div [ HP.class_ $ HH.ClassName "page-wizard"]
    [ HH.div [ HP.class_ $ HH.ClassName "page-wizard-step" ]
        [ HH.h1 [ HP.class_ $ HH.ClassName "page-wizard-title" ]
            [ HH.text $ "Hi, " <> nickname <> "!" ]
        , HH.p [ HP.class_ $ HH.ClassName "page-wizard-step-title" ]
            [ HH.text """Let's start with setting up your TeamTavern account and
                your first game profile."""
            ]
        , HH.button [ HP.class_ $ HH.ClassName "primary-button" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-gamepad button-icon" ] []
            , HH.text "Let's go"
            ]
        ]
    ]

component :: forall query output monad.
    H.Component HH.HTML query Input output monad
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

wizard :: forall action slots monad.
    Input -> HH.ComponentHTML action (wizard :: Slot | slots) monad
wizard input = HH.slot (SProxy :: SProxy "wizard") unit component input absurd
