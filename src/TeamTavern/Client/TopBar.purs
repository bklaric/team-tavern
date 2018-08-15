module TeamTavern.Client.TopBar where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onSubmit)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Window (alert)

data Query send
    = SignIn Event send
    | SignOut Event send

type State = { signedIn :: Boolean }

initialState :: { signedIn :: Boolean }
initialState = { signedIn: false }

render :: forall t7. State -> HH.HTML t7 (Query Unit)
render { signedIn } = HH.div_
    [ HH.p_ [ HH.text "This is the top bar, bitch!" ]
    , HH.form
        [ onSubmit $ if signedIn then HE.input SignOut else HE.input SignIn ]
        [ HH.input []
        , HH.button
            [ HP.type_ HP.ButtonSubmit ]
            [ HH.text $ if signedIn then "Sign out" else "Sign in" ]
        ]
    ]

eval :: Query ~> H.HalogenM State Query () Void Aff
eval = case _ of
    SignIn event send -> do
        H.liftEffect $ preventDefault event
        H.liftEffect $ window >>= alert "signin in"
        H.modify_ not
        pure send
    SignOut event send -> do
        H.liftEffect $ preventDefault event
        H.liftEffect $ window >>= alert "signin out"
        H.modify_ not
        pure send

topBar :: forall t107. H.Component HH.HTML Query t107 Void Aff
topBar =
    H.component
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        , initializer: Nothing
        , finalizer: Nothing
        }
