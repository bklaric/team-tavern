module TeamTavern.Client.TopBar where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (PlayerInfo)

data Query send = Void Void

type State = Maybe PlayerInfo

type Message = Void

type Slot = H.Slot Query Message

type ChildSlots =
    ( homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , registerAnchor :: NavigationAnchor.Slot Unit
    )

_homeAnchor = SProxy :: SProxy "homeAnchor"

_signInAnchor = SProxy :: SProxy "signInAnchor"

_registerAnchor = SProxy :: SProxy "registerAnchor"

render :: forall m. MonadEffect m => State -> H.ComponentHTML Query ChildSlots m
render playerInfo = HH.div [ HP.id_ "top-bar" ]
    [ HH.span [HP.id_ "top-bar-title" ]
        [ HH.slot _homeAnchor unit navigationAnchor
            { path: "/", text: "TeamTavern" } absurd
        ]
    , HH.div_ case playerInfo of
        Nothing ->
            [ HH.slot _registerAnchor unit navigationAnchor
                { path: "/register", text: "Register" } absurd
            , HH.slot _signInAnchor unit navigationAnchor
                { path: "/signin", text: "Sign in" } absurd
            ]
        Just { id, nickname } ->
            [ HH.slot _signInAnchor unit navigationAnchor
                { path: "/players/" <> nickname, text: nickname } absurd
            , HH.button [] [ HH.text "Sign out" ]
            ]
    ]

eval :: forall m. Query ~> H.HalogenM State Query ChildSlots Message m
eval (Void void) = absurd void

topBar :: forall m. MonadEffect m =>
    H.Component HH.HTML Query (Maybe PlayerInfo) Message m
topBar =
    H.component
        { initialState: identity
        , render
        , eval
        , receiver: const Nothing
        , initializer: Nothing
        , finalizer: Nothing
        }
