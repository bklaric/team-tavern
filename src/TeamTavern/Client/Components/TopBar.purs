module TeamTavern.Client.Components.TopBar where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (PlayerInfo, deletePlayerInfo)
import TeamTavern.Client.Script.Navigate (navigate_)

data Query send = SignOut send

type State = Maybe PlayerInfo

type Message = Void

type Slot = H.Slot Query Message

type ChildSlots =
    ( homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , registerAnchor :: NavigationAnchor.Slot Unit
    , profileAnchor :: NavigationAnchor.Slot Unit
    )

_homeAnchor = SProxy :: SProxy "homeAnchor"

_signInAnchor = SProxy :: SProxy "signInAnchor"

_registerAnchor = SProxy :: SProxy "registerAnchor"

_profileAnchor = SProxy :: SProxy "profileAnchor"

render :: forall m. MonadEffect m => State -> H.ComponentHTML Query ChildSlots m
render playerInfo = HH.div_
    [ HH.div [ HP.id_ "top-bar-filler" ] []
    , HH.div [ HP.id_ "top-bar" ]
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
                [ HH.slot _profileAnchor unit navigationAnchor
                    { path: "/players/" <> nickname, text: nickname } absurd
                , HH.button [ HE.onClick $ HE.input_ SignOut ]
                    [ HH.text "Sign out" ]
                ]
        ]
    ]

eval :: forall m. MonadEffect m =>
    Query ~> H.HalogenM State Query ChildSlots Message m
eval (SignOut send) = do
    deletePlayerInfo # H.liftEffect
    H.put Nothing
    navigate_ "/" # H.liftEffect
    pure send

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
