module TeamTavern.Client.Main where

import Prelude

import Async as A
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (liftEffect)
import Effect.Console as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Navigate (navigate)
import TeamTavern.Client.SignIn as SignIn
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Query send
    = ChangeRoute String send
    | HandleSignIn SignIn.Message send
    | Navigate String MouseEvent send

data State
    = Home
    | SignIn
    | Register
    | Code
    | NotFound

type ChildSlots =
  ( signIn :: SignIn.Slot Unit
  , signInAnchor :: NavigationAnchor.Slot Unit
  )

_signIn = SProxy :: SProxy "signIn"

_signInAnchor = SProxy :: SProxy "signInAnchor"

type Message = Void

render :: forall void. State -> H.ComponentHTML Query ChildSlots (A.Async void)
render Home = HH.div_
    [ HH.h1_ [ HH.text "Welcome to TeamTavern, you cunt!" ]
    , HH.slot _signInAnchor unit navigationAnchor
        { path: "/signin", text: "Sign in" } absurd
    ]
render SignIn = HH.slot _signIn unit SignIn.signIn unit $ HE.input HandleSignIn
render Register = HH.p_ [ HH.text "Be sure to pick a funny nickname." ]
render Code = HH.p_ [ HH.text "Prepare to get spammed." ]
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Message (A.Async void)
eval (ChangeRoute route send) =
    const send <$> H.put case route of
    "/" -> Home
    "/signin" -> SignIn
    "/register" -> Register
    "/code" -> Code
    _ -> NotFound
eval (HandleSignIn SignIn.SignedIn send) =
    liftEffect $ const send <$> C.log "Signed in"
eval (Navigate path event send) = liftEffect do
    preventDefault $ MouseEvent.toEvent event
    navigate path
    pure send

main :: forall input void.
    H.Component HH.HTML Query input Message (A.Async void)
main = H.component
    { initialState: const Home
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
