module TeamTavern.Client.Main where

import Prelude

import Async as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON (read)
import TeamTavern.Client.Code as Code
import TeamTavern.Client.Register as Register
import TeamTavern.Client.SignIn as SignIn
import TeamTavern.Client.TopBar (topBar)
import TeamTavern.Client.TopBar as TopBar

data Query send = ChangeRoute Foreign String send

data State
    = Home
    | SignIn
    | Register
    | Welcome { email :: String, nickname :: String, emailSent :: Boolean }
    | Code
    | NotFound

type ChildSlots =
  ( topBar :: TopBar.Slot Unit
  , signIn :: SignIn.Slot Unit
  , register :: Register.Slot Unit
  , code :: Code.Slot Unit
  )

_topBar = SProxy :: SProxy "topBar"

_signIn = SProxy :: SProxy "signIn"

_register = SProxy :: SProxy "register"

_code = SProxy :: SProxy "code"

type Message = Void

render :: forall void. State -> H.ComponentHTML Query ChildSlots (A.Async void)
render Home = HH.slot _topBar unit topBar unit absurd
render SignIn = HH.slot _signIn unit SignIn.signIn unit absurd
render Register = HH.slot _register unit Register.register unit absurd
render (Welcome { email, nickname, emailSent }) = HH.p_
    [ HH.text $ "Welcome to TeamTavern, " <> nickname <> "!"
    , HH.br_
    , if emailSent
        then HH.text $ "Registration email with your sign in code has been sent to " <> email
        else HH.text $ "Registration email has NOT been sent to " <> email
    ]
render Code = HH.slot _code unit Code.code unit absurd
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Message (A.Async void)
eval (ChangeRoute state route send) =
    const send <$> H.put case route of
    "/" -> Home
    "/signin" -> SignIn
    "/register" -> Register
    "/welcome" ->
        case read state of
        Left _ -> Home
        Right identifiers -> Welcome identifiers
    "/code" -> Code
    _ -> NotFound

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
