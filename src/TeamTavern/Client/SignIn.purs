module TeamTavern.Client.SignIn where

import Prelude

import Async (Async)
import Async as A
import Browser.Async.Fetch as FA
import Browser.Async.Fetch.Response as FARes
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Variant as V
import Effect.Class.Console (log)
import Error.Class as E
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as J
import TeamTavern.Player.Session.Start.Run.CreateResponse (BadRequestResponseContent)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = NicknameInput String send
    | NonceInput String send
    | SignIn Event send

type State =
    { nickname :: String
    , nonce :: String
    , nicknameError :: Boolean
    , nonceError :: Boolean
    , noTokenToConsume :: Boolean
    , otherError :: Boolean
    , alreadySignedIn :: Boolean
    }

data Message = SignedIn

initialState :: State
initialState =
    { nickname: ""
    , nonce: ""
    , nicknameError: false
    , nonceError: false
    , noTokenToConsume: false
    , otherError: false
    , alreadySignedIn: false
    }

errorClass :: Boolean -> ClassName
errorClass hasError = ClassName if hasError then "error" else ""

inputErrorClass :: Boolean -> ClassName
inputErrorClass hasError = ClassName
    if hasError then "input-error" else "hidden"

otherErrorClass :: Boolean -> ClassName
otherErrorClass hasError = ClassName
    if hasError then "other-error" else "hidden"

render :: forall m. State -> H.ComponentHTML Query () m
render
    { nickname
    , nonce
    , nicknameError
    , nonceError
    , noTokenToConsume
    , otherError
    , alreadySignedIn
    } = HH.form
    [ HE.onSubmit $ HE.input SignIn ]
    [ HH.h2_ [ HH.text "Sign in to TeamTavern" ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass nicknameError, HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ errorClass nicknameError
            , HE.onValueInput $ HE.input NicknameInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text "Please enter a valid nickname. The nickname must: "]
        , HH.ul
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.li_ [ HH.text "Have no spaces" ]
            , HH.li_ [ HH.text "Be some shit I forgot" ]
            ]
        ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass nonceError, HP.for "nonce" ]
            [ HH.text "Sign in code" ]
        , HH.input
            [ HP.id_ "nonce"
            , HP.class_ $ errorClass nonceError
            , HP.autocomplete false
            , HE.onValueInput $ HE.input NonceInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nonceError ]
            [ HH.text "This does not look like a valid nonce. Jesus Christ, how dense are you? "]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ nickname == "" || nonce == ""
        ]
        [ HH.text "Sign in" ]
    , HH.p
        [ HP.class_ $ otherErrorClass noTokenToConsume ]
        [ HH.text "Credentials don't appear to be valid. Please request another sign in code."]
    , HH.p
        [ HP.class_ $ otherErrorClass alreadySignedIn ]
        [ HH.text "You're already signed in. Please sign out to sign in."]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Lmao, something else got fucked and you're shit out of luck, mate!"]
    ]

eval :: forall void. Query ~> H.HalogenM State Query () Message (Async void)
eval = case _ of
    NicknameInput nickname send -> do
        H.modify_ (_ { nickname = nickname })
        pure send
    NonceInput nonce send -> do
        H.modify_ (_ { nonce = nonce })
        pure send
    SignIn event send -> let
        setNicknameError    = _ { nicknameError    = true }
        setNonceError       = _ { nonceError       = true }
        setNoTokenToConsume = _ { noTokenToConsume = true }
        setOtherError       = _ { otherError       = true }
        setAlreadySignedIn  = _ { alreadySignedIn  = true }
        in do
        H.liftEffect $ preventDefault event
        resetState @ { nickname, nonce } <- H.gets (_
            { nicknameError    = false
            , nonceError       = false
            , noTokenToConsume = false
            , otherError       = false
            , alreadySignedIn  = false
            })
        response' <- H.lift $ A.attempt $ FA.fetch
            ("http://localhost:8080/players/by-nickname/" <> nickname <> "/sessions")
            (FA.method := PATCH <> FA.body := J.writeJSON { nickname, nonce } )
        resetState' <- case response' of
            Left error -> do
                H.liftEffect $ log $ show $ E.message error
                pure $ setOtherError resetState
            Right response ->
                case FARes.status response of
                204 -> do
                    H.liftEffect $ log $ "Signed in"
                    H.raise SignedIn
                    pure resetState
                400 -> do
                    content <- H.lift $ FARes.text response
                    case J.readJSON content of
                        Left errors -> do
                            H.liftEffect $ log $ show errors
                            pure $ setOtherError resetState
                        Right (error :: BadRequestResponseContent) -> V.match
                            { invalidNickname:
                                const $ setNicknameError resetState
                            , invalidNonce:
                                const $ setNonceError resetState
                            , noTokenToConsume:
                                const $ setNoTokenToConsume resetState
                            , other:
                                const $ setOtherError resetState
                            }
                            error
                            # pure
                403 -> pure $ setAlreadySignedIn resetState
                _ -> pure $ setOtherError resetState
        H.put resetState'
        pure send

signIn :: forall input void.
    H.Component HH.HTML Query input Message (Async void)
signIn = H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
