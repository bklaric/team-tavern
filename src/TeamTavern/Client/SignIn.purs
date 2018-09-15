module TeamTavern.Client.SignIn (Query, Slot, signIn) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Session.Start.Response as Start
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
    }

type Slot = H.Slot Query Void

type ChildSlots =
    ( home :: NavigationAnchor.Slot Unit
    , codeAnchor :: NavigationAnchor.Slot Unit
    , registerAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render
    { nickname
    , nonce
    , nicknameError
    , nonceError
    , noTokenToConsume
    , otherError
    } = HH.form
    [ HE.onSubmit $ HE.input SignIn ]
    [ HH.h2_
        [ HH.text "Sign in to "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", text: "TeamTavern" }
        ]
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
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Lmao, something else got fucked and you're shit out of luck, mate!"]
    , navigationAnchor (SProxy :: SProxy "registerAnchor")
        { path: "/register", text: "Register" }
    , navigationAnchor (SProxy :: SProxy "codeAnchor")
        { path: "/code", text: "Get a sign in code" }
    ]

sendSignInRequest :: forall left. State -> Async left (Maybe State)
sendSignInRequest state @ { nickname, nonce } = Async.unify do
    response <- Fetch.fetch "/api/sessions"
        (  Fetch.method := PATCH
        <> Fetch.body := Json.writeJSON { nickname, nonce }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Start.BadRequestContent) -> Just $ match
                    { invalidNicknamedNonce: foldl (\state' -> match
                        { invalidNickname:
                            const $ state' { nicknameError = true }
                        , invalidNonce:
                            const $ state' { nonceError = true }
                        })
                        state
                    , noTokenToConsume:
                        const $ state { noTokenToConsume = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (NicknameInput nickname send) =
    H.modify_ (_ { nickname = nickname }) <#> const send
eval (NonceInput nonce send) =
    H.modify_ (_ { nonce = nonce }) <#> const send
eval (SignIn event send) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { nicknameError    = false
        , nonceError       = false
        , noTokenToConsume = false
        , otherError       = false
        })
    newState <- H.lift $ sendSignInRequest state
    case newState of
        Nothing -> H.liftEffect $ navigate_ "/"
        Just newState' -> H.put newState'
    pure send

component :: forall input left.
    H.Component HH.HTML Query input Void (Async left)
component = H.component
    { initialState: const
        { nickname: ""
        , nonce: ""
        , nicknameError: false
        , nonceError: false
        , noTokenToConsume: false
        , otherError: false
        }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }

signIn :: forall query children left.
    HH.ComponentHTML query (signIn :: Slot Unit | children) (Async left)
signIn = HH.slot (SProxy :: SProxy "signIn") unit component unit absurd
