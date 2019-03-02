module TeamTavern.Client.SignIn (Query, Slot, signIn) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Session.Start.SendResponse as Start
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init send
    | NicknameOrEmailInput String send
    | PasswordInput String send
    | SignIn Event send

type State =
    { nicknameOrEmail :: String
    , password :: String
    , nonce :: Maybe String
    , unconfirmedEmail :: Boolean
    , nothingConfirmed :: Boolean
    , noSessionStarted :: Boolean
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
    { nicknameOrEmail
    , password
    , nonce
    , unconfirmedEmail
    , nothingConfirmed
    , noSessionStarted
    , otherError
    } = HH.form
    [ HE.onSubmit $ HE.input SignIn ]
    [ HH.h2_
        [ HH.text "Sign in to "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", text: "TeamTavern" }
        , HH.text $ maybe "" (const " to confirm your email address") nonce
        ]
    , HH.div_
        [ HH.label
            [ HP.for "nicknameOrEmail" ]
            [ HH.text "Nickname or email address" ]
        , HH.input
            [ HP.id_ "nicknameOrEmail"
            , HE.onValueInput $ HE.input NicknameOrEmailInput
            ]
        ]
    , HH.div_
        [ HH.label
            [ HP.for "password" ]
            [ HH.text "Password" ]
        , HH.input
            [ HP.id_ "password"
            , HP.autocomplete false
            , HP.type_ InputPassword
            , HE.onValueInput $ HE.input PasswordInput
            ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ nicknameOrEmail == "" || password == ""
        ]
        [ HH.text "Sign in" ]
    , HH.p
        [ HP.class_ $ otherErrorClass unconfirmedEmail ]
        [ HH.text "Please confirm your email address before signing in."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass nothingConfirmed ]
        [ HH.text
            $  "Something went wrong with confirming your email address. "
            <> "Please try again later or contact the administrator."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass noSessionStarted ]
        [ HH.text
            $  "Entered credentials don't appear to be valid. "
            <> "Please check and try again."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong. Please try again later." ]
    , HH.p_
        [ HH.text "New to TeamTavern? "
        , navigationAnchor (SProxy :: SProxy "registerAnchor")
            { path: "/register", text: "Create an account." }
        ]
    ]

sendSignInRequest :: forall left. State -> Async left (Maybe State)
sendSignInRequest state @ { nicknameOrEmail, password, nonce } = Async.unify do
    response <- Fetch.fetch "/api/sessions"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { nicknameOrEmail, password, nonce }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Start.BadRequestContent) -> Just $ match
                    { unconfirmedEmail:
                        const $ state { unconfirmedEmail = true }
                    , nothingConfirmed:
                        const $ state { nothingConfirmed = true }
                    , noSessionStarted:
                        const $ state { noSessionStarted = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init send) = do
    nonce <- getQueryParam "nonce" # H.liftEffect
    H.modify_ (_ { nonce = nonce })
    pure send
eval (NicknameOrEmailInput nicknameOrEmail send) =
    H.modify_ (_ { nicknameOrEmail = nicknameOrEmail }) <#> const send
eval (PasswordInput password send) =
    H.modify_ (_ { password = password }) <#> const send
eval (SignIn event send) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { unconfirmedEmail     = false
        , nothingConfirmed     = false
        , noSessionStarted     = false
        , otherError           = false
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
        { nicknameOrEmail: ""
        , password: ""
        , nonce: Nothing
        , unconfirmedEmail: false
        , nothingConfirmed: false
        , noSessionStarted: false
        , otherError: false
        }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ Init unit
    , finalizer: Nothing
    }

signIn :: forall query children left.
    HH.ComponentHTML query (signIn :: Slot Unit | children) (Async left)
signIn = HH.slot (SProxy :: SProxy "signIn") unit component unit absurd
