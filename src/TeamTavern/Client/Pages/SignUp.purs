module TeamTavern.Client.Pages.SignUp (signUp) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array.NonEmpty (elem)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, length, null, trim)
import Data.Tuple.Nested ((/\))
import Data.Variant (inj, match, onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Divider (rule)
import TeamTavern.Client.Components.Flow (flow, flowError, flowLead, flowLink, formTight, submitButton, textField)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Back (authPath, readBack)
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Discord (authorizeWithDiscord)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Client.Shared.AccountErrors (nicknameInvalid, nicknameTaken, passwordShort, somethingWrong)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

type Errors =
    { email :: Maybe String
    , nickname :: Maybe String
    , password :: Maybe String
    , form :: Maybe String
    }

noErrors :: Errors
noErrors = { email: Nothing, nickname: Nothing, password: Nothing, form: Nothing }

type State =
    { back :: String
    , email :: String
    , nickname :: String
    , password :: String
    , errors :: Errors
    , sending :: Boolean
    }

emailInvalid :: String
emailInvalid = "Enter your email address."

-- The same checks the server makes, so most mistakes are named before sending.
validate :: State -> Errors
validate { email, nickname, password } = noErrors
    { email = if contains (Pattern "@") email' && contains (Pattern ".") email' then Nothing else Just emailInvalid
    , nickname = if null $ trim nickname then Just "Choose a nickname." else Nothing
    , password = if length password < 8 then Just passwordShort else Nothing
    }
    where
    email' = trim email

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState
        ({ back: "/", email: "", nickname: "", password: "", errors: noErrors, sending: false } :: State)

    let set = Hooks.modify_ stateId
        failWith errors = set _ { sending = false, errors = errors }

        submit event = do
            H.liftEffect $ preventDefault event
            let errors = validate state
            if isJust errors.email || isJust errors.nickname || isJust errors.password
            then failWith errors
            else do
                set _ { sending = true, errors = noErrors }
                result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ RegisterPlayer)
                    (inj (Proxy :: _ "password")
                        { email: trim state.email, nickname: trim state.nickname, password: state.password })
                case result of
                    Right response -> response # onMatch
                        { noContent: const $ navigate_ state.back
                        , badRequest: match
                            { registration: \registrationErrors -> let
                                has error = elem error registrationErrors
                                in
                                failWith noErrors
                                    { email = if has (inj (Proxy :: _ "email") {}) then Just emailInvalid else Nothing
                                    , nickname = if has (inj (Proxy :: _ "nickname") {}) then Just nicknameInvalid else Nothing
                                    , password = if has (inj (Proxy :: _ "password") {}) then Just passwordShort else Nothing
                                    }
                            , emailTaken: const $ failWith noErrors
                                { email = Just "An account already uses this email. Sign in instead." }
                            , nicknameTaken: const $ failWith noErrors { nickname = Just nicknameTaken }
                            , discordTaken: const $ failWith noErrors { form = Just somethingWrong }
                            }
                        , forbidden: const $ navigate_ state.back
                        }
                        (const $ failWith noErrors { form = Just somethingWrong })
                    Left _ -> failWith noErrors { form = Just somethingWrong }

    Hooks.useLifecycleEffect do
        signedIn <- hasPlayerIdCookie
        back <- readBack
        if signedIn then navigateReplace_ back else set _ { back = back }
        pure Nothing

    Hooks.pure $ flow
        [ HH.h1_ [ HH.text "Create your account" ]
        , flowLead "Find players, groups and communities, and hear when someone new fits."
        , button Outline Regular (authorizeWithDiscord state.back)
            [ Icons.discord, HH.text "Continue with Discord" ]
        , rule "or"
        , formTight submit $
            [ textField
                { id: "signup-email", label: "Email"
                , type_: HP.InputEmail, autocomplete: HP.AutocompleteEmail
                , hint: Nothing, error: state.errors.email
                , value: state.email, onInput: \value -> set _ { email = value }
                }
            , textField
                { id: "signup-nickname", label: "Nickname"
                , type_: HP.InputText, autocomplete: HP.AutocompleteNickname
                , hint: Just "Shown on your posts and your messages.", error: state.errors.nickname
                , value: state.nickname, onInput: \value -> set _ { nickname = value }
                }
            , textField
                { id: "signup-password", label: "Password"
                , type_: HP.InputPassword, autocomplete: HP.AutocompleteNewPassword
                , hint: Just "At least 8 characters.", error: state.errors.password
                , value: state.password, onInput: \value -> set _ { password = value }
                }
            ]
            <> (case state.errors.form of
                Just error -> [ flowError error ]
                Nothing -> [])
            <> [ submitButton state.sending "Create account" ]
        , HH.p [ HS.class_ "muted" ]
            [ HH.text "Already have an account? ", flowLink (authPath "/signin" state.back) "Sign in" ]
        ]

signUp :: ∀ action slots left. H.ComponentHTML action (signUp :: Slot___ | slots) (Async left)
signUp = HH.slot_ (Proxy :: _ "signUp") unit component unit
