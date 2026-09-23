module TeamTavern.Client.Pages.ForgotPassword (forgotPassword) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (null, trim)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Flow (flow, flowError, flowLead, flowLink, formTight, submitButton, textField)
import TeamTavern.Client.Script.Back (authPath, readBack)
import TeamTavern.Client.Shared.AccountErrors (somethingWrong)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Password.ForgotPassword (ForgotPassword)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

type State =
    { back :: String
    , email :: String
    , error :: Maybe String
    , formError :: Maybe String
    , sending :: Boolean
    , sentTo :: Maybe String
    }

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState
        ({ back: "/", email: "", error: Nothing, formError: Nothing, sending: false, sentTo: Nothing } :: State)

    let set = Hooks.modify_ stateId
        fail error formError = set _ { sending = false, error = error, formError = formError }

        submit event = do
            H.liftEffect $ preventDefault event
            let email = trim state.email
            if null email
            then fail (Just "Enter your email address.") Nothing
            else do
                set _ { sending = true, error = Nothing, formError = Nothing }
                result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ ForgotPassword) { email }
                case result of
                    Right response -> response # onMatch
                        { noContent: const $ set _ { sending = false, sentTo = Just email }
                        , notFound: const $ fail (Just "No account signs in with a password at this email.") Nothing
                        }
                        (const $ fail Nothing $ Just somethingWrong)
                    Left _ -> fail Nothing $ Just somethingWrong

    Hooks.useLifecycleEffect do
        back <- readBack
        set _ { back = back }
        pure Nothing

    let signInLine =
            HH.p [ HS.class_ "muted" ]
            [ HH.text "Remembered it? ", flowLink (authPath "/signin" state.back) "Sign in" ]

    Hooks.pure case state.sentTo of
        Just email -> flow
            [ HH.h1_ [ HH.text "Check your email" ]
            , flowLead $ "We sent a link to " <> email <> ". Open it within an hour to choose a new password."
            , signInLine
            ]
        Nothing -> flow
            [ HH.h1_ [ HH.text "Forgot password" ]
            , flowLead "Enter your account's email and we'll send you a link to choose a new password."
            , formTight submit $
                [ textField
                    { id: "forgot-email", label: "Email"
                    , type_: HP.InputEmail, autocomplete: HP.AutocompleteEmail
                    , hint: Nothing, error: state.error
                    , value: state.email, onInput: \value -> set _ { email = value }
                    }
                ]
                <> (case state.formError of
                    Just error -> [ flowError error ]
                    Nothing -> [])
                <> [ submitButton state.sending "Send link" ]
            , signInLine
            ]

forgotPassword :: ∀ action slots left.
    H.ComponentHTML action (forgotPassword :: Slot___ | slots) (Async left)
forgotPassword = HH.slot_ (Proxy :: _ "forgotPassword") unit component unit
