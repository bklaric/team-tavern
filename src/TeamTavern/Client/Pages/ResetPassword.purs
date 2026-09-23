module TeamTavern.Client.Pages.ResetPassword (resetPassword) where

import Prelude

import Async (Async)
import Async as Async
import Control.Bind (bindFlipped)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import JSURI (decodeURIComponent)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), buttonLink)
import TeamTavern.Client.Components.Flow (flow, flowError, flowLead, flowLink, formTight, submitButton, textField)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Shared.AccountErrors (passwordShort, somethingWrong)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Password.ResetPassword (ResetPassword)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

data Screen = Form | Expired | Done

type State =
    { screen :: Screen
    , nonce :: String
    , password :: String
    , error :: Maybe String
    , formError :: Maybe String
    , sending :: Boolean
    }

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState
        ({ screen: Form, nonce: "", password: "", error: Nothing, formError: Nothing, sending: false } :: State)

    let set = Hooks.modify_ stateId
        fail error formError = set _ { sending = false, error = error, formError = formError }

        submit event = do
            H.liftEffect $ preventDefault event
            if length state.password < 8
            then fail (Just passwordShort) Nothing
            else do
                set _ { sending = true, error = Nothing, formError = Nothing }
                result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ ResetPassword)
                    { password: state.password, nonce: state.nonce }
                case result of
                    Right response -> response # onMatch
                        { noContent: const $ set _ { sending = false, screen = Done }
                        , badRequest: const $ fail (Just passwordShort) Nothing
                        , notFound: const $ set _ { sending = false, screen = Expired }
                        }
                        (const $ fail Nothing $ Just somethingWrong)
                    Left _ -> fail Nothing $ Just somethingWrong

    Hooks.useLifecycleEffect do
        nonce <- getQueryParam "nonce" <#> bindFlipped decodeURIComponent
        case nonce of
            Just nonce' -> set _ { nonce = nonce' }
            Nothing -> set _ { screen = Expired }
        pure Nothing

    Hooks.pure case state.screen of
        Done -> flow
            [ HH.h1_ [ HH.text "Your password is changed" ]
            , flowLead "Sign in with your new password."
            , buttonLink Primary Regular "/signin" [ HH.text "Sign in" ]
            ]
        Expired -> flow
            [ HH.h1_ [ HH.text "This link doesn't work" ]
            , flowLead "A password reset link works once, within an hour of being sent."
            , HH.p [ HS.class_ "muted" ] [ flowLink "/forgot-password" "Send a new link" ]
            ]
        Form -> flow
            [ HH.h1_ [ HH.text "Choose a new password" ]
            , formTight submit $
                [ textField
                    { id: "reset-password", label: "New password"
                    , type_: HP.InputPassword, autocomplete: HP.AutocompleteNewPassword
                    , hint: Just "At least 8 characters.", error: state.error
                    , value: state.password, onInput: \value -> set _ { password = value }
                    }
                ]
                <> (case state.formError of
                    Just error -> [ flowError error ]
                    Nothing -> [])
                <> [ submitButton state.sending "Change password" ]
            ]

resetPassword :: ∀ action slots left.
    H.ComponentHTML action (resetPassword :: Slot___ | slots) (Async left)
resetPassword = HH.slot_ (Proxy :: _ "resetPassword") unit component unit
