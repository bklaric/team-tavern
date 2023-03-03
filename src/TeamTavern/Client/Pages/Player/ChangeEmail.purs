module TeamTavern.Client.Pages.Player.ChangeEmail (changeEmail) where

import Prelude

import Async (Async, attempt)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Tuple.Nested ((/\))
import Data.Variant (match, onMatch)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Input (inputGroup, inputLabel_, requiredTextLineInputNamed)
import TeamTavern.Client.Components.InputError as InputError
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.PasswordInput (passwordInput_)
import TeamTavern.Client.Script.Analytics (track_)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Shared.Fetch (fetchPathBody)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.UpdatePlayerEmail (UpdatePlayerEmail)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

type Input = {email :: Maybe String, nickname :: String}

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ input @ {nickname} -> Hooks.do
    email /\ emailId <- Hooks.useState $ maybe "" identity input.email
    emailError /\ emailErrorId <- Hooks.useState false
    emailTaken /\ emailTakenId <- Hooks.useState false
    password /\ passwordId <- Hooks.useState ""
    wrongPassword /\ wrongPasswordId <- Hooks.useState false
    otherError /\ otherErrorId <- Hooks.useState false
    submitting /\ submittingId <- Hooks.useState false
    let onSubmit event = do
            liftEffect $ preventDefault event
            Hooks.put emailErrorId false
            Hooks.put emailTakenId false
            Hooks.put wrongPasswordId false
            Hooks.put otherErrorId false
            Hooks.put submittingId true
            response <- lift $ attempt $
                fetchPathBody (Proxy :: _ UpdatePlayerEmail)
                {nickname} {email, password}
            Hooks.put submittingId false
            case response of
                Left _ -> Hooks.put otherErrorId true
                Right response' -> response' # onMatch
                    { noContent: const do
                        track_ "Email change"
                        hardNavigate $ "/players/" <> nickname
                    , badRequest: match
                        { email: const $ Hooks.put emailErrorId true
                        , emailTaken: const $ Hooks.put emailTakenId true
                        , wrongPassword: const $ Hooks.put wrongPasswordId true
                        }
                    }
                    (const $ Hooks.put otherErrorId true)
    Hooks.pure $
        form onSubmit $
        [ inputGroup $
            [ inputLabel_ "Email"
            , requiredTextLineInputNamed "email" email $ Hooks.put emailId
            ]
            <> InputError.emailError emailError
            <> InputError.emailTaken emailTaken
        , inputGroup $
            [ inputLabel_ "Password"
            , passwordInput_ password (Hooks.put passwordId)
            ]
            <> InputError.passwordWrong wrongPassword
        , HH.p [ HS.class_ "boarding-description" ]
            [ HH.text $ "Enter your password to confirm changing your email." ]
        , submitButton "fas fa-edit" "Change email" "Changing email..." submitting
        ]
        <> otherFormError otherError

changeEmail
    :: ∀ action children left
    .  Input
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (changeEmail :: Modal.Slot_ | children) (Async left)
changeEmail input handleMessage = HH.slot
    (Proxy :: _ "changeEmail") unit
    (Modal.component ("Change email") component)
    input handleMessage
