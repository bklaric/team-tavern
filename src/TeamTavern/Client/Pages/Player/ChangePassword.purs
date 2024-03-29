module TeamTavern.Client.Pages.Player.ChangePassword (changePassword) where

import Prelude

import Async (Async, attempt)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (match, onMatch)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Input (inputGroup, inputLabel_)
import TeamTavern.Client.Components.InputError as InputError
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.PasswordInput (passwordInput)
import TeamTavern.Client.Script.Analytics (track_)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Shared.Fetch (fetchPathBody)
import TeamTavern.Routes.Player.UpdatePlayerPassword (UpdatePlayerPassword)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

type Input = {nickname :: String}

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ {nickname} -> Hooks.do
    passwordOld /\ passwordOldId <- Hooks.useState ""
    passwordOldWrong /\ passwordOldWrongId <- Hooks.useState false
    passwordNew /\ passwordNewId <- Hooks.useState ""
    passwordNewError /\ passwordNewErrorId <- Hooks.useState false
    otherError /\ otherErrorId <- Hooks.useState false
    submitting /\ submittingId <- Hooks.useState false
    let onSubmit event = do
            liftEffect $ preventDefault event
            Hooks.put passwordOldWrongId false
            Hooks.put passwordNewErrorId false
            Hooks.put otherErrorId false
            Hooks.put submittingId true
            response <- lift $ attempt $
                fetchPathBody (Proxy :: _ UpdatePlayerPassword)
                {nickname} {passwordOld, passwordNew}
            Hooks.put submittingId false
            case response of
                Left _ -> Hooks.put otherErrorId true
                Right response' -> response' # onMatch
                    { noContent: const do
                        track_ "Password change"
                        hardNavigate $ "/players/" <> nickname
                    , badRequest: match
                        { password: const $ Hooks.put passwordNewErrorId true
                        , wrongPassword: const $ Hooks.put passwordOldWrongId true
                        }
                    }
                    (const $ Hooks.put otherErrorId true)
    Hooks.pure $
        form onSubmit $
        [ inputGroup $
            [ inputLabel_ "Current password"
            , passwordInput (Proxy :: _ "passwordInputOld") passwordOld (Hooks.put passwordOldId)
            ]
            <> InputError.passwordWrong passwordOldWrong
        , inputGroup $
            [ inputLabel_ "New password"
            , passwordInput (Proxy :: _ "passwordInputNew") passwordNew (Hooks.put passwordNewId)
            ]
            <> InputError.passwordError passwordNewError
        , submitButton "fas fa-edit" "Change password" "Changing password..." submitting
        ]
        <> otherFormError otherError

changePassword
    :: ∀ action children left
    .  Input
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (changePassword :: Modal.Slot_ | children) (Async left)
changePassword input handleMessage = HH.slot
    (Proxy :: _ "changePassword") unit
    (Modal.component ("Change password") component)
    input handleMessage
