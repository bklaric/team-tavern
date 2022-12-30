module TeamTavern.Client.Pages.Player.DeletePlayerProfile (Slot, deletePlayerProfile) where

import Prelude

import Async (Async)
import Control.Monad.Trans.Class (lift)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Shared.Fetch (fetchPathNoContent)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Profile.DeletePlayerProfile (DeletePlayerProfile)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

type Input =
    { player :: ViewPlayer.OkContent
    , profile :: ViewPlayer.OkContentProfile
    }

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { player: { nickname }, profile: { handle, title } } -> Hooks.do
    { otherError, submitting } /\ stateId <-
        Hooks.useState { otherError: false, submitting: false }
    let onSubmit event = do
            liftEffect $ preventDefault event
            currentState <- Hooks.modify stateId _ { submitting = true }
            response <- lift $ fetchPathNoContent
                (Proxy :: _ DeletePlayerProfile) { nickname, handle }
            case response of
                Just _ -> hardNavigate $ "/players/" <> nickname
                Nothing -> Hooks.put stateId currentState
                    { submitting = false, otherError = true }
    Hooks.pure $
        form onSubmit $
        [ HH.p [ HS.class_ "boarding-description" ]
            [ HH.text $ "Are you sure you want to delete your " <> title <> " profile?" ]
        , submitButton "fas fa-trash" "Delete profile" "Deleting profile..." submitting
        ]
        <> otherFormError otherError

deletePlayerProfile
    :: ∀ action children left
    .  Input
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (deletePlayerProfile :: Slot | children) (Async left)
deletePlayerProfile input handleMessage = HH.slot
    (Proxy :: _ "deletePlayerProfile") unit
    (Modal.component ("Delete " <> input.profile.title <> " profile") component)
    input handleMessage
