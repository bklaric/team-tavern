module TeamTavern.Client.Pages.Team.DeleteTeam (deleteTeam) where

import Prelude

import Async (Async)
import Control.Monad.Trans.Class (lift)
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
import TeamTavern.Routes.Shared.Organization (nameOrHandleNW)
import TeamTavern.Routes.Team.DeleteTeam (DeleteTeam)
import TeamTavern.Routes.Team.ViewTeam as ViewTeam
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

component :: ∀ query output left.
    H.Component query ViewTeam.OkContent output (Async left)
component = Hooks.component \_ { handle } -> Hooks.do
    { otherError, submitting } /\ stateId <-
        Hooks.useState { otherError: false, submitting: false }
    let onSubmit event = do
            liftEffect $ preventDefault event
            currentState <- Hooks.modify stateId _ { submitting = true }
            response <- lift $ fetchPathNoContent
                (Proxy :: _ DeleteTeam) { handle }
            case response of
                Just _ -> hardNavigate "/"
                Nothing -> Hooks.put stateId currentState
                    { submitting = false, otherError = true }
    Hooks.pure $
        form onSubmit $
        [ HH.p [ HS.class_ "boarding-description" ]
            [ HH.text $ "Are you sure you want to delete your team?" ]
        , submitButton "fas fa-trash" "Delete team" "Deleting team..." submitting
        ]
        <> otherFormError otherError

deleteTeam
    :: ∀ action children left
    .  ViewTeam.OkContent
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (deleteTeam :: Modal.Slot_ | children) (Async left)
deleteTeam team handleMessage = HH.slot
    (Proxy :: _ "deleteTeam") unit
    (Modal.component ("Delete " <> nameOrHandleNW team.handle team.organization <> " team") component)
    team handleMessage
