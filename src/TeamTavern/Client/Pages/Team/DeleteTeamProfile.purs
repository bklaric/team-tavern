module TeamTavern.Client.Pages.Team.DeleteTeamProfile where

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
import TeamTavern.Routes.Profile.DeleteTeamProfile (DeleteTeamProfile)
import TeamTavern.Routes.Team.ViewTeam as ViewTeam
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

type Input =
    { team :: ViewTeam.OkContent
    , profile :: ViewTeam.OkContentProfile
    }

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { team, profile } -> Hooks.do
    { otherError, submitting } /\ stateId <-
        Hooks.useState { otherError: false, submitting: false }
    let onSubmit event = do
            liftEffect $ preventDefault event
            currentState <- Hooks.modify stateId _ { submitting = true }
            response <- lift $ fetchPathNoContent
                (Proxy :: _ DeleteTeamProfile)
                { teamHandle: team.handle, gameHandle: profile.handle }
            case response of
                Just _ -> hardNavigate $ "/teams/" <> team.handle
                Nothing -> Hooks.put stateId currentState
                    { submitting = false, otherError = true }
    Hooks.pure $
        form onSubmit $
        [ HH.p [ HS.class_ "boarding-description" ]
            [ HH.text $ "Are you sure you want to delete your team's " <> profile.title <> " profile?" ]
        , submitButton "fas fa-trash" "Delete profile" "Deleting profile..." submitting
        ]
        <> otherFormError otherError

deleteTeamProfile
    :: ∀ action children left
    .  Input
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (deleteTeamProfile :: Modal.Slot_ | children) (Async left)
deleteTeamProfile input handleMessage = HH.slot
    (Proxy :: _ "deleteTeamProfile") unit
    (Modal.component ("Delete " <> input.profile.title <> " profile") component)
    input handleMessage
