module TeamTavern.Client.Pages.Player.CreateTeam where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import CSS as CSS
import Data.Array (intercalate)
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.String as String
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.ModalDeclarative as Modal
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails (enterPlayerDetails)
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails as EnterPlayerDetails
import TeamTavern.Client.Pages.Wizard.EnterTeamDetails (enterTeamDetails)
import TeamTavern.Client.Pages.Wizard.EnterTeamDetails as EnterTeamDetails
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Server.Player.UpdateDetails.SendResponse as Update
import TeamTavern.Server.Player.ViewDetails.SendResponse as ViewAccount
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = { nickname :: String }

data Output = TeamCreated { handle :: String } | CloseClicked

type State =
    { nickname :: String
    , details :: EnterTeamDetails.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = UpdateDetails EnterTeamDetails.Output
    | SendRequest Event
    | Close

type ChildSlots = (enterTeamDetails :: EnterTeamDetails.Slot)

type Slot = H.Slot (Const Void) (Modal.Output Output) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $
    HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< SendRequest ]
    [ closeButton Close
    , HH.h1 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Create team" ]
    , HH.p [ HP.class_ $ HH.ClassName "form-subheading", HC.style $ CSS.marginBottom $ CSS.px 0.0 ]
        [ HH.text """Yo, write about your team and what are your members like."""
        ]
    , enterTeamDetails details (Just <<< UpdateDetails)
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled submitting
        , HC.style $ CSS.marginTop $ CSS.px 21.0
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
        , HH.text
            if submitting
            then "Creating team..."
            else "Create team"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (UpdateDetails details) =
    H.modify_ \state -> state
        { details = state.details
            { name = details.name
            , website = details.website
            , ageFrom = details.ageFrom
            , ageTo = details.ageTo
            , locations = details.locations
            , languages = details.languages
            , hasMicrophone = details.hasMicrophone
            , discordServer = details.discordServer
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            , about = details.about
            }
        }
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    H.raise $ TeamCreated { handle: "dota-gang" }
    -- currentState <- H.modify _ { submitting = true }
    -- response <- H.lift $ sendRequest currentState.nickname currentState.details
    -- case response of
    --     Just (Right _) -> H.raise $ DetailsEditted $ currentState.nickname
    --     Just (Left errors) -> H.put $
    --         foldl
    --         (\state error ->
    --             match
    --             { invalidDiscordTag: const $ state
    --                 { details = state.details { discordTagError = true } }
    --             , invalidAbout: const $ state
    --                 { details = state.details { aboutError = true } }
    --             }
    --             error
    --         )
    --         (currentState
    --             { submitting = false
    --             , details = currentState.details
    --                 { discordTagError = false, aboutError = false }
    --             , otherError = false
    --             }
    --         )
    --         errors
    --     Nothing -> H.put currentState
    --         { submitting = false
    --         , details = currentState.details
    --             { discordTagError = false, aboutError = false }
    --         , otherError = true
    --         }
handleAction Close = H.raise CloseClicked

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ nickname } ->
        { nickname
        , details: EnterTeamDetails.emptyInput
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

createTeam
    :: forall action children left
    .  Input
    -> (Modal.Output Output -> Maybe action)
    -> HH.ComponentHTML action (createTeam :: Slot | children) (Async left)
createTeam input handleMessage = HH.slot
    (SProxy :: SProxy "createTeam") unit
    (Modal.component component) input handleMessage
