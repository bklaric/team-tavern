module TeamTavern.Client.Pages.Player.EditDetails
    (Input(..), Output(..), Slot, editDetails) where

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
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails (enterPlayerDetails)
import TeamTavern.Client.Pages.Wizard.EnterPlayerDetails as EnterPlayerDetails
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Server.Player.UpdateDetails.SendResponse as Update
import TeamTavern.Server.Player.ViewDetails.SendResponse as ViewAccount
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = { nickname :: String, details :: ViewAccount.OkContent }

data Action
    = UpdatePlayerDetails EnterPlayerDetails.Output
    | Update Event
    | Close

data Output = DetailsEditted String | CloseClicked

type State =
    { nickname :: String
    , otherError :: Boolean
    , submitting :: Boolean
    , details :: EnterPlayerDetails.Input
    }

type ChildSlots = (enterPlayerDetails :: EnterPlayerDetails.Slot)

type Slot = H.Slot (Const Void) (Modal.Output Output) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $
    HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update ]
    [ HH.h1 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Edit player details" ]
    , HH.p [ HP.class_ $ HH.ClassName "form-subheading", HC.style $ CSS.marginBottom $ CSS.px 0.0 ]
        [ HH.text """Enter details about yourself so your new gay gamer friends
            can find you, faggot. Fill out as much as you can to ensure the
            gayest gamers find you. All fields are optional, bitch."""
        ]
    , enterPlayerDetails details (Just <<< UpdatePlayerDetails)
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled submitting
        , HC.style $ CSS.marginTop $ CSS.px 21.0
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text
            if submitting
            then "Editing details..."
            else "Edit details"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

sendRequest :: forall left.
    String -> EnterPlayerDetails.Input -> Async left (Maybe (Either Update.BadRequestContent Unit))
sendRequest nickname details = Async.unify do
    response <-
        Fetch.fetch
        ("/api/players/by-nickname/" <> nickname <> "/details")
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON
            { birthday: if String.null details.birthday then Nothing else Just details.birthday
            , country: details.location
            , languages: details.languages
            , hasMicrophone: details.microphone
            , discordTag: if String.null details.discordTag then Nothing else Just details.discordTag
            , timezone: details.timezone
            , weekdayFrom: if String.null details.weekdayFrom then Nothing else Just details.weekdayFrom
            , weekdayTo: if String.null details.weekdayTo then Nothing else Just details.weekdayTo
            , weekendFrom: if String.null details.weekendFrom then Nothing else Just details.weekendFrom
            , weekendTo: if String.null details.weekendTo then Nothing else Just details.weekendTo
            , about: details.about
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const Nothing)
    content <- case FetchRes.status response of
        204 -> pure $ Right unit
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just content

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (UpdatePlayerDetails details) =
    H.modify_ \state -> state
        { details = state.details
            { birthday = details.birthday
            , location = details.location
            , languages = details.languages
            , microphone = details.microphone
            , discordTag = details.discordTag
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            , about = details.about
            }
        }
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState.nickname currentState.details
    case response of
        Just (Right _) -> H.raise $ DetailsEditted $ currentState.nickname
        Just (Left errors) -> H.put $
            foldl
            (\state error ->
                match
                { invalidDiscordTag: const $ state
                    { details = state.details { discordTagError = true } }
                , invalidAbout: const $ state
                    { details = state.details { aboutError = true } }
                }
                error
            )
            (currentState
                { submitting = false
                , details = currentState.details
                    { discordTagError = false, aboutError = false }
                , otherError = false
                }
            )
            errors
        Nothing -> H.put currentState
            { submitting = false
            , details = currentState.details
                { discordTagError = false, aboutError = false }
            , otherError = true
            }
handleAction Close = H.raise CloseClicked

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ nickname, details } ->
        { nickname
        , details:
            { birthday: maybe "" identity details.birthday
            , location: details.country
            , languages: details.languages
            , microphone: details.hasMicrophone
            , discordTag: maybe "" identity details.discordTag
            , timezone: details.timezone
            , weekdayFrom: maybe "" identity $ _.sourceFrom <$> details.weekdayOnline
            , weekdayTo: maybe "" identity $ _.sourceTo <$> details.weekdayOnline
            , weekendFrom: maybe "" identity $ _.sourceFrom <$> details.weekendOnline
            , weekendTo: maybe "" identity $ _.sourceTo <$> details.weekendOnline
            , about: intercalate "\n\n" details.about
            , discordTagError: false
            , aboutError: false
            }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editDetails
    :: forall action children left
    .  Input
    -> (Modal.Output Output -> Maybe action)
    -> HH.ComponentHTML action (editDetails :: Slot | children) (Async left)
editDetails input handleMessage = HH.slot
    (SProxy :: SProxy "editDetails") unit
    (Modal.component component) input handleMessage
