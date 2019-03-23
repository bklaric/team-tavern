module TeamTavern.Client.EditProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (intercalate)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Profile.Update.SendResponse as Update
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { handle :: String
    , title :: String
    , nickname :: String
    , summary :: Array String
    }

data Action
    = SummaryInput String
    | Update Event

data Message = ProfileUpdated String

type State =
    { nickname :: String
    , handle :: String
    , title :: String
    , summary :: String
    , summaryError :: Boolean
    , otherError :: Boolean
    }

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Message)

render :: forall slots. State -> HH.HTML slots Action
render { title, summary, summaryError, otherError } = HH.form
    [ HP.class_ $ H.ClassName "single-form", HE.onSubmit $ Just <<< Update ]
    [ HH.h2_ [ HH.text $ "Edit your " <> title <> " profile" ]
    , HH.div_
        [ HH.label
            [ HP.for "summary" ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HE.onValueInput $ Just <<< SummaryInput
            , HP.value summary
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text
                "The summary cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ summary == ""
        ]
        [ HH.text "Save changes" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

updateProfile :: forall left. State -> Async left (Maybe State)
updateProfile state @ { nickname, handle, summary } = Async.unify do
    response <- Fetch.fetch
        ("/api/profiles/single/" <> handle <> "/" <> nickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { summary }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidSummary: const $ state { summaryError = true } }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall slots left.
    Action -> H.HalogenM State Action slots Message (Async left) Unit
handleAction (SummaryInput summary) = do
    H.modify_ (_ { summary = summary }) $> unit
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { summaryError = false
        , otherError   = false
        })
    newState <- H.lift $ updateProfile state
    case newState of
        Nothing -> H.raise $ ProfileUpdated state.nickname
        Just newState' -> H.put newState'
    pure unit

component :: forall query left.
    H.Component HH.HTML query Input Message (Async left)
component = H.mkComponent
    { initialState: \{ handle, title, nickname, summary } ->
        { handle
        , title
        , nickname
        , summary: intercalate "\n\n" summary
        , summaryError: false
        , otherError: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editProfile
    :: forall query children left
    .  (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query (editProfile :: Slot Unit | children) (Async left)
editProfile handleMessage = HH.slot
    (SProxy :: SProxy "editProfile") unit
    (Modal.component component) unit handleMessage
