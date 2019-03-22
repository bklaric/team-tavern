module TeamTavern.Client.CreateProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
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
import TeamTavern.Profile.Create.SendResponse as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = { handle :: String }

data Action
    = SummaryInput String
    | Create Event

data Message = ProfileCreated String

type State =
    { handle :: String
    , summary :: String
    , summaryError :: Boolean
    , otherError :: Boolean
    }

type Slot = H.Slot (Modal.Query Unit (Const Void)) (Modal.Message Message)

render :: forall slots. State -> HH.HTML slots Action
render { summary, summaryError, otherError } = HH.form
    [ HP.class_ $ ClassName "single-form", HE.onSubmit $ Just <<< Create ]
    [ HH.h2_ [ HH.text "Create a new profile" ]
    , HH.div_
        [ HH.label
            [ HP.for "summary" ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HE.onValueInput $ Just <<< SummaryInput
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
        [ HH.text "Create" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

sendCreateRequest :: forall left. State -> Async left (Maybe State)
sendCreateRequest state @ { handle, summary } = Async.unify do
    response <- Fetch.fetch
        ("/api/profiles/single/" <> handle)
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { summary }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Create.BadRequestContent) -> Just $ match
                    { invalidSummary: const $ state { summaryError = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall slots left.
    Action -> H.HalogenM State Action slots Message (Async left) Unit
handleAction (SummaryInput summary) =
    H.modify_ (_ { summary = summary }) $> unit
handleAction (Create event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { summaryError = false
        , otherError   = false
        })
    newState <- H.lift $ sendCreateRequest state
    case newState of
        Nothing -> H.raise $ ProfileCreated state.handle
        Just newState' -> H.put newState'
    pure unit

component :: forall query input left.
    String -> H.Component HH.HTML query input Message (Async left)
component handle = H.mkComponent
    { initialState: const
        { handle
        , summary: ""
        , summaryError: false
        , otherError: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

createProfile
    :: forall query children left
    .  String
    -> (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query
        (createProfile :: Slot Unit | children)
        (Async left)
createProfile handle handleMessage = HH.slot
    (SProxy :: SProxy "createProfile") unit
    (Modal.component $ component handle) unit handleMessage
