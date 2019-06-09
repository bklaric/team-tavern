module TeamTavern.Client.EditProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (find, intercalate)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
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
import TeamTavern.Server.Profile.Update.SendResponse as Update
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { nickname :: String
    , handle :: String
    , title :: String
    , summary :: Array String
    , fieldValues :: Array
        { fieldId :: Int
        , url :: Maybe String
        , optionId :: Maybe Int
        , optionIds :: Maybe (Array Int)
        }
    , fields :: Array
        { id :: Int
        , type :: Int
        , label :: String
        , options :: Maybe (Array
            { id :: Int
            , option :: String
            })
        }
    }

data Action
    = SummaryInput String
    | UrlValueInput Int String
    | Update Event

data Message = ProfileUpdated String

type State =
    { nickname :: String
    , handle :: String
    , title :: String
    , fields :: Array
        { id :: Int
        , type :: Int
        , label :: String
        , options :: Maybe (Array
            { id :: Int
            , option :: String
            })
        }
    , summary :: String
    , summaryError :: Boolean
    , fieldValues :: Array
        { fieldId :: Int
        , url :: Maybe String
        , optionId :: Maybe Int
        , optionIds :: Maybe (Array Int)
        }
    , otherError :: Boolean
    }

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Message)

fieldInput fieldValues { id, type: 1, label } = let
    fieldValue' = fieldValues # find \{ fieldId } -> fieldId == id
    in
    case fieldValue' of
    Just { url } ->
        HH.div_
        [ HH.label [ HP.for label ] [ HH.text label ]
        , HH.input
            [ HP.id_ label
            , HE.onValueInput $ Just <<< UrlValueInput id
            , HP.value $ maybe "" identity url
            ]
        ]
    Nothing -> HH.div_ []
fieldInput _ _ = HH.div_ []

render :: forall slots. State -> HH.HTML slots Action
render { title, fields, summary, summaryError, fieldValues, otherError } = HH.form
    [ HP.class_ $ H.ClassName "single-form-wide", HE.onSubmit $ Just <<< Update ] $
    [ HH.h2_ [ HH.text $ "Edit your " <> title <> " profile" ] ]
    <> (fields <#> fieldInput fieldValues) <>
    [ HH.div_
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
updateProfile state @ { nickname, handle, summary, fieldValues } = Async.unify do
    response <- Fetch.fetch
        ("/api/profiles/single/" <> handle <> "/" <> nickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { summary, fieldValues }
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
handleAction (UrlValueInput fieldId url) = do
    state @ { fieldValues } <- H.get
    let newFieldValues = fieldValues <#> \value ->
        if value.fieldId == fieldId
            then value { url = Just url}
            else value
    H.put $ state { fieldValues = newFieldValues }
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
    { initialState: \{ handle, title, nickname, summary, fieldValues, fields } ->
        { handle
        , title
        , nickname
        , fields
        , summary: intercalate "\n\n" summary
        , summaryError: false
        , fieldValues: fields <#> (\field -> let
            fieldValue' = fieldValues # find \{ fieldId } -> fieldId == field.id
            in
            case fieldValue' of
            Nothing ->
                { fieldId: field.id
                , url: Nothing
                , optionId: Nothing
                , optionIds: Nothing
                }
            Just { url, optionId, optionIds } ->
                { fieldId: field.id
                , url
                , optionId
                , optionIds
                })
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
