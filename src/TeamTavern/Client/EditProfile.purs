module TeamTavern.Client.EditProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (any, foldl)
import Data.Array as Arary
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (find, intercalate)
import Data.HTTP.Method (Method(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Options ((:=))
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.SingleSelect as SingleSelect
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Profile.Update.SendResponse as Update
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data ProfileIlk = Players | Teams

type Input =
    { nickname :: String
    , handle :: String
    , title :: String
    , profileIlk :: ProfileIlk
    , summary :: Array String
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , fields :: Array
        { key :: String
        , type :: Int
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        }
    }

data Action
    = SummaryInput String
    | UrlValueInput String String
    | Update Event
    | Close

data Message = ProfileUpdated String ProfileIlk | CloseClicked

type State =
    { nickname :: String
    , handle :: String
    , title :: String
    , profileIlk :: ProfileIlk
    , fields :: Array
        { key :: String
        , type :: Int
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        }
    , summary :: String
    , summaryError :: Boolean
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , urlValueErrors :: Array { fieldKey :: String }
    , missingErrors :: Array { fieldKey :: String }
    , otherError :: Boolean
    , submitting :: Boolean
    }

type ChildSlots =
    ( "singleSelectField" :: SingleSelect.Slot { key :: String, option :: String } String
    , "multiSelectField" :: MultiSelect.Slot { key :: String, option :: String } String
    )

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Message)

fieldLabel :: forall slots action.
    String -> String -> String -> Boolean -> Maybe String -> HH.HTML slots action
fieldLabel key label icon required domain =
    HH.label
        [ HP.class_ $ HH.ClassName "input-label", HP.for key ] $
        [ HH.i [ HP.class_ $ HH.ClassName $ icon <> " filter-field-icon" ] []
        , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text label ]
        ]
        <>
        (case domain of
        Just domain' ->
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text domain' ]
            ]
        Nothing -> [])
        <>
        (if required
        then []
        else
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text "optional" ]
            ])

fieldInput
    :: forall left
    .  ProfileIlk
    -> Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    -> Array { fieldKey :: String }
    -> Array { fieldKey :: String }
    ->  { type :: Int
        , key :: String
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array { key :: String , option :: String })
        }
    -> H.ComponentHTML Action ChildSlots (Async left)
fieldInput Players fieldValues urlValueErrors missingErrors { key, type: 1, label, icon, required, domain: Just domain } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    urlError = urlValueErrors # any (_.fieldKey >>> (_ == key))
    missingError = missingErrors # any (_.fieldKey >>> (_ == key))
    url = fieldValue' >>= _.url
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon required (Just domain)
    , HH.input
        [ HP.id_ key
        , HP.class_ $ HH.ClassName "text-line-input"
        , HE.onValueInput $ Just <<< UrlValueInput key
        , HP.value $ maybe "" identity url
        ]
    , HH.p
        [ HP.class_ $ inputErrorClass urlError ]
        [ HH.text $ "This doesn't look like a valid " <> label <> " (" <> domain <> ") address." ]
    , HH.p
        [ HP.class_ $ inputErrorClass missingError ]
        [ HH.text $ label <> " is required." ]
    ]
fieldInput Players fieldValues _ _ { key, type: 2, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon required Nothing
    , singleSelectIndexed (SProxy :: SProxy "singleSelectField") key
        { options
        , selected: fieldValue' >>= _.optionKey >>= \optionKey ->
            options # find \option -> optionKey == option.key
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        }
    ]
fieldInput Teams fieldValues _ _ { key, type: 2, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selectedOptionIds' = fieldValue' >>= _.optionKeys
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon required Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options: options <#> \option ->
            { option
            , selected: selectedOptionIds' # maybe false \selectedOptionIds ->
                selectedOptionIds # any (_ == option.key) }
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        }
    ]
fieldInput Players fieldValues _ _ { key, type: 3, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selectedOptionIds' = fieldValue' >>= _.optionKeys
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon required Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options: options <#> \option ->
            { option
            , selected: selectedOptionIds' # maybe false \selectedOptionIds ->
                selectedOptionIds # any (_ == option.key) }
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        }
    ]
fieldInput Teams fieldValues _ _ { key, type: 3, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selectedOptionIds' = fieldValue' >>= _.optionKeys
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon required Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options: options <#> \option ->
            { option
            , selected: selectedOptionIds' # maybe false \selectedOptionIds ->
                selectedOptionIds # any (_ == option.key) }
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        }
    ]
fieldInput _ _ _ _ _ = HH.div_ []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { title
    , profileIlk
    , fields
    , summary
    , summaryError
    , fieldValues
    , urlValueErrors
    , missingErrors
    , otherError
    , submitting
    } =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $ HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update ] $
    [ closeButton Close
    , HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text
            case profileIlk of
            Players -> "Edit your " <> title <> " profile"
            Teams -> "Edit your " <> title <> " team profile"
        ]
    , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
        [ HH.text
            case profileIlk of
            Players -> "Describe yourself as a player and let other players find you."
            Teams -> "Describe players you are looking for and let them find your team."
        ]
    ]
    <> (fields <#> fieldInput profileIlk fieldValues urlValueErrors missingErrors) <>
    [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label"
            , HP.for "summary"
            ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HP.class_ $ HH.ClassName "text-input"
            , HE.onValueInput $ Just <<< SummaryInput
            , HP.value summary
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text
                "The summary cannot be empty and cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled $ summary == "" || submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-edit button-icon" ] []
        , HH.text
            if submitting
            then "Editting profile..."
            else "Edit profile"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

updateProfile :: forall left. State -> Async left (Maybe State)
updateProfile state @ { nickname, handle, profileIlk, summary, fieldValues } = Async.unify do
    response <- Fetch.fetch
        ("/api/profiles/single/" <> handle <> "/" <> nickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON
            { summary
            , fieldValues: fieldValues # Array.filter
                \{ url, optionKey, optionKeys } ->
                    isJust url
                    || isJust optionKey
                    || (case optionKeys of
                        Just optionKeys' | not $ Array.null optionKeys' -> true
                        _ -> false)
            , type:
                case profileIlk of
                Players -> 1
                Teams -> 2
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidProfile: foldl (\state' error' ->
                        error' # match
                            { invalidSummary: const $ state' { summaryError = true }
                            , invalidUrl: \{ fieldKey, errors } ->
                                state' { urlValueErrors = Arary.cons { fieldKey } state'.urlValueErrors }
                            , missing: \{ fieldKey } ->
                                state' { missingErrors = Arary.cons { fieldKey } state'.missingErrors }
                            })
                        state
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Message (Async left) Unit
handleAction (SummaryInput summary) = do
    H.modify_ (_ { summary = summary }) $> unit
handleAction (UrlValueInput fieldKey inputUrl) = do
    state @ { fields, fieldValues } <- H.get
    let allFieldValues = fields <#> (\field -> let
            fieldValue' = fieldValues # find \fieldValue -> fieldValue.fieldKey == field.key
            in
            case fieldValue' of
            Nothing ->
                { fieldKey: field.key
                , url: Nothing
                , optionKey: Nothing
                , optionKeys: Nothing
                }
            Just { url, optionKey, optionKeys } ->
                { fieldKey: field.key
                , url
                , optionKey
                , optionKeys
                })
    let newFieldValues = allFieldValues <#> \value ->
            if value.fieldKey == fieldKey
                then
                    if null inputUrl
                    then value { url = Nothing }
                    else value { url = Just inputUrl }
                else value
    H.put $ state { fieldValues = newFieldValues }
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    state @ { fields } <- H.gets (_
        { summaryError   = false
        , otherError     = false
        , urlValueErrors = []
        , missingErrors  = []
        , submitting     = true
        })
    H.put state
    singleSelectResults <-
        (H.queryAll (SProxy :: SProxy "singleSelectField")
        $ SingleSelect.Selected identity)
    let (singleSelectValues :: Array _) =
            singleSelectResults
            # Map.toUnfoldable
            <#> \(Tuple fieldKey option) ->
                { fieldKey
                , url: Nothing
                , optionKey: option <#> _.key
                , optionKeys: Nothing
                }
    multiSelectResults <-
        (H.queryAll (SProxy :: SProxy "multiSelectField")
        $ MultiSelect.Selected identity)
    let (multiSelectValues :: Array _) =
            multiSelectResults
            # Map.toUnfoldable
            <#> \(Tuple fieldKey options) ->
                { fieldKey
                , url: Nothing
                , optionKey: Nothing
                , optionKeys: Just $ options <#> _.key
                }
    let state' = state
            { fieldValues =
                (state.fieldValues # Array.filter (_.url >>> isJust))
                <> singleSelectValues
                <> multiSelectValues
            }
    newState <- H.lift $ updateProfile state'
    case newState of
        Nothing -> H.raise $ ProfileUpdated state.nickname state.profileIlk
        Just newState' -> H.put newState' { submitting = false }
handleAction Close = H.raise CloseClicked

component :: forall query left.
    H.Component HH.HTML query Input Message (Async left)
component = H.mkComponent
    { initialState: \{ handle, title, nickname, profileIlk, summary, fieldValues, fields } ->
        { handle
        , title
        , nickname
        , profileIlk
        , fields
        , summary: intercalate "\n\n" summary
        , summaryError: false
        , fieldValues: fields <#> (\field -> let
            fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == field.key
            in
            case fieldValue' of
            Nothing ->
                { fieldKey: field.key
                , url: Nothing
                , optionKey: Nothing
                , optionKeys: Nothing
                }
            Just { url, optionKey, optionKeys } ->
                { fieldKey: field.key
                , url
                , optionKey
                , optionKeys
                })
        , urlValueErrors: []
        , missingErrors: []
        , otherError: false
        , submitting: false
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
