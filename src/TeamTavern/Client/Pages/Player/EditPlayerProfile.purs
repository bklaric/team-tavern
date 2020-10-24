module TeamTavern.Client.Pages.Player.EditPlayerProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import CSS as CSS
import Data.Array (any, foldl)
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
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.SingleSelect as SingleSelect
import TeamTavern.Client.Pages.Player.Types (Nickname)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Profile.UpdatePlayerProfile.SendResponse as Update
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Option =
    { key :: String
    , label :: String
    }

type Field =
    { key :: String
    , ilk :: Int
    , label :: String
    , icon :: String
    , required :: Boolean
    , domain :: Maybe String
    , options :: Maybe (Array Option)
    }

type FieldValue =
    { fieldKey :: String
    , url :: Maybe String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type Input =
    { nickname :: Nickname
    , handle :: String
    , title :: String
    , fields :: Array Field
    , fieldValues :: Array FieldValue
    , summary :: Array String
    , newOrReturning :: Boolean
    }

data Action
    = SummaryInput String
    | UrlValueInput String String
    | NewOrReturningInput Boolean
    | Update Event
    | Close

data Output = ProfileUpdated Nickname | CloseClicked

type State =
    { nickname :: Nickname
    , handle :: String
    , title :: String
    , fields :: Array Field
    , fieldValues :: Array FieldValue
    , summary :: String
    , newOrReturning :: Boolean
    , summaryError :: Boolean
    , urlValueErrors :: Array String
    , missingErrors :: Array String
    , otherError :: Boolean
    , submitting :: Boolean
    }

type ChildSlots =
    ( "singleSelectField" :: SingleSelect.Slot Option String
    , "multiSelectField" :: MultiSelect.Slot Option String
    )

type Slot = H.Slot (Const Void) (Modal.Output Output)

fieldLabel :: forall slots action.
    String -> String -> Boolean -> Maybe String -> HH.HTML slots action
fieldLabel label icon required domain =
    HH.label
        [ HP.class_ $ HH.ClassName "input-label" ] $
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
        then
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "input-primary-sublabel" ] [ HH.text "required" ]
            ]
        else
            [])

fieldInput
    :: forall left
    .  Array FieldValue
    -> Array String
    -> Array String
    -> Field
    -> H.ComponentHTML Action ChildSlots (Async left)
fieldInput fieldValues urlValueErrors missingErrors { key, ilk: 1, label, icon, required, domain: Just domain } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    urlError = urlValueErrors # any (_ == key)
    missingError = missingErrors # any (_ == key)
    url = fieldValue' >>= _.url
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon required (Just domain)
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
fieldInput fieldValues _ _ { key, ilk: 2, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon required Nothing
    , singleSelectIndexed (SProxy :: SProxy "singleSelectField") key
        { options
        , selected: fieldValue' >>= _.optionKey >>= \optionKey ->
            options # find \option -> optionKey == option.key
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , showFilter: Nothing
        }
    ]
fieldInput fieldValues _ _ { key, ilk: 3, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selectedOptionIds' = fieldValue' >>= _.optionKeys
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon required Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options: options <#> \option ->
            { option
            , selected: selectedOptionIds' # maybe false \selectedOptionIds ->
                selectedOptionIds # any (_ == option.key) }
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , showFilter: Nothing
        }
    ]
fieldInput _ _ _ _ = HH.div_ []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { title
    , fields
    , summary
    , summaryError
    , fieldValues
    , newOrReturning
    , urlValueErrors
    , missingErrors
    , otherError
    , submitting
    } =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $ HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update ] $
    [ HH.p [ HP.class_ $ HH.ClassName "form-subheading", HC.style $ CSS.marginBottom $ CSS.px 0.0 ]
        [ HH.text "Describe yourself as a player and let other players find you." ]
    , HH.h3 [ HP.class_ $ HH.ClassName "input-groups-heading" ]
        [ HH.text "Details" ]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ] $
        (fields <#> fieldInput fieldValues urlValueErrors missingErrors)
        <>
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ fieldLabel "New or returning player" "fas fa-book" false Nothing
            , HH.label
                [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                [ HH.input
                    [ HP.class_ $ HH.ClassName "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    , HP.checked newOrReturning
                    , HE.onChecked (Just <<< NewOrReturningInput)
                    ]
                , HH.text "I'm a new or returning player to the game."
                ]
            ]
        ]
    , HH.h3 [ HP.class_ $ HH.ClassName "input-groups-heading" ]
        [ HH.text "Ambitions" ]
    , HH.textarea
        [ HP.id_ "summary"
        , HP.class_ $ HH.ClassName "text-input"
        , HE.onValueInput $ Just <<< SummaryInput
        , HP.value summary
        ]
    , HH.label [ HP.class_ $ HH.ClassName "input-underlabel" ]
        [ HH.text """Why are you even playing this game, bruh? What do you want
            do get from it? Do you have any specific goals you want to achieve?"""
        ]
    , HH.p
        [ HP.class_ $ inputErrorClass summaryError ]
        [ HH.text "Ambitions text cannot be more than 2000 characters long." ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled submitting
        , HC.style $ CSS.marginTop $ CSS.px 21.0
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-edit button-icon" ] []
        , HH.text
            if submitting
            then "Editing profile..."
            else "Edit profile"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

data UpdateError
    = Other
    | Content
        { summary :: Boolean
        , url :: Array String
        , missing :: Array String
        }

updateProfile :: forall left.
    String -> Nickname -> String -> Array FieldValue -> Boolean -> Async left (Maybe UpdateError)
updateProfile handle nickname summary fieldValues newOrReturning = Async.unify do
    response <-
        Fetch.fetch
        ("/api/profiles/by-handle/" <> handle <> "/players/" <> nickname)
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
            , newOrReturning
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just Other)
    result <- case FetchRes.status response of
        204 -> pure Nothing
        400 ->  FetchRes.text response
            >>= JsonAsync.readJSON
            #   bimap
                (const $ Just Other)
                (\(content :: Update.BadRequestContent) -> Just $ Content $
                    match
                    { invalidProfile:
                        foldl
                        (\errors error ->
                            match
                            { invalidSummary: const $ errors { summary = true }
                            , invalidUrl: \{ fieldKey } ->
                                errors { url = Array.cons fieldKey errors.url }
                            , missing: \{ fieldKey } ->
                                errors { missing = Array.cons fieldKey errors.missing }
                            }
                            error
                        )
                        { summary: false, url: [], missing: [] }
                    }
                    content
                )
        _ -> pure $ Just Other
    pure result

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (SummaryInput summary) = do
    H.modify_ (_ { summary = summary })
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
handleAction (NewOrReturningInput newOrReturning) =
    H.modify_ (_ { newOrReturning = newOrReturning })
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
    result <- H.lift $ updateProfile state'.handle state'.nickname state'.summary state'.fieldValues state'.newOrReturning
    case result of
        Nothing -> H.raise $ ProfileUpdated state.nickname
        Just Other -> H.put $ state' { otherError = true, submitting = false }
        Just (Content errors) -> H.put $ state'
            { summaryError = errors.summary
            , urlValueErrors = errors.url
            , missingErrors = errors.missing
            , submitting = false
            }
handleAction Close = H.raise CloseClicked

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ handle, title, nickname, summary, fieldValues, fields, newOrReturning } ->
        { handle
        , title
        , nickname
        , fields
        , summary: intercalate "\n\n" summary
        , summaryError: false
        , fieldValues: fields <#> (\field ->
            case fieldValues # find \{ fieldKey } -> fieldKey == field.key of
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
        , newOrReturning
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
    .  Input
    -> (Modal.Output Output -> Maybe query)
    -> HH.ComponentHTML query (editProfile :: Slot Unit | children) (Async left)
editProfile input handleMessage = HH.slot
    (SProxy :: SProxy "editProfile") unit
    (Modal.component ("Edit your " <> input.title <> " profile") component) input handleMessage
