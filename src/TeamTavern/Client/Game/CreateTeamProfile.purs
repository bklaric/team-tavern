module TeamTavern.Client.Game.CreateTeamProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (foldl)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Int as Int
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
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.ModalDeclarative as Modal
import TeamTavern.Client.Components.SelectDeclarative.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.SelectDeclarative.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDeclarative.MultiSelect2 as MultiSelect2
import TeamTavern.Client.Components.SelectDeclarative.TreeSelect (treeSelect)
import TeamTavern.Client.Components.SelectDeclarative.TreeSelect as TreeSelect
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Game.View.SendResponse as ViewGame
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import TeamTavern.Server.Profile.AddGamePlayer.SendResponse as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Option =
    { key :: String
    , label :: String
    }

type Field =
    { key :: String
    , label :: String
    , icon :: String
    , options :: Array Option
    }

data FieldValue = FieldValue Field (MultiSelect.Input Option)

type Input =
    { game :: ViewGame.OkContent
    , player :: PlayerInfo
    }

type State =
    { game :: ViewGame.OkContent
    , player :: PlayerInfo
    , summary :: String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , languages :: Array String
    , regions :: Array String
    , hasMicrophone :: Boolean
    , fieldValues :: Array FieldValue
    , summaryError :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = SummaryInput String
    | AgeFromInput String
    | AgeToInput String
    | LanguageInput (MultiSelect2.Output String)
    | RegionInput (TreeSelect.Output String)
    | MicrophoneInput Boolean
    | FieldValueInput String (Array (MultiSelect.InputEntry Option))
    | Create Event
    | Close

data Output = ProfileCreated | CloseClicked

type Slot = H.Slot (Const Void) (Modal.Output Output) Unit

type ChildSlots =
    ( "language" :: MultiSelect2.Slot String Unit
    , "country" :: TreeSelect.Slot String
    , "multiSelectField" :: MultiSelect.Slot Option String
    )

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
        then []
        else
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text "optional" ]
            ])

inputLabel :: forall action slots. String -> String -> HH.HTML slots action
inputLabel label icon = fieldLabel label icon false Nothing

fieldInput :: forall left.
    FieldValue -> H.ComponentHTML Action ChildSlots (Async left)
fieldInput (FieldValue { key, label, icon } input) =
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon false Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key input
        case _ of
        MultiSelect.SelectedChanged entries -> Just $ FieldValueInput key entries
        MultiSelect.FilterChanged text -> Nothing
    ]

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { game
    , summary
    , fieldValues
    , summaryError
    , otherError
    , submitting
    } =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $
    Array.singleton $
    HH.form
    [ HP.class_ $ ClassName "form", HE.onSubmit $ Just <<< Create ] $
    [ closeButton Close
    , HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text $ "Create your " <> game.title <> " profile" ]
    , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
        [ HH.text "Describe players you are looking for and let them find your team" ]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ] $
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ inputLabel "Age" "fas fa-calendar-alt"
            , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputNumber
                    , HE.onValueChange $ Just <<< AgeFromInput
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputNumber
                    , HE.onValueChange $ Just <<< AgeToInput
                    ]
                ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ inputLabel "Language" "fas fa-comments"
            , MultiSelect2.multiSelect (SProxy :: SProxy "language")
                { entries: allLanguages <#> { option: _, selected: false }
                , labeler: identity
                , comparer: (==)
                , filter: Just "Search languages"
                }
                (Just <<< LanguageInput)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ inputLabel "Country" "fas fa-globe-europe"
            , treeSelect (SProxy :: SProxy "country")
                { entries: allRegions <#> regionToEntry
                , labeler: identity
                , comparer: (==)
                , placeHolder: "Search countries"
                }
                (Just <<< RegionInput)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ inputLabel "Microphone" "fas fa-microphone"
            , HH.label
                [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                [ HH.input
                    [ HP.class_ $ HH.ClassName "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    , HE.onChecked $ Just <<< MicrophoneInput
                    ]
                , HH.text "Must have a microphone and be willing to communicate."
                ]
            ]
        ]
        <>
        (fieldValues <#> fieldInput)
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "summary" ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HP.class_ $ HH.ClassName "text-input"
            , HE.onValueInput $ Just <<< SummaryInput
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
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
        , HH.text
            if submitting
            then "Creating team profile..."
            else "Create team profile"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

data CreateError
    = Other
    | Content { summary :: Boolean }

sendCreateRequest :: forall left.
    State -> Async left (Maybe CreateError)
sendCreateRequest state @ { game, player } = Async.unify do
    response <-
        Fetch.fetch
        (  "/api/profiles/by-handle/" <> game.handle
        <> "/teams/" <> player.nickname
        )
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON
            { summary: state.summary
            , ageFrom: state.ageFrom
            , ageTo: state.ageTo
            , languages: state.languages
            , regions: state.regions
            , hasMicrophone: state.hasMicrophone
            , fieldValues: state.fieldValues # Array.mapMaybe
                case _ of
                FieldValue field input
                | entries <- Array.filter _.selected input.entries
                , not $ Array.null entries -> Just
                    { fieldKey: field.key
                    , optionKeys: entries <#> (_.option >>> _.key)
                    }
                _ -> Nothing
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just Other)
    result <-
        case FetchRes.status response of
        204 -> pure Nothing
        400 ->
            FetchRes.text response
            >>= JsonAsync.readJSON
            # bimap
                (const $ Just Other)
                (\(error :: Create.BadRequestContent) -> Just $ Content $
                    match
                    { invalidProfile:
                        foldl
                        (\errors error' ->
                            error' # match
                                { invalidSummary: const $
                                    errors { summary = true }
                                , invalidUrl: const errors
                                , missing: const errors
                                }
                        )
                        ({ summary: false })
                    }
                    error)
        _ -> pure $ Just Other
    pure result

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (SummaryInput summary) =
    H.modify_ (_ { summary = summary })
handleAction (AgeFromInput ageFrom) =
    H.modify_ (_ { ageFrom = Int.fromString ageFrom })
handleAction (AgeToInput ageTo) =
    H.modify_ (_ { ageTo = Int.fromString ageTo })
handleAction (LanguageInput (MultiSelect2.SelectedChanged languages)) =
    H.modify_ (_ { languages = languages })
handleAction (RegionInput (TreeSelect.SelectedChanged regions)) =
    H.modify_ (_ { regions = regions })
handleAction (MicrophoneInput hasMicrophone) =
    H.modify_ (_ { hasMicrophone = hasMicrophone })
handleAction (FieldValueInput fieldKey entries) =
    H.modify_ \state -> state
        { fieldValues = state.fieldValues <#>
            case _ of
            FieldValue field input | field.key == fieldKey ->
                FieldValue field input { entries = entries }
            other -> other
        }
handleAction (Create event) = do
    H.liftEffect $ preventDefault event
    resetState <-
        H.modify (\state -> state
            { summaryError = false
            , otherError   = false
            , submitting   = true
            })
    result <- H.lift $
        sendCreateRequest resetState
    case result of
        Nothing -> H.raise ProfileCreated
        Just Other -> H.put $ resetState
            { otherError = true
            , submitting = false
            }
        Just (Content errors) -> H.put $ resetState
            { summaryError = errors.summary
            , submitting = false
            }
handleAction Close = H.raise CloseClicked

regionToEntry :: Region -> TreeSelect.InputEntry String
regionToEntry (Region region subRegions) = TreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> regionToEntry
    }

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ game, player } ->
        { game
        , player
        , summary: ""
        , ageFrom: Nothing
        , ageTo: Nothing
        , languages: []
        , regions: []
        , hasMicrophone: false
        , fieldValues:
            game.fields
            <#> (\field ->
                case field.ilk of
                2 | Just options <- field.options -> Just $ FieldValue
                    { key: field.key
                    , label: field.label
                    , icon: field.icon
                    , options
                    }
                    { entries: options <#> \option ->
                        { option, selected: false }
                    , labeler: _.label
                    , comparer: \leftOption rightOption ->
                        leftOption.key == rightOption.key
                    , filter: Nothing
                    }
                3 | Just options <- field.options -> Just $ FieldValue
                    { key: field.key
                    , label: field.label
                    , icon: field.icon
                    , options
                    }
                    { entries: options <#> \option ->
                        { option, selected: false }
                    , labeler: _.label
                    , comparer: \leftOption rightOption ->
                        leftOption.key == rightOption.key
                    , filter: Nothing
                    }
                _ -> Nothing)
            # Array.catMaybes
        , summaryError: false
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

createTeamProfile
    :: forall children action left
    .  Input
    -> (Modal.Output Output -> Maybe action)
    -> HH.ComponentHTML action
        (createTeamProfile :: Slot | children) (Async left)
createTeamProfile input handleMessage =
    HH.slot (SProxy :: SProxy "createTeamProfile") unit
    (Modal.component component) input handleMessage
