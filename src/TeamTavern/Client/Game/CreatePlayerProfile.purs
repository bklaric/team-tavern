module TeamTavern.Client.Game.CreatePlayerProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (foldl)
import Data.Array as Arary
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String as String
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
import TeamTavern.Client.Components.SelectDeclarative.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.SelectDeclarative.SingleSelect as SingleSelect
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Game.View.SendResponse as ViewGame
import TeamTavern.Server.Profile.Create.SendResponse as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Field other =
    { key :: String
    , label :: String
    , icon :: String
    | other
    }

type UrlValue = String

type UrlField = Field
    ( domain :: String
    , required :: Boolean
    )

type Option =
    { key :: String
    , label :: String
    }

type SelectField = Field
    ( options :: Array Option )

type UrlError = Boolean

type MissingError = Boolean

data FieldValue
    = Url UrlField UrlValue UrlError MissingError
    | Single SelectField (SingleSelect.Input Option)
    | Multi SelectField (MultiSelect.Input Option)

type Input =
    { game :: ViewGame.OkContent
    , player :: PlayerInfo
    }

type State =
    { game :: ViewGame.OkContent
    , player :: PlayerInfo
    , summary :: String
    , fieldValues :: Array FieldValue
    , summaryError :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = SummaryInput String
    | UrlValueInput String UrlValue
    | SingleValueInput String (Maybe Option)
    | MultiValueInput String (Array (MultiSelect.InputEntry Option))
    | Create Event
    | Close

data Output = ProfileCreated | CloseClicked

type Slot = H.Slot (Const Void) (Modal.Output Output) Unit

type ChildSlots =
    ( "singleSelectField" :: SingleSelect.Slot Option String
    , "multiSelectField" :: MultiSelect.Slot Option String
    )

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

fieldInput :: forall left.
    FieldValue ->  H.ComponentHTML Action ChildSlots (Async left)
fieldInput (Url { key, label, icon, required, domain } url urlError missingError) =
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon required (Just domain)
    , HH.input
        [ HP.id_ key
        , HP.class_ $ HH.ClassName "text-line-input"
        , HP.value url
        , HE.onValueInput $ Just <<< UrlValueInput key
        ]
    , HH.p
        [ HP.class_ $ inputErrorClass urlError ]
        [ HH.text $ "This doesn't look like a valid " <> label <> " (" <> domain <> ") address." ]
    , HH.p
        [ HP.class_ $ inputErrorClass missingError ]
        [ HH.text $ label <> " is required." ]
    ]
fieldInput (Single { key, label, icon } input) =
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon false Nothing
    , singleSelectIndexed (SProxy :: SProxy "singleSelectField") key input
        case _ of
        SingleSelect.SelectedChanged selected -> Just $ SingleValueInput key selected
        SingleSelect.FilterChanged text -> Nothing
    ]
fieldInput (Multi { key, label, icon } input) =
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel key label icon false Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key input
        case _ of
        MultiSelect.SelectedChanged entries -> Just $ MultiValueInput key entries
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
        [ HH.text "Describe yourself as a player and let other players find you." ]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ]
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
            then "Creating player profile..."
            else "Create player profile"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

data CreateError
    = Other
    | Content
        { summary :: Boolean
        , url :: Array String
        , missing :: Array String
        }

sendCreateRequest :: forall left.
    String -> String -> String -> Array FieldValue -> Async left (Maybe CreateError)
sendCreateRequest handle nickname summary fieldValues = Async.unify do
    response <-
        Fetch.fetch
        ("/api/profiles/by-handle/" <> handle <> "/players/" <> nickname)
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON
            { summary
            , fieldValues: fieldValues <#>
                case _ of
                Url field url _ _ | not $ String.null url -> Just
                    { fieldKey: field.key
                    , url: Just url
                    , optionKey: Nothing
                    , optionKeys: Nothing
                    }
                Single field input | Just selected <- input.selected -> Just
                    { fieldKey: field.key
                    , url: Nothing
                    , optionKey: Just selected.key
                    , optionKeys: Nothing
                    }
                Multi field input | not $ Array.null input.entries -> Just
                    { fieldKey: field.key
                    , url: Nothing
                    , optionKey: Nothing
                    , optionKeys: Just $ input.entries <#> (_.option >>> _.key)
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
                                , invalidUrl: \{ fieldKey } ->
                                    errors { url = Arary.cons fieldKey errors.url }
                                , missing: \{ fieldKey } ->
                                    errors { missing = Arary.cons fieldKey errors.missing }
                                }
                        )
                        ({ summary: false, url: [], missing: [] })
                    }
                    error)
        _ -> pure $ Just Other
    pure result

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (SummaryInput summary) =
    H.modify_ (_ { summary = summary })
handleAction (UrlValueInput fieldKey url) =
    H.modify_ \state -> state
        { fieldValues = state.fieldValues <#>
            case _ of
            Url field _ urlError missingError | field.key == fieldKey ->
                Url field url urlError missingError
            other -> other
        }
handleAction (SingleValueInput fieldKey selected) =
    H.modify_ \state -> state
        { fieldValues = state.fieldValues <#>
            case _ of
            Single field input | field.key == fieldKey ->
                Single field input { selected = selected }
            other -> other
        }
handleAction (MultiValueInput fieldKey entries) =
    H.modify_ \state -> state
        { fieldValues = state.fieldValues <#>
            case _ of
            Multi field input | field.key == fieldKey ->
                Multi field input { entries = entries }
            other -> other
        }
handleAction (Create event) = do
    H.liftEffect $ preventDefault event
    resetState @ { game, player, summary, fieldValues } <-
        H.modify (\state -> state
            { summaryError = false
            , otherError   = false
            , submitting   = true
            , fieldValues  = state.fieldValues <#>
                case _ of
                Url field url _ _ -> Url field url false false
                other -> other
            })
    result <- H.lift $
        sendCreateRequest game.handle player.nickname summary fieldValues
    case result of
        Nothing -> H.raise ProfileCreated
        Just Other -> H.put $ resetState
            { otherError = true
            , submitting = false
            }
        Just (Content errors) -> H.put $ resetState
            { summaryError = errors.summary
            , fieldValues = resetState.fieldValues <#>
                case _ of
                Url field url _ _ -> Url field url
                    (errors.url # Array.any (_ == field.key))
                    (errors.missing # Array.any (_ == field.key))
                other -> other
            , submitting = false
            }
handleAction Close = H.raise $ CloseClicked

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ game, player } ->
        { game
        , player
        , summary: ""
        , fieldValues:
            game.fields
            <#> (\field ->
                case field.ilk of
                1 | Just domain <- field.domain -> Just $ Url
                    { key: field.key
                    , label: field.label
                    , icon: field.icon
                    , required: field.required
                    , domain
                    }
                    ""
                    false
                    false
                2 | Just options <- field.options -> Just $ Single
                    { key: field.key
                    , label: field.label
                    , icon: field.icon
                    , options
                    }
                    { options
                    , selected: Nothing
                    , labeler: _.label
                    , comparer: \leftOption rightOption ->
                        leftOption.key == rightOption.key
                    , filter: Nothing
                    }
                3 | Just options <- field.options -> Just $ Multi
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

createPlayerProfile
    :: forall children action left
    .  Input
    -> (Modal.Output Output -> Maybe action)
    -> HH.ComponentHTML action
        (createPlayerProfile :: Slot | children) (Async left)
createPlayerProfile input handleMessage =
    HH.slot (SProxy :: SProxy "createPlayerProfile") unit
    (Modal.component component) input handleMessage
