module TeamTavern.Client.Components.Account.EditAccount
    (Message(..), Slot, editAccount) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (sortBy)
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (any, find, foldl, intercalate)
import Data.HTTP.Method (Method(..))
import Data.Int (fromNumber)
import Data.JSDate (getDate, getFullYear, getMonth, now)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Options ((:=))
import Data.String (trim)
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.CheckboxInput (checkboxInput)
import TeamTavern.Client.Components.CheckboxInput as CheckboxInput
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.MultiSelect (multiSelect)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SingleSelect (singleSelect)
import TeamTavern.Client.Components.SingleSelect as SingleSelect
import TeamTavern.Client.Components.TextLineInput (textLineInput)
import TeamTavern.Client.Components.TextLineInput as TextLineInput
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Infrastructure.Countries (allCountries)
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Timezones (Timezone, allTimezones)
import TeamTavern.Server.Player.Update.SendResponse as Update
import TeamTavern.Server.Player.ViewAccount.SendResponse as ViewAccount
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | NicknameInput String
    | AboutInput String
    | NotifyInput Boolean
    | Update LoadedState Event
    | Close

data Message = AccountUpdated String | CloseClicked

type LoadedState =
    { originalNickname :: String
    , nickname :: String
    , discordTag :: Maybe String
    , birthday :: Maybe String
    , minDate :: String
    , maxDate :: String
    , languages :: Array String
    , country :: Maybe String
    , timezone :: Maybe String
    , hasMicrophone :: Boolean
    , about :: String
    , notify :: Boolean
    , nicknameError :: Boolean
    , discordTagError :: Boolean
    , aboutError :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

data State
    = Empty
    | Error
    | Loaded LoadedState

type ChildSlots =
    ( discordTagInput :: TextLineInput.Slot
    , birthdayInput :: TextLineInput.Slot
    , languageInput :: MultiSelect.Slot String Unit
    , countryInput :: SingleSelect.Slot String Unit
    , timezoneInput :: SingleSelect.Slot Timezone Unit
    , hasMicrophoneInput :: CheckboxInput.Slot
    )

type Slot = H.Slot (Modal.Query Unit (Const Void)) (Modal.Message Message) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render Error = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render (Loaded loadedState @
    { originalNickname
    , nickname
    , discordTag
    , birthday
    , minDate
    , maxDate
    , languages
    , country
    , timezone
    , hasMicrophone
    , about
    , notify
    , nicknameError
    , discordTagError
    , aboutError
    , nicknameTaken
    , otherError
    , submitting
    }) = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] $ pure $
    HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update loadedState ]
    [ closeButton Close
    , HH.h2  [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Edit your account" ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HE.onValueInput $ Just <<< NicknameInput
            , HP.value nickname
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text
                $ "The nickname can contain only alphanumeric characters and "
                <> "cannot be more than 40 characters long." ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text
                "This nickname is already taken, please pick another one." ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "discord-tag" ]
            [ HH.text "Discord tag" ]
        , textLineInput (SProxy :: SProxy "discordTagInput")
            { id: "discord-tag", value: maybe "" identity discordTag, type_: TextLineInput.Text }
        , HH.label
            [ HP.class_ $ HH.ClassName "input-underlabel" ]
            [ HH.text "Example: username#1234" ]
        , HH.p
            [ HP.class_ $ inputErrorClass discordTagError ]
            [ HH.text $ "This does not look like a valid Discord tag." ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "birthday" ]
            [ HH.text "Birthday" ]
        , textLineInput (SProxy :: SProxy "birthdayInput")
            { id: "birthday", value: maybe "" identity birthday, type_: TextLineInput.Date (Just minDate) (Just maxDate) }
        , HH.label
            [ HP.class_ $ HH.ClassName "input-underlabel" ]
            [ HH.text $ "Your birthday will be used to calculate your age "
                <> "and will not be shown to anyone."
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "language" ]
            [ HH.text "Language" ]
        , multiSelect (SProxy :: SProxy "languageInput")
            { options: allLanguages <#> \allLanguage ->
                { option: allLanguage
                , selected: languages # any (_ == allLanguage)
                }
            , labeler: identity
            , comparer: (==)
            , showFilter: Just "Search languages"
            }
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "country" ]
            [ HH.text "Country" ]
        , singleSelect (SProxy :: SProxy "countryInput")
            { options: allCountries
            , selected: country
            , labeler: identity
            , comparer: (==)
            , showFilter: Just "Search countries"
            }
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "timezone" ]
            [ HH.text "Timezone" ]
        , singleSelect (SProxy :: SProxy "timezoneInput")
            { options: allTimezones # sortBy \leftTimezone rightTimezone -> let
                countryComparison = leftTimezone.country `compare` rightTimezone.country
                in
                case countryComparison of
                EQ -> leftTimezone.city `compare` rightTimezone.city
                other -> other
            , selected: timezone >>= \timezone' -> allTimezones # find (_.name >>> (_ == timezone'))
            , labeler: \{ city, country: country' } -> country' <> ", " <> city
            , comparer: \leftTimezone rightTimezone -> leftTimezone.name == rightTimezone.name
            , showFilter: Just "Search timezones"
            }
        ]
   , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "checkbox-input-label", HP.for "has-microphone" ]
            [ checkboxInput (SProxy :: SProxy "hasMicrophoneInput")
                { id: "has-microphone", value: hasMicrophone }
            , HH.text "I have a microphone and I'm willing to communicate."
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "checkbox-input-label", HP.for "notify" ]
            [ HH.input
                [ HP.id_ "notify"
                , HP.class_ $ HH.ClassName "checkbox-input"
                , HP.type_ HP.InputCheckbox
                , HE.onChecked $ Just <<< NotifyInput
                , HP.checked notify
                ]
            , HH.text "Send me an email when someone messages me."
            ]
        ]
    -- , HH.div_
    --     [ HH.label
    --         [ HP.for "about" ]
    --         [ HH.text "About" ]
    --     , HH.textarea
    --         [ HP.id_ "about"
    --         , HE.onValueInput $ Just <<< AboutInput
    --         , HP.value about
    --         ]
    --     , HH.p
    --         [ HP.class_ $ inputErrorClass aboutError ]
    --         [ HH.text
    --             "The about entry cannot be more than 2000 characters long." ]
    --     ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled $ nickname == "" || submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text
            if submitting
            then "Editting account..."
            else "Edit account"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

loadAccount :: forall left. String -> Async left State
loadAccount nickname = Async.unify do
    response <- Fetch.fetch
        ("/api/players/by-nickname/" <> nickname <> "/account")
        (Fetch.credentials := Fetch.Include)
        # lmap (const Error)
    content :: ViewAccount.OkContent <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Error)
        _ -> Async.left Error
    currentDate <- H.liftEffect now
    currentDay <- H.liftEffect $ unsafePartial fromJust <$> fromNumber <$> getDate currentDate
    currentMonth <- H.liftEffect $ unsafePartial fromJust <$> fromNumber <$> getMonth currentDate
    currentYear <- H.liftEffect $ unsafePartial fromJust <$> fromNumber <$> getFullYear currentDate
    let thirteenYearsAgo =
            show (currentYear - 13) <> "-"
            <> (if currentMonth + 1 < 10 then "0" <> show (currentMonth + 1) else show (currentMonth + 1)) <> "-"
            <> (if currentDay < 10 then "0" <> show currentDay else show currentDay)
    pure $ Loaded
        { originalNickname: content.nickname
        , nickname: content.nickname
        , discordTag: content.discordTag
        , birthday: content.birthday
        , minDate: "1900-01-01"
        , maxDate: thirteenYearsAgo
        , languages: content.languages
        , country: content.country
        , timezone: content.timezone
        , hasMicrophone: content.hasMicrophone
        , about: intercalate "\n\n" content.about
        , notify: content.notify
        , nicknameError: false
        , discordTagError: false
        , aboutError: false
        , nicknameTaken: false
        , otherError: false
        , submitting: false
        }

updateAccount :: forall left. LoadedState -> Async left (Maybe LoadedState)
updateAccount state @
    { originalNickname
    , nickname
    , discordTag
    , birthday
    , languages
    , country
    , timezone
    , hasMicrophone
    , about
    , notify
    } = Async.unify do
    response <- Fetch.fetch ("/api/players/by-nickname/" <> originalNickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON
            { nickname
            , discordTag
            , birthday
            , languages
            , country
            , timezone
            , hasMicrophone
            , about
            , notify
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidIdentifiers: foldl (\state' -> match
                        { invalidNickname:
                            const $ state' { nicknameError = true }
                        , invalidDiscordTag:
                            const $ state' { discordTagError = true }
                        , invalidAbout:
                            const $ state' { aboutError = true }
                        })
                        state
                    , nicknameTaken: const $ state { nicknameTaken = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Message (Async left) Unit
handleAction Init = do
    playerInfo <- H.liftEffect getPlayerInfo
    case playerInfo of
        Nothing -> H.liftEffect $ navigate_ "/"
        Just { nickname } -> do
            state <- H.lift $ loadAccount nickname
            H.put state
handleAction (NicknameInput nickname) =
    H.modify_ case _ of
        Loaded state -> Loaded $ state { nickname = nickname }
        state -> state
handleAction (AboutInput about) = do
    H.modify_ case _ of
        Loaded state -> Loaded $ state { about = about }
        state -> state
handleAction (NotifyInput notify) = do
    H.modify_ case _ of
        Loaded state -> Loaded $ state { notify = notify }
        state -> state
handleAction (Update loadedState event) = do
    H.liftEffect $ preventDefault event
    discordTag <- H.query (SProxy :: SProxy "discordTagInput") unit
        (TextLineInput.GetValue identity)
    birthday <- H.query (SProxy :: SProxy "birthdayInput") unit
        (TextLineInput.GetValue identity)
    languages <- H.query (SProxy :: SProxy "languageInput") unit
        (MultiSelect.Selected identity)
    country <- H.query (SProxy :: SProxy "countryInput") unit
        (SingleSelect.Selected identity)
    timezone <- H.query (SProxy :: SProxy "timezoneInput") unit
        (SingleSelect.Selected $ map _.name)
    hasMicrophone <- H.query (SProxy :: SProxy "hasMicrophoneInput") unit
        (CheckboxInput.GetValue identity)
    let resetState = loadedState
            { discordTag =
                discordTag >>=
                case _ of
                "" -> Nothing
                discordTag' -> Just discordTag'
            , birthday =
                birthday >>=
                case _ of
                "" -> Nothing
                birthday' -> Just birthday'
            , languages       = maybe [] identity languages
            , country         = join country
            , timezone        = join timezone
            , hasMicrophone   = maybe false identity hasMicrophone
            , nicknameError   = false
            , discordTagError = false
            , aboutError      = false
            , nicknameTaken   = false
            , otherError      = false
            , submitting      = true
            }
    H.put $ Loaded resetState
    newState <- H.lift $ updateAccount resetState
    case newState of
        Nothing -> H.raise $ AccountUpdated $ trim loadedState.nickname
        Just newState' -> H.put $ Loaded newState' { submitting = false }
handleAction Close = H.raise CloseClicked

component :: forall query input left.
    H.Component HH.HTML query input Message (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

editAccount
    :: forall query children left
    .  (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query (editAccount :: Slot | children) (Async left)
editAccount handleMessage = HH.slot
    (SProxy :: SProxy "editAccount") unit
    (Modal.component component) unit handleMessage
