module TeamTavern.Client.Game.PlayerProfiles (Fields, PlayerProfileRow, InputPlayerProfile, Input, Message(..), Slot, playerProfiles) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (find, foldr, intercalate)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record.Builder as Record
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (pageSize)

type Fields = Array
    { ilk :: Int
    , label :: String
    , key :: String
    , icon :: String
    , required :: Boolean
    , domain :: Maybe String
    , options :: Maybe (Array
        { key :: String
        , label :: String
        })
    }

type PlayerProfileRow other =
    ( nickname :: String
    , discordTag :: Maybe String
    , age :: Maybe Int
    , country :: Maybe String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    , fieldValues :: Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , url :: Maybe String
        , option :: Maybe
            { key :: String
            , label :: String
            }
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    , summary :: Array String
    , newOrReturning :: Boolean
    , updated :: String
    , updatedSeconds :: Number
    | other)

type InputPlayerProfile = Record (PlayerProfileRow ())

type Input =
    { fields :: Fields
    , profiles :: Array InputPlayerProfile
    , profileCount :: Int
    , showCreateProfile :: Boolean
    , playerInfo :: Maybe PlayerInfo
    , page :: Int
    }

type StatePlayerProfile = Record (PlayerProfileRow (discordTagCopied :: Boolean))

type State =
    { fields :: Fields
    , profiles :: Array StatePlayerProfile
    , profileCount :: Int
    , showCreateProfile :: Boolean
    , playerInfo :: Maybe PlayerInfo
    , page :: Int
    }

data Action
    = Receive Input
    | ChangePageAction Int
    | CreateProfileAction
    | CopyDiscordTag String String

data Message
    = CreateProfile
    | ChangePage Int

type Slot = H.Slot (Const Void) Message Unit

type ChildSlots =
    ( players :: Anchor.Slot Int
    , messagePlayer :: Anchor.Slot Int
    )

yearSeconds :: Number
yearSeconds = 60.0 * 60.0 * 24.0 * 365.0

monthSeconds :: Number
monthSeconds = 60.0 * 60.0 * 24.0 * 30.0

daySeconds :: Number
daySeconds = 60.0 * 60.0 * 24.0

hourSeconds :: Number
hourSeconds = 60.0 * 60.0

minuteSeconds :: Number
minuteSeconds = 60.0

lastUpdated :: Number -> String
lastUpdated updatedSeconds = let
    yearsAgo = floor(updatedSeconds / yearSeconds)
    monthsAgo = floor(updatedSeconds / monthSeconds)
    daysAgo = floor(updatedSeconds / daySeconds)
    hoursAgo = floor(updatedSeconds / hourSeconds)
    minutesAgo = floor(updatedSeconds / minuteSeconds)
    interval =
        if yearsAgo > 0 then Just { unit: "year", count: yearsAgo } else
        if monthsAgo > 0 then Just { unit: "month", count: monthsAgo } else
        if daysAgo > 0 then Just { unit: "day", count: daysAgo } else
        if hoursAgo > 0 then Just { unit: "hour", count: hoursAgo } else
        if minutesAgo > 0 then Just { unit: "minute", count: minutesAgo } else
        Nothing
    in
    case interval of
    Just { unit, count } -> show count <> " " <> unit <> (if count == 1 then "" else "s") <> " ago"
    Nothing -> "less than a minute ago"

totalPages :: Int -> Int
totalPages count = ceil (toNumber count / toNumber pageSize)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { fields, profiles, profileCount, showCreateProfile, playerInfo, page } =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h3 [ HP.class_ $ HH.ClassName "card-title" ] $
        [ HH.span [ HP.class_ $ HH.ClassName "card-title-text" ]
            [ HH.text "Player profiles"
            , divider
            , HH.span [ HP.class_ $ HH.ClassName "card-subtitle" ]
                [ HH.text $
                    (if profileCount == 0
                        then "Showing 0"
                        else
                            "Showing " <> show (1 + ((page - 1) * pageSize))
                            <> " - " <> show (min profileCount (page * pageSize))
                            <> " out of " <> show profileCount)
                    <> " players"
                ]
            ]
        ]
        <>
        if showCreateProfile
        then Array.singleton $
            HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HE.onClick $ const $ Just CreateProfileAction
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
            , HH.text "Create your profile"
            ]
        else []
    ]
    <>
    if Array.null profiles
    then Array.singleton $
        HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ]
    else (profiles # mapWithIndex \index profile ->
        HH.div [ HP.class_ $ HH.ClassName "card-section" ] $
        [ HH.h4 [ HP.class_ $ HH.ClassName "player-profile-title" ] $
            [ HH.div [ HP.class_ $ HH.ClassName "player-profile-title-item" ]
                [ navigationAnchorIndexed (SProxy :: SProxy "players") index
                    { path: "/players/" <> profile.nickname, content: HH.text profile.nickname }
                , divider
                , HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                    [ HH.text $ "Updated " <> lastUpdated profile.updatedSeconds ]
                ]
            ]
            <>
            case playerInfo of
            Just { nickname } | nickname /= profile.nickname ->
                [ HH.div [ HP.class_ $ HH.ClassName "player-profile-title-item" ]
                    [ navigationAnchorIndexed (SProxy :: SProxy "messagePlayer") index
                        { path: "/account/conversations/" <> profile.nickname
                        , content: HH.span [ HP.class_ $ HH.ClassName "player-profile-title-message"]
                            [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] [], HH.text "Message player" ]
                        }
                    ]
                ]
            _ -> []
        ]
        <>
        [ case profile.age of
            Just age ->
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Is " ]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show age ]
                , HH.text " years old"
                ]
            Nothing ->
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Age not specified" ]
                ]
        , case profile.country of
            Just country ->
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Lives in " ]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ]
                ]
            Nothing ->
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Location not specified" ]
                ]
       , if Array.null profile.languages
            then
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Language not specified" ]
                ]
            else
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Speaks " ]
                ]
                <>
                ( foldr
                    (\language state ->
                        if not state.firstLanguage
                        then state { firstLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ] ] }
                        else if not state.secondLanguage
                        then state { secondLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text " and " ] <> state.languagesSoFar }
                        else state { languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text ", " ] <> state.languagesSoFar }
                    )
                    { firstLanguage: false, secondLanguage: false, languagesSoFar: [] }
                    profile.languages
                    # _.languagesSoFar
                )
        , if profile.hasMicrophone
            then
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text "Has microphone" ]
                , HH.text $ " and is willing to communicate"
                ]
            else
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Doesn't have a microphone" ]
                ]
        , case profile.discordTag of
            Just discordTag ->
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                [ HH.i [ HP.class_ $ HH.ClassName "fab fa-discord profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text "Discord tag: " ]
                , HH.a
                    [ HP.class_ $ HH.ClassName "discord-tag"
                    , HE.onClick $ const $ Just $ CopyDiscordTag profile.nickname discordTag ]
                    [ HH.text discordTag ]
                ]
                <>
                if profile.discordTagCopied
                then Array.singleton $ HH.span [ HP.class_ $ HH.ClassName "discord-tag-copied" ] [ HH.text "Copied!" ]
                else []
            Nothing ->
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fab fa-discord profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Discord tag not specified" ]
                ]
        , case profile.weekdayOnline of
            Just { from, to } ->
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekdays" ]
                , HH.text " from "
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
                , HH.text " to "
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
                ]
            Nothing ->
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Weekday time not specified" ]
                ]
        , case profile.weekendOnline of
            Just { from, to } ->
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekends" ]
                , HH.text " from "
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
                , HH.text " to "
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
                ]
            Nothing ->
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Weekend time not specified" ]
                ]
        ]
        <> (fields <#> \field ->
            case field.ilk of
            1 ->
                case find (\fieldValue -> fieldValue.field.key == field.key) profile.fieldValues of
                Just fieldValue | Just url <- fieldValue.url ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.a [ HP.class_ $ HH.ClassName "profile-field-url", HP.href url ] [ HH.text field.label ]
                    ]
                _ ->
                    HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ field.label <> " not specified" ]
                    ]
            2 ->
                case find (\fieldValue -> fieldValue.field.key == field.key) profile.fieldValues of
                Just fieldValue | Just option <- fieldValue.option ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text option.label ]
                    ]
                _ ->
                    HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ field.label <> " not specified" ]
                    ]
            3 ->
                case find (\fieldValue -> fieldValue.field.key == field.key) profile.fieldValues of
                Just fieldValue | Just options <- fieldValue.options, not $ Array.null options ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                    ]
                    <>
                    (intercalate [(HH.text ", ")] $
                        map (\{ label } -> [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text label ] ]) options)
                _ ->
                    HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ field.label <> " not specified" ]
                    ]
            _ ->
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ field.label <> " not specified" ]
                ]
        )
        <> (if profile.newOrReturning
            then Array.singleton $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-book profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Is a"]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text " new or returning player" ]
                , HH.text $ " to the game"
                ]
            else Array.singleton $
                HH.p [ HP.class_ $ HH.ClassName "unspecified-profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-book profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Is an experienced player" ]
                ])
        <> (profile.summary <#> \paragraph ->
            HH.p [ HP.class_ $ HH.ClassName "profile-summary" ] [ HH.text paragraph ]))
    <> (Array.singleton $
        HH.div [ HP.class_ $ HH.ClassName "card-footer" ]
            [ HH.div [ HP.class_$ HH.ClassName "pagination" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == 1
                    , HE.onClick $ const $ Just $ ChangePageAction 1
                    ]
                    [ HH.text "First" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == 1
                    , HE.onClick $ const $ Just $ ChangePageAction $ page - 1
                    ]
                    [ HH.text "<" ]
                , HH.span [ HP.class_ $ HH.ClassName "pagination-page" ] [ HH.text $ show page <> "/" <> show (totalPages profileCount) ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == (totalPages profileCount)
                    , HE.onClick $ const $ Just $ ChangePageAction $ page + 1
                    ]
                    [ HH.text ">" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == (totalPages profileCount)
                    , HE.onClick $ const $ Just $ ChangePageAction $ totalPages profileCount
                    ]
                    [ HH.text "Last" ]
                ]
            ])

prepareState :: Input -> State
prepareState input = input { profiles = input.profiles <#>
    Record.build (Record.insert (SProxy :: SProxy "discordTagCopied") false) }

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Message (Async left) Unit
handleAction (Receive input) = H.put $ prepareState input
handleAction (ChangePageAction page) = H.raise $ ChangePage page
handleAction CreateProfileAction = H.raise CreateProfile
handleAction (CopyDiscordTag nickname discordTag) = do
    result <- H.lift $ Async.attempt $ writeTextAsync discordTag
    case result of
        Right _ -> H.modify_ \state -> state { profiles = state.profiles <#> \profile ->
            if profile.nickname == nickname
            then profile { discordTagCopied = true }
            else profile { discordTagCopied = false } }
        _ -> pure unit

component :: forall query left.
    H.Component HH.HTML query Input Message (Async left)
component = H.mkComponent
    { initialState: prepareState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

playerProfiles
    :: forall children action left
    .  Input
    -> (Message -> Maybe action)
    -> HH.ComponentHTML action (playerProfiles :: Slot | children) (Async left)
playerProfiles input handleOutput =
    HH.slot (SProxy :: SProxy "playerProfiles") unit component input handleOutput
