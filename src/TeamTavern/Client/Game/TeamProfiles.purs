module TeamTavern.Client.Game.TeamProfiles (TeamProfile, Input, Message(..), Slot, teamProfiles) where

import Prelude

import Async (Async)
import Data.Array (foldr, intercalate)
import Data.Array as Array
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (pageSize, pageSize')

type TeamProfile =
    { nickname :: String
    , age :: { from :: Maybe Int, to :: Maybe Int }
    , countries :: Array String
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
        , options :: Array
            { key :: String
            , label :: String
            }
        }
    , summary :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type Input =
    { profiles :: Array TeamProfile
    , profileCount :: Int
    , showCreateProfile :: Boolean
    , playerInfo :: Maybe PlayerInfo
    , page :: Int
    }

type State = Input

data Action
    = Receive Input
    | ChangePageAction Int
    | CreateProfileAction

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
totalPages count = ceil (toNumber count / pageSize')

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profiles, profileCount, showCreateProfile, playerInfo, page } =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.span [ HP.class_ $ HH.ClassName "card-title" ] $
        [ HH.span [ HP.class_ $ HH.ClassName "card-title-text" ]
            [ HH.text "Team profiles"
            , divider
            , HH.span [ HP.class_ $ HH.ClassName "card-subtitle" ]
                [ HH.text $
                    (if profileCount == 0
                        then "Showing 0"
                        else
                            "Showing " <> show (1 + ((page - 1) * pageSize))
                            <> " - " <> show (min profileCount (page * pageSize))
                            <> " out of " <> show profileCount)
                    <> " teams"
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
            , HH.text "Create team profile"
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
        [ HH.h3 [ HP.class_ $ HH.ClassName "player-profile-title" ] $
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
        <> Array.catMaybes
        [ case profile.age.from, profile.age.to of
            Nothing, Nothing -> Nothing
            Just from, Nothing -> Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are older than " ]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show from ]
                ]
            Nothing, Just to -> Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are younger than " ]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show to ]
                ]
            Just from, Just to -> Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are between " ]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show from ]
                , HH.text " and "
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show to ]
                , HH.text " years old"
                ]
        , if Array.null profile.countries
            then Nothing
            else Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Live in " ]
                ]
                <>
                (foldr
                    (\country state ->
                        if not state.firstCountry
                        then state { firstCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ] ] }
                        else if not state.secondCountry
                        then state { secondCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text " or " ] <> state.regionsSoFar }
                        else state { regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text ", " ] <> state.regionsSoFar }
                    )
                    { firstCountry: false, secondCountry: false, regionsSoFar: [] }
                    profile.countries
                    # _.regionsSoFar
                )
        , if Array.null profile.languages
            then Nothing
            else Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Speak " ]
                ]
                <>
                ( foldr
                    (\language state ->
                        if not state.firstLanguage
                        then state { firstLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ] ] }
                        else if not state.secondLanguage
                        then state { secondLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text " or " ] <> state.languagesSoFar }
                        else state { languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text ", " ] <> state.languagesSoFar }
                    )
                    { firstLanguage: false, secondLanguage: false, languagesSoFar: [] }
                    profile.languages
                    # _.languagesSoFar
                )
        , if profile.hasMicrophone
            then Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text "Have a microphone" ]
                , HH.text $ " and are willing to communicate."
                ]
            else Nothing
        , profile.weekdayOnline <#> \{ from, to } ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekdays" ]
            , HH.text " from "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
            , HH.text " to "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
            ]
        , profile.weekendOnline <#> \{ from, to } ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekends" ]
            , HH.text " from "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
            , HH.text " to "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
            ]
        ]
        <> (profile.fieldValues <#> \{ field, options } ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
            [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
            ]
            <>
            (intercalate [(HH.text ", ")] $
                map (\{ label } -> [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text label ] ]) options)
            )
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

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Message (Async left) Unit
handleAction (Receive input) = H.put input
handleAction (ChangePageAction page) = H.raise $ ChangePage page
handleAction CreateProfileAction = H.raise CreateProfile

component :: forall query left.
    H.Component HH.HTML query Input Message (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

teamProfiles
    :: forall children action left
    .  Input
    -> (Message -> Maybe action)
    -> HH.ComponentHTML action (teamProfiles :: Slot | children) (Async left)
teamProfiles input handleOutput =
    HH.slot (SProxy :: SProxy "teamProfiles") unit component input handleOutput
