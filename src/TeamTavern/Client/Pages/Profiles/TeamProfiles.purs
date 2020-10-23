module TeamTavern.Client.Pages.Profiles.TeamProfiles (TeamProfile, Input, Message(..), Slot, teamProfiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Profile (profileHeader, profileHeaderItem, profileHeading, profileSubheading)
import TeamTavern.Client.Components.Team.ProfileDetail (profileDetails')
import TeamTavern.Client.Components.Team.TeamDetail (teamDetails)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (pageSize)

type TeamProfile =
    { owner :: String
    , handle :: String
    , name :: String
    , website :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: Maybe String
    , weekdayOnline :: Maybe
        { from :: String
        , to :: String
        }
    , weekendOnline :: Maybe
        { from :: String
        , to :: String
        }
    , about :: Array String
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
    , newOrReturning :: Boolean
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
    ( teams :: Anchor.Slot String
    , messageOwner :: Anchor.Slot String
    )

totalPages :: Int -> Int
totalPages count = ceil (toNumber count / toNumber pageSize)



render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profiles, profileCount, showCreateProfile, playerInfo, page } =
    card $
    [ cardHeader $
        [ HH.div_ $
            [ cardHeading "Team profiles"
            , divider
            , cardSubheading $
                ( if profileCount == 0
                    then "Showing 0"
                    else
                        "Showing " <> show (1 + ((page - 1) * pageSize))
                        <> " - " <> show (min profileCount (page * pageSize))
                        <> " out of " <> show profileCount
                )
                <> " teams"
            ]
        ]
        -- <>
        -- if showCreateProfile
        -- then Array.singleton $
        --     HH.button
        --     [ HP.class_ $ HH.ClassName "primary-button"
        --     , HE.onClick $ const $ Just CreateProfileAction
        --     ]
        --     [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
        --     , HH.text "Create team profile"
        --     ]
        -- else []
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else
    (profiles # mapWithIndex \index profile -> let
        teamDetails' = teamDetails profile
        profileDetails'' = profileDetails' profile.fieldValues profile.newOrReturning
        about = textDetail profile.about
        ambitions = textDetail profile.summary
        in
        cardSection $
        [ profileHeader $
            [ profileHeaderItem
                [ profileHeading (SProxy :: SProxy "teams") profile.handle
                    ("/teams/" <> profile.handle) profile.name
                , divider
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            ]
            <>
            case playerInfo of
            Just { nickname } | nickname /= profile.owner ->
                [ profileHeaderItem
                    [ navigationAnchorIndexed (SProxy :: SProxy "messageOwner") profile.owner
                        { path: "/conversations/" <> profile.owner
                        , content: HH.span_
                            [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
                            , HH.text "Message team owner"
                            ]
                        }
                    ]
                ]
            _ -> []
        ]
        <>
        if (not $ Array.null teamDetails') || (not $ Array.null profileDetails'')
            || (not $ Array.null about) || (not $ Array.null ambitions)
        then
            [ detailColumns $
                ( if (not $ Array.null teamDetails') || (not $ Array.null profileDetails'')
                    then
                        [ detailColumn $
                            ( if not $ Array.null teamDetails'
                                then [ detailColumnHeading "Team details" ] <> teamDetails'
                                else []
                            )
                            <>
                            ( if not $ Array.null profileDetails''
                                then [ detailColumnHeading "Profile details" ] <> profileDetails''
                                else []
                            )
                        ]
                    else []
                )
                <>
                ( if (not $ Array.null about) || (not $ Array.null ambitions)
                    then
                        [ detailColumn $
                            ( if not $ Array.null about
                                then [ detailColumnHeading "About" ] <> about
                                else []
                            )
                            <>
                            ( if not $ Array.null ambitions
                                then [ detailColumnHeading "Ambitions" ] <> ambitions
                                else []
                            )
                        ]
                    else []
                )
            ]
        else []
        -- <> Array.catMaybes
        -- [ case profile.age.from, profile.age.to of
        --     Nothing, Nothing -> Nothing
        --     Just from, Nothing -> Just $
        --         HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
        --         [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are older than " ]
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show from ]
        --         ]
        --     Nothing, Just to -> Just $
        --         HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
        --         [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are younger than " ]
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show to ]
        --         ]
        --     Just from, Just to -> Just $
        --         HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
        --         [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are between " ]
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show from ]
        --         , HH.text " and "
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show to ]
        --         , HH.text " years old"
        --         ]
        -- , if Array.null profile.countries
        --     then Nothing
        --     else Just $
        --         HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
        --         [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe profile-field-icon" ] []
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Live in " ]
        --         ]
        --         <>
        --         (foldr
        --             (\country state ->
        --                 if not state.firstCountry
        --                 then state { firstCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ] ] }
        --                 else if not state.secondCountry
        --                 then state { secondCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text " or " ] <> state.regionsSoFar }
        --                 else state { regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text ", " ] <> state.regionsSoFar }
        --             )
        --             { firstCountry: false, secondCountry: false, regionsSoFar: [] }
        --             profile.countries
        --             # _.regionsSoFar
        --         )
        -- , if Array.null profile.languages
        --     then Nothing
        --     else Just $
        --         HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
        --         [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Speak " ]
        --         ]
        --         <>
        --         ( foldr
        --             (\language state ->
        --                 if not state.firstLanguage
        --                 then state { firstLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ] ] }
        --                 else if not state.secondLanguage
        --                 then state { secondLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text " or " ] <> state.languagesSoFar }
        --                 else state { languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text ", " ] <> state.languagesSoFar }
        --             )
        --             { firstLanguage: false, secondLanguage: false, languagesSoFar: [] }
        --             profile.languages
        --             # _.languagesSoFar
        --         )
        -- , if profile.hasMicrophone
        --     then Just $
        --         HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
        --         [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text "Have a microphone" ]
        --         , HH.text $ " and are willing to communicate"
        --         ]
        --     else Nothing
        -- , profile.weekdayOnline <#> \{ from, to } ->
        --     HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
        --     [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekdays" ]
        --     , HH.text " from "
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
        --     , HH.text " to "
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
        --     ]
        -- , profile.weekendOnline <#> \{ from, to } ->
        --     HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
        --     [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekends" ]
        --     , HH.text " from "
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
        --     , HH.text " to "
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
        --     ]
        -- ]
        -- <> (profile.fieldValues <#> \{ field, options } ->
        --     HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
        --     [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
        --     , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
        --     ]
        --     <>
        --     (intercalate [(HH.text ", ")] $
        --         map (\{ label } -> [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text label ] ]) options)
        --     )
        -- <> (if profile.newOrReturning
        --     then Array.singleton $
        --         HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
        --         [ HH.i [ HP.class_ $ HH.ClassName "fas fa-book profile-field-icon" ] []
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are"]
        --         , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text " new or returning players" ]
        --         , HH.text $ " to the game"
        --         ]
        --     else [])
        -- <> (profile.summary <#> \paragraph ->
        --     HH.p [ HP.class_ $ HH.ClassName "profile-summary" ] [ HH.text paragraph ])
    )
    <> (Array.singleton $
        HH.div [ HP.class_ $ HH.ClassName "pagination" ]
            [ HH.div [ HP.class_$ HH.ClassName "pagination-left-buttons" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "pagination-first-button"
                    , HP.disabled $ page == 1
                    , HE.onClick $ const $ Just $ ChangePageAction 1
                    ]
                    [ HH.text "First" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-previous-button"
                    , HP.disabled $ page == 1
                    , HE.onClick $ const $ Just $ ChangePageAction $ page - 1
                    ]
                    [ HH.text "Previous" ]
                ]
            , HH.div [ HP.class_ $ HH.ClassName "pagination-page" ]
                [ HH.text $ show page <> "/" <> show (totalPages profileCount) ]
            , HH.div [ HP.class_$ HH.ClassName "pagination-right-buttons" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "pagination-next-button"
                    , HP.disabled $ page == (totalPages profileCount)
                    , HE.onClick $ const $ Just $ ChangePageAction $ page + 1
                    ]
                    [ HH.text "Next" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-last-button"
                    , HP.disabled $ page == (totalPages profileCount)
                    , HE.onClick $ const $ Just $ ChangePageAction $ totalPages profileCount
                    ]
                    [ HH.text "Last" ]
                ]
            ]
        )

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
