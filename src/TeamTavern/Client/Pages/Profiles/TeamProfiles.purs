module TeamTavern.Client.Pages.Profiles.TeamProfiles (TeamProfile, Input, Output(..), Slot, teamProfiles) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Ads (profileSectionsWithLeaderboard)
import TeamTavern.Client.Components.Card (cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Pagination (pagination)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading, profileSubheading)
import TeamTavern.Client.Components.Team.ProfileDetails (profileDetails')
import TeamTavern.Client.Components.Team.TeamDetails (teamDetails)
import TeamTavern.Client.Pages.Profiles.TeamBadge (communityBadge, informalBadge, organizedBadge, partyBadge)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import TeamTavern.Routes.Shared.Organization (OrganizationNW(..), nameOrHandleNW)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Size (Size(..))
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (pageSize)
import Web.UIEvent.MouseEvent (MouseEvent)

type TeamProfile =
    { owner :: String
    , handle :: String
    , organization :: OrganizationNW
    , discordTag :: Maybe String
    , discordServer :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe
        { from :: String
        , to :: String
        }
    , weekendOnline :: Maybe
        { from :: String
        , to :: String
        }
    , about :: Array String
    , allPlatforms :: Platforms
    , size :: Size
    , selectedPlatforms :: Array Platform
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
    , ambitions :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type Input =
    { profiles :: Array TeamProfile
    , profileCount :: Int
    , playerInfo :: Maybe PlayerInfo
    , page :: Int
    }

type State = Input

data Action
    = Receive Input
    | ChangePage Int
    | OpenPreboarding MouseEvent

data Output = PageChanged Int | PreboardingClicked

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( teams :: Anchor.Slot String
    , discordTag :: Copyable.Slot String
    )

profileSection :: forall action left.
    TeamProfile -> HH.ComponentHTML action ChildSlots (Async left)
profileSection profile = let
    teamDetails' = teamDetails profile
    profileDetails'' = profileDetails' profile
    about = textDetail profile.about
    ambitions = textDetail profile.ambitions
    in
    cardSection $
    [ profileHeader
        [ HH.div [ HS.class_ "team-profile-heading-container" ]
            [ profileHeading (SProxy :: SProxy "teams") profile.handle
                ("/teams/" <> profile.handle) (nameOrHandleNW profile.handle profile.organization)
            , case profile.organization of
                InformalNW -> informalBadge
                OrganizedNW _ -> organizedBadge
            , case profile.size of
                Party -> partyBadge
                Community -> communityBadge
            , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
            ]
        ]
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
                            then [ detailColumnHeading4 "Team details" ] <> teamDetails'
                            else []
                        )
                        <>
                        ( if not $ Array.null profileDetails''
                            then [ detailColumnHeading4 "Profile details" ] <> profileDetails''
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
                            then [ detailColumnHeading4 "About" ] <> about
                            else []
                        )
                        <>
                        ( if not $ Array.null ambitions
                            then [ detailColumnHeading4 "Ambitions" ] <> ambitions
                            else []
                        )
                    ]
                else []
            )
        ]
    else []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profiles, profileCount, playerInfo, page } =
    HH.div [ HS.class_ "profiles-container" ] $ [
    HH.div [ HP.id_ "profiles-card", HS.class_ "card" ] $
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
        <>
        if isNothing playerInfo
        then Array.singleton $
            HH.a [ HS.class_ "primary-button", HE.onClick $ Just <<< OpenPreboarding ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon"] [], HH.text "Create team profile" ]
        else []
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else ( profiles <#> profileSection # profileSectionsWithLeaderboard )
        <> [ pagination page profileCount ChangePage ]
    ]

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) = H.put input
handleAction (ChangePage page) = H.raise $ PageChanged page
handleAction (OpenPreboarding mouseEvent) = do
    preventMouseDefault mouseEvent
    H.raise PreboardingClicked

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
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
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (teamProfiles :: Slot | children) (Async left)
teamProfiles input handleOutput =
    HH.slot (SProxy :: SProxy "teamProfiles") unit component input handleOutput
