module TeamTavern.Client.Pages.Profiles.TeamProfiles (TeamProfile, Input, Output(..), Slot, teamProfiles) where

import Prelude

import Async (Async)
import Data.Array (sort)
import Data.Array as Array
import Data.Array.Extra (full)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Ads (AdSlots, insertAdsInMiddle)
import TeamTavern.Client.Components.Ads as Ads
import TeamTavern.Client.Components.Card (cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Pagination (pagination)
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading, profileSubheading)
import TeamTavern.Client.Components.Team.Contacts (profileContacts)
import TeamTavern.Client.Components.Team.ProfileDetails (profileDetails')
import TeamTavern.Client.Components.Team.TeamDetails (teamDetails)
import TeamTavern.Client.Pages.Profiles.TeamBadge (communityBadge, informalBadge, organizedBadge, partyBadge, platformBadge)
import TeamTavern.Client.Pages.Profiles.TeamProfileOptions (teamProfileOptions)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Shared.Slot (Slot__String, Slot_O_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import TeamTavern.Routes.Profile.Shared (pageSize)
import TeamTavern.Routes.Shared.Field (ValuesMulti)
import TeamTavern.Routes.Shared.Organization (OrganizationNW(..), nameOrHandleNW)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Size (Size(..))
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsOpen)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web.UIEvent.MouseEvent (MouseEvent)

type TeamProfile = TeamContactsOpen
    ( owner :: String
    , handle :: String
    , organization :: OrganizationNW
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
    , allPlatforms :: Platforms
    , size :: Size
    , selectedPlatforms :: Array Platform
    , fieldValues :: ValuesMulti
    , newOrReturning :: Boolean
    , about :: Array String
    , ambitions :: Array String
    , updated :: String
    , updatedSeconds :: Number
    )

type Input =
    { handle :: String
    , profiles :: Array TeamProfile
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

type Slot = Slot_O_ Output

type ChildSlots = PlatformIdSlots + AdSlots
    ( teams :: Slot__String
    , discordTag :: Slot__String
    , teamProfileOptions :: Slot__String
    )

profileSection :: ∀ action left.
    String -> TeamProfile -> HH.ComponentHTML action ChildSlots (Async left)
profileSection handle profile = let
    teamDetails' = teamDetails profile
    profileDetails'' = profileDetails' profile.fieldValues profile.newOrReturning
    contactsDetails' = profileContacts profile
    about = textDetail profile.about
    ambitions = textDetail profile.ambitions
    in
    cardSection $
    [ profileHeader $
        [ HH.div [ HS.class_ "team-profile-heading-container" ] $
            [ profileHeading (Proxy :: _ "teams") profile.handle
                ("/teams/" <> profile.handle) (nameOrHandleNW profile.handle profile.organization)
            , case profile.organization of
                InformalNW -> informalBadge
                OrganizedNW _ -> organizedBadge
            , case profile.size of
                Party -> partyBadge
                Community -> communityBadge
            ]
            <> guard (full profile.allPlatforms.tail) (profile.selectedPlatforms # sort <#> platformBadge)
            <> [ profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
        ]
        <>
        [ teamProfileOptions { teamHandle: profile.handle, gameHandle: handle } ]
    ]
    <>
    [ detailColumns $
        [ detailColumn $
            guard (full contactsDetails')
            [ detailColumnHeading4 "Contacts" ] <> contactsDetails'
            <>
            guard (full teamDetails')
            [ detailColumnHeading4 "Team details" ] <> teamDetails'
            <>
            guard (full profileDetails'')
            [ detailColumnHeading4 "Game details" ] <> profileDetails''
        ]
        <>
        guard (full about || full ambitions)
        [ detailColumn $
            guard (full about) ([ detailColumnHeading4 "About" ] <> about)
            <>
            guard (full ambitions) ([ detailColumnHeading4 "Ambitions" ] <> ambitions)
        ]
    ]

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { handle, profiles, profileCount, playerInfo, page } =
    HH.div_ $ [
    HH.div [ HP.id "profiles-card", HS.class_ "card" ] $
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
            HH.a [ HS.class_ "primary-button", HE.onClick OpenPreboarding ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon"] [], HH.text "Create team profile" ]
        else []
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else ( profiles <#> profileSection handle # insertAdsInMiddle ) <> [ pagination page profileCount ChangePage ]
    ]

handleAction :: ∀ left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) = do
    H.put input
    H.query (Proxy :: _ "billboard") unit (Ads.Refresh unit) # void
    H.query (Proxy :: _ "leaderboard") unit (Ads.Refresh unit) # void
    H.query (Proxy :: _ "mobileTakeover") unit (Ads.Refresh unit) # void
    H.query (Proxy :: _ "mobileMpu") unit (Ads.Refresh unit) # void
handleAction (ChangePage page) = H.raise $ PageChanged page
handleAction (OpenPreboarding mouseEvent) = do
    preventMouseDefault mouseEvent
    H.raise PreboardingClicked

component :: ∀ query left. H.Component query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

teamProfiles
    :: ∀ children action left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (teamProfiles :: Slot | children) (Async left)
teamProfiles input handleOutput =
    HH.slot (Proxy :: _ "teamProfiles") unit component input handleOutput
