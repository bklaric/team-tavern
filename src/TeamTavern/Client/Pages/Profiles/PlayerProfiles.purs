module TeamTavern.Client.Pages.Profiles.PlayerProfiles (PlayerProfile, Input, Output(..), Slot, playerProfiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Array.Extra (full)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Ads (insertMobileMpuInMiddleOrAtEnd)
import TeamTavern.Client.Components.Card (cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Pagination (pagination)
import TeamTavern.Client.Components.Player.Contacts (profileContacts)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots, profileDetails')
import TeamTavern.Client.Components.Player.TrackerDetails (trackerDetails)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading, profileSubheading)
import TeamTavern.Client.Pages.Profiles.PlayerProfileOptions (playerProfileOptions)
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformBadge)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Shared.Slot (Slot_O_, Slot__String)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import TeamTavern.Routes.Profile.Shared (pageSize)
import TeamTavern.Routes.Shared.Field (Values)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsOpen)
import TeamTavern.Routes.Shared.Tracker (Trackers)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

type PlayerProfile = PlayerContactsOpen
    ( nickname :: String
    , age :: Maybe Int
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    , platforms :: Platforms
    , platform :: Platform
    , fieldValues :: Values
    , about :: Array String
    , ambitions :: Array String
    , newOrReturning :: Boolean
    , updated :: String
    , updatedSeconds :: Number
    )

type Input =
    { handle :: String
    , trackers :: Trackers
    , profiles :: Array PlayerProfile
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

type ChildSlots = PlatformIdSlots
    ( players :: Slot__String
    , discordTag :: Slot__String
    , playerProfileOptions :: Slot__String
    )

profileSection :: ∀ action left.
    String -> Trackers -> PlayerProfile -> HH.ComponentHTML action ChildSlots (Async left)
profileSection handle trackers profile = let
    playerDetails' = playerDetails profile
    profileDetails'' = profileDetails' profile.fieldValues profile.newOrReturning
    contactsDetails' = profileContacts profile
    trackerDetails' = trackerDetails profile trackers
    about = textDetail profile.about
    ambitions = textDetail profile.ambitions
    in
    cardSection $
    [ profileHeader $
        [ if full profile.platforms.tail
        then
        HH.div [ HS.class_ "team-profile-heading-container" ] $
            [ profileHeading (Proxy :: _ "players") profile.nickname
                ("/players/" <> profile.nickname) profile.nickname
            ]
            <> [ platformBadge profile.platform, profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
        else
        HH.div_ $
            [ profileHeading (Proxy :: _ "players") profile.nickname
                ("/players/" <> profile.nickname) profile.nickname
            ]
            <> [ divider, profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
        ]
        <>
        [ playerProfileOptions { nickname: profile.nickname, handle } ]
    ]
    <>
    [ detailColumns $
        [ detailColumn $
            guard (full contactsDetails')
            [ detailColumnHeading4 "Contacts" ] <> contactsDetails'
            <>
            guard (full trackerDetails')
            [ detailColumnHeading4 "Trackers" ] <> trackerDetails'
            <>
            guard (full playerDetails')
            [ detailColumnHeading4 "Player details" ] <> playerDetails'
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
render { handle, trackers, profiles, profileCount, playerInfo, page } =
    HH.div_ $ [
    HH.div [ HP.id "profiles-card", HS.class_ "card" ] $
    [ cardHeader $
        [ HH.div_
            [ cardHeading "Player profiles"
            , divider
            , cardSubheading $
                ( if profileCount == 0
                    then "Showing 0"
                    else
                        "Showing " <> show (1 + ((page - 1) * pageSize))
                        <> " - " <> show (min profileCount (page * pageSize))
                        <> " out of " <> show profileCount
                )
                <> " players"
            ]
        ]
        <>
        if isNothing playerInfo
        then Array.singleton $
            HH.a [ HS.class_ "primary-button", HE.onClick OpenPreboarding ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon"] [], HH.text "Create player profile" ]
        else []
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else ( profiles <#> profileSection handle trackers # insertMobileMpuInMiddleOrAtEnd ) <> [ pagination page profileCount ChangePage ]
    ]

handleAction :: ∀ left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) = H.put input
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

playerProfiles
    :: ∀ children action left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (playerProfiles :: Slot | children) (Async left)
playerProfiles input handleOutput =
    HH.slot (Proxy :: _ "playerProfiles") unit component input handleOutput
