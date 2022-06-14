module TeamTavern.Client.Pages.Profiles.PlayerProfiles (Fields, PlayerProfile, Input, Output(..), Slot, playerProfiles) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Array.Extra (full)
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (guard)
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Card (cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Pagination (pagination)
import TeamTavern.Client.Components.Player.Contacts (profileContacts)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots, profileDetails')
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading, profileSubheading)
import TeamTavern.Client.Pages.Profiles.PlayerProfileOptions (playerProfileOptions)
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformBadge)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Shared.Slot (StringSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsOpen)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (pageSize)
import Web.UIEvent.MouseEvent (MouseEvent)

type Fields = Array
    { ilk :: Int
    , label :: String
    , key :: String
    , icon :: String
    , domain :: Maybe String
    , options :: Maybe (Array
        { key :: String
        , label :: String
        })
    }

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
    , about :: Array String
    , ambitions :: Array String
    , newOrReturning :: Boolean
    , updated :: String
    , updatedSeconds :: Number
    )

type Input =
    { handle :: String
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

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots = PlatformIdSlots
    ( players :: Anchor.Slot String
    , discordTag :: Copyable.Slot String
    , playerProfileOptions :: StringSlot
    )

profileSection :: forall action left.
    String -> PlayerProfile -> HH.ComponentHTML action ChildSlots (Async left)
profileSection handle profile = let
    playerDetails' = playerDetails profile
    profileDetails'' = profileDetails' profile.platform profile.fieldValues profile.newOrReturning
    contactsDetails' = profileContacts profile
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

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { handle, profiles, profileCount, playerInfo, page } =
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
            HH.a [ HS.class_ "primary-button", HE.onClick $ Just <<< OpenPreboarding ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon"] [], HH.text "Create player profile" ]
        else []
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else ( profiles <#> profileSection handle ) <> [ pagination page profileCount ChangePage ]
    ]

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) = H.put input
handleAction (ChangePage page) = H.raise $ PageChanged page
handleAction (OpenPreboarding mouseEvent) = do
    preventMouseDefault mouseEvent
    H.raise PreboardingClicked

component :: forall query left. H.Component query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

playerProfiles
    :: forall children action left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (playerProfiles :: Slot | children) (Async left)
playerProfiles input handleOutput =
    HH.slot (Proxy :: _ "playerProfiles") unit component input handleOutput
