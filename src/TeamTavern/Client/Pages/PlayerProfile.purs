module TeamTavern.Client.Pages.PlayerProfile (Input, playerProfile) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array.Extra (full)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Ads (descriptionLeaderboards, stickyLeaderboards)
import TeamTavern.Client.Components.Card (card, cardSection)
import TeamTavern.Client.Components.Content (contentDescription, contentHeader, contentHeaderSection, contentHeading', contentHeadingFaIcon)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Player.Contacts (profileContacts)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots, profileDetails')
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading', profileSubheading)
import TeamTavern.Client.Pages.Player.Status (Status(..), getStatus)
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformBadge)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.ViewPlayerProfile as ViewPlayerProfile

type Input =
    { nickname :: String
    , handle :: String
    }

data State
    = Empty Input
    | Loaded
        { profile :: ViewPlayerProfile.OkContent
        , status :: Status
        }
    | NotFound
    | Error

data Action
    = Initialize
    | Receive Input

type Slots = PlatformIdSlots
    ( discordTag :: Copyable.Slot String
    , games :: NavigationAnchor.Slot String
    , player :: SimpleSlot
    )

render :: forall left. State -> H.ComponentHTML Action Slots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { profile, status }) = let
    contactsDetails = profileContacts profile
    playerDetails' = playerDetails profile
    profileDetails = profileDetails' profile.platform profile.fieldValues profile.newOrReturning
    about = textDetail profile.about
    ambitions = textDetail profile.ambitions
    in
    HH.div_ $
    [ contentHeader $
        [ contentHeaderSection
            [ contentHeading'
                [ navigationAnchor (Proxy :: _ "player")
                    { path: "/players/" <> profile.nickname
                    , content: HH.span_
                        [ contentHeadingFaIcon "fas fa-user"
                        , HH.text profile.nickname
                        ]
                    }
                ]
            ]
        ]
    , contentDescription
        case status of
        SignedInSelf -> "View your " <> profile.title <> " profile."
        _ -> "View " <> profile.title <> " profile of player " <> profile.nickname <> "."
    ]
    <> descriptionLeaderboards
    <>
    [ card
        [ cardSection $
            [ profileHeader $
                if full profile.platforms.tail
                then
                [ HH.div [ HS.class_ "team-profile-heading-container" ] $
                    [ profileHeading' (Proxy :: _ "games") profile.handle
                        ("/games/" <> profile.handle <> "/players") profile.title
                    ]
                    <> [ platformBadge profile.platform, profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
                ]
                else
                [ HH.div_ $
                    [ profileHeading' (Proxy :: _ "games") profile.handle
                        ("/games/" <> profile.handle <> "/players") profile.title
                    ]
                    <> [ divider, profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
                ]
            ]
            <>
            [ detailColumns $
                [ detailColumn $
                    guard (full contactsDetails)
                    [ detailColumnHeading4 "Contacts" ] <> contactsDetails
                    <>
                    guard (full playerDetails')
                    [ detailColumnHeading4 "Player details" ] <> playerDetails'
                    <>
                    guard (full profileDetails)
                    [ detailColumnHeading4 "Game details"] <> profileDetails
                ]
                <>
                guard (full about || full ambitions)
                [ detailColumn $
                    guard (full about) ([ detailColumnHeading4 "About" ] <> about)
                    <>
                    guard (full ambitions) ([ detailColumnHeading4 "Ambitions" ] <> ambitions)
                ]
            ]
        ]
    ]
    <> stickyLeaderboards
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayerProfile :: forall left. Input -> Async left (Maybe ViewPlayerProfile.OkContent)
loadPlayerProfile { nickname, handle } = do
    timezone <- getClientTimezone
    get ("/api/players/" <> nickname <> "/profiles/" <> handle <> "?timezone=" <> timezone)

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive input) = do
    playerProfileMaybe <- H.lift $ loadPlayerProfile input
    case playerProfileMaybe of
        Just playerProfile' @ { nickname, title } -> do
            status <- getStatus nickname
            H.put $ Loaded { profile: playerProfile', status }
            setMeta (nickname <> " - " <> title <> " | TeamTavern")
                ( "View " <> title <> " profile of player "
                <> nickname <> " on TeamTavern."
                )
        _ -> pure unit

component :: forall query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

playerProfile :: forall query children left.
    Input -> HH.ComponentHTML query (playerProfile :: SimpleSlot | children) (Async left)
playerProfile input = HH.slot (Proxy :: _ "playerProfile") unit component input absurd
