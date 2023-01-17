module TeamTavern.Client.Pages.TeamProfile (Input, teamProfile) where

import Prelude

import Async (Async)
import Async as Async
import Client.Components.Copyable as Copyable
import Data.Array.Extra (full)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Ads (descriptionLeaderboards, stickyLeaderboards)
import TeamTavern.Client.Components.Card (card, cardSection)
import TeamTavern.Client.Components.Content (contentColumns, contentDescription, contentHeader, contentHeaderSection, contentHeading', contentHeadingFaIcon)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading', profileSubheading)
import TeamTavern.Client.Components.Team.Contacts (profileContacts)
import TeamTavern.Client.Components.Team.ProfileDetails (profileDetails')
import TeamTavern.Client.Components.Team.TeamDetails (teamDetails)
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformBadge)
import TeamTavern.Client.Pages.Team.Status (Status(..), getStatus)
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPathQuery)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Profile.ViewTeamProfile (ViewTeamProfile)
import TeamTavern.Routes.Profile.ViewTeamProfile as ViewTeamProfile
import TeamTavern.Routes.Shared.Organization (OrganizationNW(..))
import Type.Proxy (Proxy(..))

type Input =
    { teamHandle :: String
    , gameHandle :: String
    }

data State
    = Empty Input
    | Loaded
        { profile :: ViewTeamProfile.OkContent
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
    , team :: SimpleSlot
    )

nameOrHandle :: ∀ fields. { handle :: String, organization :: OrganizationNW | fields } -> String
nameOrHandle { organization: OrganizedNW { name } } = name
nameOrHandle { handle } = handle

render :: ∀ left. State -> H.ComponentHTML Action Slots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { profile, status }) = let
    contactsDetails = profileContacts profile
    teamDetails' = teamDetails profile
    profileDetails = profileDetails' profile
    about = textDetail profile.about
    ambitions = textDetail profile.ambitions
    in
    HH.div_ $
    [ contentHeader $
        [ contentHeaderSection
            [ contentHeading'
                [ navigationAnchor (Proxy :: _ "team")
                    { path: "/teams/" <> profile.handle
                    , content: HH.span_
                        [ contentHeadingFaIcon "fas fa-users"
                        , HH.text profile.handle
                        ]
                    }
                ]
            ]
        ]
    , contentDescription
        case status of
        SignedInOwner -> "View your teams " <> profile.title <> " profile."
        _ -> "View " <> profile.title <> " profile of team " <> nameOrHandle profile <> "."
    ]
    <> descriptionLeaderboards
    <>
    [ card
        [ cardSection $
            [ profileHeader $
                if full profile.allPlatforms.tail
                then
                [ HH.div [ HS.class_ "team-profile-heading-container" ] $
                    [ profileHeading' (Proxy :: _ "games") profile.gameHandle
                        ("/games/" <> profile.gameHandle <> "/teams") profile.title
                    ]
                    <> (platformBadge <$> profile.selectedPlatforms)
                    <> [ profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
                ]
                else
                [ HH.div_ $
                    [ profileHeading' (Proxy :: _ "games") profile.gameHandle
                        ("/games/" <> profile.gameHandle <> "/teams") profile.title
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
                    guard (full teamDetails')
                    [ detailColumnHeading4 "Team details" ] <> teamDetails'
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
render NotFound = contentColumns [ HH.p_ [ HH.text "Team profile could not be found." ] ]
render Error = contentColumns [ HH.p_ [ HH.text
    "There has been an error loading the team profile. Please try again later." ] ]

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    idMaybe <- getQueryParam "id"
    tokenMaybe <- getQueryParam "token"
    case idMaybe, tokenMaybe of
        -- Can't name the property token because it's reserved by Mixpanel.
        Just id, Just token -> track "Alert open" {ilk: "team", id, tokenA: token}
        _, _ -> pure unit

    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive input) = do
    timezone <- getClientTimezone
    result <- H.lift $ Async.attempt $
        fetchPathQuery (Proxy :: _ ViewTeamProfile) input { timezone }
    case result of
        Left _ -> H.put Error
        Right response -> response # onMatch
            { ok: \teamProfile' @ { owner, title } -> do
                status <- getStatus owner
                H.put $ Loaded { profile: teamProfile', status }
                setMeta (nameOrHandle teamProfile' <> " - " <> title <> " | TeamTavern")
                    ( "View " <> title <> " profile of team "
                    <> nameOrHandle teamProfile' <> " on TeamTavern."
                    )
            , notFound: const $ H.put NotFound
            }
            (const $ H.put Error)

component :: ∀ query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

teamProfile :: ∀ query children left.
    Input -> HH.ComponentHTML query (teamProfile :: SimpleSlot | children) (Async left)
teamProfile input = HH.slot (Proxy :: _ "teamProfile") unit component input absurd
