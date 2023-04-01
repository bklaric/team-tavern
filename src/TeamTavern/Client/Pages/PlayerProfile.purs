module TeamTavern.Client.Pages.PlayerProfile (Input, playerProfile) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array.Extra (full)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Variant (onMatch)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Card (card, cardSection)
import TeamTavern.Client.Components.Content (actualContent, contentDescription, contentHeader, contentHeaderSection, contentHeading', contentHeadingFaIcon)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.Player.Contacts (profileContacts)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots, profileDetails')
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading', profileSubheading)
import TeamTavern.Client.Pages.Player.Status (Status(..), getStatus)
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformBadge)
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Script.Rendertron (appendRendetronNotFound)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPathQuery)
import TeamTavern.Client.Shared.Slot (Slot___, Slot__String)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Profile.ViewPlayerProfile (ViewPlayerProfile)
import TeamTavern.Routes.Profile.ViewPlayerProfile as ViewPlayerProfile
import Type.Proxy (Proxy(..))

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
    ( discordTag :: Slot__String
    , games :: Slot__String
    , player :: Slot___
    )

render :: ∀ left. State -> H.ComponentHTML Action Slots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { profile, status }) = let
    contactsDetails = profileContacts profile
    playerDetails' = playerDetails profile
    profileDetails = profileDetails' profile.fieldValues profile.newOrReturning
    about = textDetail profile.about
    ambitions = textDetail profile.ambitions
    in
    actualContent $
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
render NotFound = HH.p_ [ HH.text "Player profile could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player profile. Please try again later." ]

trackAlertOpen :: forall m. Bind m => MonadEffect m => String -> m Unit
trackAlertOpen handle = do
    idMaybe <- getQueryParam "id"
    tokenMaybe <- getQueryParam "token"
    case idMaybe, tokenMaybe of
        -- Can't name the property token because it's reserved by Mixpanel.
        Just id, Just token -> track "Alert open"
            {ilk: "player", id, tokenA: token, game: handle}
        _, _ -> pure unit

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> do
            trackAlertOpen input.handle
            handleAction $ Receive input
        _ -> pure unit
handleAction (Receive input) = do
    timezone <- getClientTimezone
    result <- H.lift $ Async.attempt $
        fetchPathQuery (Proxy :: _ ViewPlayerProfile) input { timezone }
    case result of
        Left _ -> H.put Error
        Right response -> response # onMatch
            { ok: \playerProfile' @ { nickname, title } -> do
                status <- getStatus nickname
                H.put $ Loaded { profile: playerProfile', status }
                setMeta (nickname <> " - " <> title <> " | TeamTavern")
                    ( "View " <> title <> " profile of player "
                    <> nickname <> " on TeamTavern."
                    )
            , notFound: const $ do
                appendRendetronNotFound
                H.put NotFound
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

playerProfile :: ∀ query children left.
    Input -> HH.ComponentHTML query (playerProfile :: Slot___ | children) (Async left)
playerProfile input = HH.slot (Proxy :: _ "playerProfile") unit component input absurd
