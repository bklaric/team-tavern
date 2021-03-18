module TeamTavern.Client.Pages.Profiles.TeamBadge where

import Prelude

import Data.Array as Array
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HP
import TeamTavern.Client.Components.Checkable (checkables, checkbox, radio)
import TeamTavern.Client.Snippets.Brands (battleNetSvg, playStationSvg, riotSvg, steamSvg, switchSvg, xboxSvg)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Organization (Organization(..))
import TeamTavern.Routes.Shared.Platform (Platform(..), Platforms)
import TeamTavern.Routes.Shared.Size (Size(..))

-- Badges.

partyBadge :: forall slots action. HH.HTML slots action
partyBadge = HH.span [ HS.class_ "party-badge" ] [ HH.text "Party" ]

communityBadge :: forall slots action. HH.HTML slots action
communityBadge = HH.span [ HS.class_ "community-badge" ] [ HH.text "Community" ]

informalBadge :: forall slots action. HH.HTML slots action
informalBadge = HH.span [ HS.class_ "informal-badge" ] [ HH.text "Informal" ]

organizedBadge :: forall slots action. HH.HTML slots action
organizedBadge = HH.span [ HS.class_ "organized-badge" ] [ HH.text "Organized" ]

platformBadge :: forall slots action. HH.HTML slots action -> String -> HH.HTML slots action
platformBadge icon platform = HH.span [ HS.class_ "platform-badge" ] [ icon, HH.text platform ]

-- Organization checkable badges.

organizationCheckableBadges :: forall slots action.
    HH.HTML slots action -> HH.HTML slots action -> (Organization -> action) -> HH.HTML slots action
organizationCheckableBadges informalCheckable organizedCheckable onValue =
    checkables
    [ HH.span
        [ HS.class_ "informal-badge checkable-container"
        , HP.onClick $ const $ Just $ onValue Informal]
        [ HH.text "Informal"
        , informalCheckable
        ]
    , HH.span
        [ HS.class_ "organized-badge checkable-container"
        , HP.onClick $ const $ Just $ onValue Organized]
        [ HH.text "Organized"
        , organizedCheckable
        ]
    ]

organizationCheckboxBadges :: forall slots action.
    Array Organization -> (Organization -> action) -> HH.HTML slots action
organizationCheckboxBadges selected onValue =
    organizationCheckableBadges (checkbox $ elem Informal selected) (checkbox $ elem Organized selected) onValue

organizationRadioBadges :: forall slots action.
    Organization -> (Organization -> action) -> HH.HTML slots action
organizationRadioBadges selected onValue =
    organizationCheckableBadges (radio $ Informal == selected) (radio $ Organized == selected) onValue

-- Size checkable badges.

sizeCheckableBadges :: forall slots action.
    HH.HTML slots action -> HH.HTML slots action -> (Size -> action) -> HH.HTML slots action
sizeCheckableBadges partyCheckable communityCheckable onValue =
    checkables
    [ HH.span
        [ HS.class_ "party-badge checkable-container"
        , HP.onClick $ const $ Just $ onValue Party]
        [ HH.text "Party"
        , partyCheckable
        ]
    , HH.span
        [ HS.class_ "community-badge checkable-container"
        , HP.onClick $ const $ Just $ onValue Community]
        [ HH.text "Community"
        , communityCheckable
        ]
    ]

sizeCheckboxBadges :: forall slots action.
    Array Size -> (Size -> action) -> HH.HTML slots action
sizeCheckboxBadges selected onValue =
    sizeCheckableBadges (checkbox $ elem Party selected) (checkbox $ elem Community selected) onValue

sizeRadioBadges :: forall slots action.
    Size -> (Size -> action) -> HH.HTML slots action
sizeRadioBadges selected onValue =
    sizeCheckableBadges (radio $ Party == selected) (radio $ Community == selected) onValue

-- Platform checkable badges.

badgeSteamSvg :: forall slots actions. HH.HTML slots actions
badgeSteamSvg = steamSvg "badge-icon"

badgeRiotSvg :: forall slots actions. HH.HTML slots actions
badgeRiotSvg = riotSvg "badge-icon"

badgeBattleNetSvg :: forall slots actions. HH.HTML slots actions
badgeBattleNetSvg = battleNetSvg "badge-icon"

badgePlayStationSvg :: forall slots actions. HH.HTML slots actions
badgePlayStationSvg = playStationSvg "badge-icon"

badgeXboxSvg :: forall slots actions. HH.HTML slots actions
badgeXboxSvg = xboxSvg "badge-icon"

badgeSwitchSvg :: forall slots actions. HH.HTML slots actions
badgeSwitchSvg = switchSvg "badge-icon"

platformCheckableBadge :: forall slots action.
    HH.HTML slots action -> HH.HTML slots action -> String -> action -> HH.HTML slots action
platformCheckableBadge checkable icon label onValue =
    HH.span
    [ HS.class_ "platform-badge checkable-container"
    , HP.onClick $ const $ Just onValue]
    [ icon
    , HH.text label
    , checkable
    ]

platformCheckboxBadge :: forall slots action.
    Boolean -> HH.HTML slots action -> String -> action -> HH.HTML slots action
platformCheckboxBadge checked = platformCheckableBadge (checkbox checked)

platformRadioBadge :: forall slots action.
    Boolean -> HH.HTML slots action -> String -> action -> HH.HTML slots action
platformRadioBadge checked = platformCheckableBadge (radio checked)

platformCheckboxBadges :: forall action slots.
    Platforms -> Array Platform -> (Platform -> action) -> HH.HTML slots action
platformCheckboxBadges allPlatforms selectedPlatforms onValue =
    checkables $
    Array.cons allPlatforms.head allPlatforms.tail <#>
    case _ of
    Steam       -> platformCheckboxBadge (Array.elem Steam       selectedPlatforms) badgeSteamSvg       "Steam"       (onValue Steam)
    Riot        -> platformCheckboxBadge (Array.elem Riot        selectedPlatforms) badgeRiotSvg        "Riot"        (onValue Riot)
    BattleNet   -> platformCheckboxBadge (Array.elem BattleNet   selectedPlatforms) badgeBattleNetSvg   "Battle.net"  (onValue BattleNet)
    PlayStation -> platformCheckboxBadge (Array.elem PlayStation selectedPlatforms) badgePlayStationSvg "PlayStation" (onValue PlayStation)
    Xbox        -> platformCheckboxBadge (Array.elem Xbox        selectedPlatforms) badgeXboxSvg        "Xbox"        (onValue Xbox)
    Switch      -> platformCheckboxBadge (Array.elem Switch      selectedPlatforms) badgeSwitchSvg      "Switch"      (onValue Switch)

platformRadioBadges :: forall action slots.
    Platforms -> Platform -> (Platform -> action) -> HH.HTML slots action
platformRadioBadges allPlatforms selectedPlatform onValue =
    checkables $
    Array.cons allPlatforms.head allPlatforms.tail <#>
    case _ of
    Steam       -> platformRadioBadge (selectedPlatform == Steam      ) badgeSteamSvg       "Steam"       (onValue Steam)
    Riot        -> platformRadioBadge (selectedPlatform == Riot       ) badgeRiotSvg        "Riot"        (onValue Riot)
    BattleNet   -> platformRadioBadge (selectedPlatform == BattleNet  ) badgeBattleNetSvg   "Battle.net"  (onValue BattleNet)
    PlayStation -> platformRadioBadge (selectedPlatform == PlayStation) badgePlayStationSvg "PlayStation" (onValue PlayStation)
    Xbox        -> platformRadioBadge (selectedPlatform == Xbox       ) badgeXboxSvg        "Xbox"        (onValue Xbox)
    Switch      -> platformRadioBadge (selectedPlatform == Switch     ) badgeSwitchSvg      "Switch"      (onValue Switch)
