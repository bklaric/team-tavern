module TeamTavern.Client.Pages.Profiles.TeamBadge where

import Prelude

import Data.Array as Array
import Data.Foldable (elem)
import Halogen.HTML as HH
import Halogen.HTML.Events as HP
import TeamTavern.Client.Components.Checkable (checkables, checkbox, radio)
import TeamTavern.Client.Snippets.Brands (platformSvg)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Organization (Organization(..))
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.Size (Size(..))

-- Badges.

partyBadge :: ∀ slots action. HH.HTML slots action
partyBadge = HH.span [ HS.class_ "party-badge" ] [ HH.text "Party" ]

communityBadge :: ∀ slots action. HH.HTML slots action
communityBadge = HH.span [ HS.class_ "community-badge" ] [ HH.text "Community" ]

informalBadge :: ∀ slots action. HH.HTML slots action
informalBadge = HH.span [ HS.class_ "informal-badge" ] [ HH.text "Informal" ]

organizedBadge :: ∀ slots action. HH.HTML slots action
organizedBadge = HH.span [ HS.class_ "organized-badge" ] [ HH.text "Organized" ]

platformBadgeSvg :: ∀ slots actions. Platform -> HH.HTML slots actions
platformBadgeSvg = platformSvg "badge-icon"

platformBadge :: ∀ slots action. Platform -> HH.HTML slots action
platformBadge platform =
    HH.span [ HS.class_ "platform-badge" ]
    [ platformBadgeSvg platform, HH.text $ Platform.toLabel platform ]

-- Organization checkable badges.

organizationCheckableBadges :: ∀ slots action.
    HH.HTML slots action -> HH.HTML slots action -> (Organization -> action) -> HH.HTML slots action
organizationCheckableBadges informalCheckable organizedCheckable onValue =
    checkables
    [ HH.span
        [ HS.class_ "informal-badge checkable-container"
        , HP.onClick $ const $ onValue Informal]
        [ HH.text "Informal"
        , informalCheckable
        ]
    , HH.span
        [ HS.class_ "organized-badge checkable-container"
        , HP.onClick $ const $ onValue Organized]
        [ HH.text "Organized"
        , organizedCheckable
        ]
    ]

organizationCheckboxBadges :: ∀ slots action.
    Array Organization -> (Organization -> action) -> HH.HTML slots action
organizationCheckboxBadges selected onValue =
    organizationCheckableBadges (checkbox $ elem Informal selected) (checkbox $ elem Organized selected) onValue

organizationRadioBadges :: ∀ slots action.
    Organization -> (Organization -> action) -> HH.HTML slots action
organizationRadioBadges selected onValue =
    organizationCheckableBadges (radio $ Informal == selected) (radio $ Organized == selected) onValue

-- Size checkable badges.

sizeCheckableBadges :: ∀ slots action.
    HH.HTML slots action -> HH.HTML slots action -> (Size -> action) -> HH.HTML slots action
sizeCheckableBadges partyCheckable communityCheckable onValue =
    checkables
    [ HH.span
        [ HS.class_ "party-badge checkable-container"
        , HP.onClick $ const $ onValue Party]
        [ HH.text "Party"
        , partyCheckable
        ]
    , HH.span
        [ HS.class_ "community-badge checkable-container"
        , HP.onClick $ const $ onValue Community]
        [ HH.text "Community"
        , communityCheckable
        ]
    ]

sizeCheckboxBadges :: ∀ slots action.
    Array Size -> (Size -> action) -> HH.HTML slots action
sizeCheckboxBadges selected onValue =
    sizeCheckableBadges (checkbox $ elem Party selected) (checkbox $ elem Community selected) onValue

sizeRadioBadges :: ∀ slots action.
    Size -> (Size -> action) -> HH.HTML slots action
sizeRadioBadges selected onValue =
    sizeCheckableBadges (radio $ Party == selected) (radio $ Community == selected) onValue

-- Platform checkable badges.

platformCheckableBadge :: ∀ slots action.
    HH.HTML slots action -> HH.HTML slots action -> String -> action -> HH.HTML slots action
platformCheckableBadge checkable icon label onValue =
    HH.span
    [ HS.class_ "platform-badge checkable-container"
    , HP.onClick $ const onValue]
    [ icon
    , HH.text label
    , checkable
    ]

platformCheckboxBadge :: ∀ slots action.
    Boolean -> HH.HTML slots action -> String -> action -> HH.HTML slots action
platformCheckboxBadge checked = platformCheckableBadge (checkbox checked)

platformRadioBadge :: ∀ slots action.
    Boolean -> HH.HTML slots action -> String -> action -> HH.HTML slots action
platformRadioBadge checked = platformCheckableBadge (radio checked)

platformCheckboxBadges :: ∀ action slots.
    Platforms -> Array Platform -> (Platform -> action) -> HH.HTML slots action
platformCheckboxBadges allPlatforms selectedPlatforms onValue =
    checkables $
    Array.cons allPlatforms.head allPlatforms.tail <#> \platform ->
        platformCheckboxBadge
        (Array.elem platform selectedPlatforms) (platformBadgeSvg platform)
        (Platform.toLabel platform) (onValue platform)

platformRadioBadges :: ∀ action slots.
    Platforms -> Platform -> (Platform -> action) -> HH.HTML slots action
platformRadioBadges allPlatforms selectedPlatform onValue =
    checkables $
    Array.cons allPlatforms.head allPlatforms.tail <#> \platform ->
        platformRadioBadge
        (selectedPlatform == platform) (platformBadgeSvg platform)
        (Platform.toLabel platform) (onValue platform)
