module TeamTavern.Client.Components.Player.TrackerDetails where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Halogen.HTML as HH
import JSURI (encodeURIComponent)
import TeamTavern.Client.Components.Detail (urlDetail)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsOpen)
import TeamTavern.Routes.Shared.Tracker (Trackers, Tracker)

-- Create tracker URLs for player profile.
trackerUrl :: forall fields.
    PlayerContactsOpen fields -> Tracker -> Maybe String
trackerUrl contacts {platform, title, template} =
    case platform, contacts of
    Steam, {steamId: Just steamId} -> encodeURIComponent steamId <#> (template <> _)
    Riot, {riotId: Just riotId} ->
        case title of
        "blitz.gg" -> Just $ template <> (replace (Pattern "#") (Replacement "-") riotId)
        _ -> encodeURIComponent riotId <#> (template <> _)
    BattleNet, {battleTag: Just battleTag} ->
        case title of
        "overbuff.com" -> Just $ template <> (replace (Pattern "#") (Replacement "-") battleTag)
        _ -> encodeURIComponent battleTag <#> (template <> _)
    Origin, {eaId: Just eaId} -> encodeURIComponent eaId <#> (template <> _)
    Ubisoft, {ubisoftUsername: Just ubisoftUsername} -> encodeURIComponent ubisoftUsername <#> (template <> _)
    PlayStation, {psnId: Just psnId} -> encodeURIComponent psnId <#> (template <> _)
    Xbox, {gamerTag: Just gamerTag} -> encodeURIComponent gamerTag <#> (template <> _)
    Switch, {friendCode: Just friendCode} -> encodeURIComponent friendCode <#> (template <> _)
    _, _ -> Nothing

-- Create tracker details for player profile.
trackerDetails :: forall fields slots action.
    PlayerContactsOpen fields -> Trackers -> Array (HH.HTML slots action)
trackerDetails contacts trackers = trackers # mapMaybe \tracker @ {title} ->
    trackerUrl contacts tracker <#> \url -> urlDetail "fas fa-link" title url
