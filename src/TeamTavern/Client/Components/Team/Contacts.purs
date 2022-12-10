module TeamTavern.Client.Components.Team.Contacts (ContactsSlots, contacts, profileContacts) where

import Prelude

import Async (Async)
import Client.Components.Copyable (copyable)
import Client.Components.Copyable as Copyable
import Data.Array (elem)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Anchor (textAnchor)
import TeamTavern.Client.Components.Detail (detail', discordTagDetail, fieldDetail', urlDetail)
import TeamTavern.Client.Snippets.Brands (detailBattleNetSvg, detailOriginSvg, detailPlayStationSvg, detailRiotSvg, detailSteamSvg, detailSwitchSvg, detailUbisoftSvg, detailXboxSvg)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsOpen)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

type ContactsSlots slots =
    ( battleTag :: Copyable.Slot String
    , discordTag :: Copyable.Slot String
    , friendCode :: Copyable.Slot String
    , gamerTag :: Copyable.Slot String
    , eaId :: Copyable.Slot String
    , ubisoftUsername :: Copyable.Slot String
    , psnId :: Copyable.Slot String
    , riotId :: Copyable.Slot String
    , steamId :: Copyable.Slot String
    | slots
    )

teamDiscordServerDetail :: ∀ slots actions. Maybe String -> Maybe (HH.HTML slots actions)
teamDiscordServerDetail discordServer = urlDetail "fab fa-discord" "Discord server" discordServer

steamIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (steamId :: Copyable.Slot String | slots) (Async left)
steamIdDetail steamId' = steamId' <#> \steamId ->
    fieldDetail' detailSteamSvg "Steam ID"
    [ copyable (Proxy :: _ "steamId") steamId steamId ]

steamUrlDetail :: ∀ slots action. Maybe String -> Maybe $ HH.HTML slots action
steamUrlDetail steamId' = steamId' <#> \steamId ->
    detail' detailSteamSvg [ textAnchor "detail-url" ("https://steamcommunity.com/profiles/" <> steamId) "Steam profile" ]

riotIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (riotId :: Copyable.Slot String | slots) (Async left)
riotIdDetail riotId' = riotId' <#> \riotId ->
    fieldDetail' detailRiotSvg "Riot ID"
    [ copyable (Proxy :: _ "riotId") riotId riotId ]

battleTagDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (battleTag :: Copyable.Slot String | slots) (Async left)
battleTagDetail battleTag' = battleTag' <#> \battleTag ->
    fieldDetail' detailBattleNetSvg "BattleTag"
    [ copyable (Proxy :: _ "battleTag") battleTag battleTag ]

eaIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (eaId :: Copyable.Slot String | slots) (Async left)
eaIdDetail eaId' = eaId' <#> \eaId ->
    fieldDetail' detailOriginSvg "EA ID"
    [ copyable (Proxy :: _ "eaId") eaId eaId ]

ubisoftUsernameDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (ubisoftUsername :: Copyable.Slot String | slots) (Async left)
ubisoftUsernameDetail ubisoftUsername' = ubisoftUsername' <#> \ubisoftUsername ->
    fieldDetail' detailUbisoftSvg "Ubisoft Connect username"
    [ copyable (Proxy :: _ "ubisoftUsername") ubisoftUsername ubisoftUsername ]

psnIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (psnId :: Copyable.Slot String | slots) (Async left)
psnIdDetail psnId' = psnId' <#> \psnId ->
    fieldDetail' detailPlayStationSvg "PSN ID"
    [ copyable (Proxy :: _ "psnId") psnId psnId ]

gamerTagDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (gamerTag :: Copyable.Slot String | slots) (Async left)
gamerTagDetail gamerTag' = gamerTag' <#> \gamerTag ->
    fieldDetail' detailXboxSvg "Gamertag"
    [ copyable (Proxy :: _ "gamerTag") gamerTag gamerTag ]

friendCodeDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (friendCode :: Copyable.Slot String | slots) (Async left)
friendCodeDetail friendCode' = friendCode' <#> \friendCode ->
    fieldDetail' detailSwitchSvg "Friend code"
    [ copyable (Proxy :: _ "friendCode") friendCode friendCode ]

contacts :: ∀ fields action slots left.
    TeamContactsOpen (handle :: String | fields) -> Array (HH.ComponentHTML action (ContactsSlots slots) (Async left))
contacts conts =
    Array.catMaybes
    [ discordTagDetail conts.handle conts.discordTag
    , teamDiscordServerDetail conts.discordServer
    , steamIdDetail conts.steamId
    , steamUrlDetail conts.steamId
    , riotIdDetail conts.riotId
    , battleTagDetail conts.battleTag
    , eaIdDetail conts.eaId
    , ubisoftUsernameDetail conts.ubisoftUsername
    , psnIdDetail conts.psnId
    , gamerTagDetail conts.gamerTag
    , friendCodeDetail conts.friendCode
    ]

profileContacts :: ∀ fields action slots left.
    TeamContactsOpen (handle :: String, selectedPlatforms :: Array Platform | fields) -> Array (HH.ComponentHTML action (ContactsSlots slots) (Async left))
profileContacts conts @ { selectedPlatforms } =
    contacts conts
    { steamId = if Steam `elem` selectedPlatforms then conts.steamId else Nothing
    , riotId = if Riot `elem` selectedPlatforms then conts.riotId else Nothing
    , battleTag = if BattleNet `elem` selectedPlatforms then conts.battleTag else Nothing
    , eaId = if Origin `elem` selectedPlatforms then conts.eaId else Nothing
    , ubisoftUsername = if Ubisoft `elem` selectedPlatforms then conts.ubisoftUsername else Nothing
    , psnId = if PlayStation `elem` selectedPlatforms then conts.psnId else Nothing
    , gamerTag = if Xbox `elem` selectedPlatforms then conts.gamerTag else Nothing
    , friendCode = if Switch `elem` selectedPlatforms then conts.friendCode else Nothing
    }
