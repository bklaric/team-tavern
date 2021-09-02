module TeamTavern.Client.Components.Team.Contacts (ContactsSlots, contacts, profileContacts) where

import Prelude

import Async (Async)
import Client.Components.Copyable (copyable)
import Client.Components.Copyable as Copyable
import Data.Array (elem)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Anchor (textAnchor)
import TeamTavern.Client.Components.Detail (detail', discordTagDetail, fieldDetail', urlDetail)
import TeamTavern.Client.Snippets.Brands (detailBattleNetSvg, detailPlayStationSvg, detailRiotSvg, detailSteamSvg, detailSwitchSvg, detailXboxSvg)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.Team (Contacts')
import Type (type ($))

type ContactsSlots slots =
    ( battleTag :: Copyable.Slot String
    , discordTag :: Copyable.Slot String
    , friendCode :: Copyable.Slot String
    , gamerTag :: Copyable.Slot String
    , psnId :: Copyable.Slot String
    , riotId :: Copyable.Slot String
    , steamId :: Copyable.Slot String
    | slots
    )

teamDiscordServerDetail :: forall slots actions. Maybe String -> Maybe (HH.HTML slots actions)
teamDiscordServerDetail discordServer = urlDetail "fab fa-discord" "Discord server" discordServer

steamIdDetail :: forall left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (steamId :: Copyable.Slot String | slots) (Async left)
steamIdDetail steamId' = steamId' <#> \steamId ->
    fieldDetail' detailSteamSvg "Steam ID"
    [ copyable (SProxy :: _ "steamId") steamId steamId ]

steamUrlDetail :: forall slots action. Maybe String -> Maybe $ HH.HTML slots action
steamUrlDetail steamId' = steamId' <#> \steamId ->
    detail' detailSteamSvg [ textAnchor "detail-url" ("https://steamcommunity.com/profiles/" <> steamId) "Steam profile" ]

riotIdDetail :: forall left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (riotId :: Copyable.Slot String | slots) (Async left)
riotIdDetail riotId' = riotId' <#> \riotId ->
    fieldDetail' detailRiotSvg "Riot ID"
    [ copyable (SProxy :: _ "riotId") riotId riotId ]

battleTagDetail :: forall left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (battleTag :: Copyable.Slot String | slots) (Async left)
battleTagDetail battleTag' = battleTag' <#> \battleTag ->
    fieldDetail' detailBattleNetSvg "BattleTag"
    [ copyable (SProxy :: _ "battleTag") battleTag battleTag ]

psnIdDetail :: forall left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (psnId :: Copyable.Slot String | slots) (Async left)
psnIdDetail psnId' = psnId' <#> \psnId ->
    fieldDetail' detailPlayStationSvg "PSN ID"
    [ copyable (SProxy :: _ "psnId") psnId psnId ]

gamerTagDetail :: forall left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (gamerTag :: Copyable.Slot String | slots) (Async left)
gamerTagDetail gamerTag' = gamerTag' <#> \gamerTag ->
    fieldDetail' detailXboxSvg "Gamertag"
    [ copyable (SProxy :: _ "gamerTag") gamerTag gamerTag ]

friendCodeDetail :: forall left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (friendCode :: Copyable.Slot String | slots) (Async left)
friendCodeDetail friendCode' = friendCode' <#> \friendCode ->
    fieldDetail' detailSwitchSvg "Friend code"
    [ copyable (SProxy :: _ "friendCode") friendCode friendCode ]

contacts :: forall fields action slots left.
    Contacts' (handle :: String | fields) -> Array (HH.ComponentHTML action (ContactsSlots slots) (Async left))
contacts conts =
    Array.catMaybes
    [ discordTagDetail conts.handle conts.discordTag
    , teamDiscordServerDetail conts.discordServer
    , steamIdDetail conts.steamId
    , steamUrlDetail conts.steamId
    , riotIdDetail conts.riotId
    , battleTagDetail conts.battleTag
    , psnIdDetail conts.psnId
    , gamerTagDetail conts.gamerTag
    , friendCodeDetail conts.friendCode
    ]

profileContacts :: forall fields action slots left.
    Contacts' (handle :: String, selectedPlatforms :: Array Platform | fields) -> Array (HH.ComponentHTML action (ContactsSlots slots) (Async left))
profileContacts conts @ { selectedPlatforms } =
    contacts conts
    { steamId = if Steam `elem` selectedPlatforms then conts.steamId else Nothing
    , riotId = if Riot `elem` selectedPlatforms then conts.riotId else Nothing
    , battleTag = if BattleNet `elem` selectedPlatforms then conts.battleTag else Nothing
    , psnId = if PlayStation `elem` selectedPlatforms then conts.psnId else Nothing
    , gamerTag = if Xbox `elem` selectedPlatforms then conts.gamerTag else Nothing
    , friendCode = if Switch `elem` selectedPlatforms then conts.friendCode else Nothing
    }
