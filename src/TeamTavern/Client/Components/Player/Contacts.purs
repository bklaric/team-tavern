module TeamTavern.Client.Components.Player.Contacts (ContactsSlots, contacts, profileContacts) where

import Prelude

import Async (Async)
import Client.Components.Copyable (copyable)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Anchor (textAnchor)
import TeamTavern.Client.Components.Detail (detail', discordTagDetail, fieldDetail')
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Client.Snippets.Brands (detailBattleNetSvg, detailOriginSvg, detailPlayStationSvg, detailRiotSvg, detailSteamSvg, detailSwitchSvg, detailUbisoftSvg, detailXboxSvg)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsOpen)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

type ContactsSlots slots =
    ( battleTag :: Slot__String
    , discordTag :: Slot__String
    , friendCode :: Slot__String
    , gamerTag :: Slot__String
    , eaId :: Slot__String
    , ubisoftUsername :: Slot__String
    , psnId :: Slot__String
    , riotId :: Slot__String
    , steamId :: Slot__String
    | slots
    )

steamIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (steamId :: Slot__String | slots) (Async left)
steamIdDetail steamId' = steamId' <#> \steamId ->
    fieldDetail' detailSteamSvg "Steam ID"
    [ copyable (Proxy :: _ "steamId") steamId steamId ]

steamUrlDetail :: ∀ slots action. Maybe String -> Maybe $ HH.HTML slots action
steamUrlDetail steamId' = steamId' <#> \steamId ->
    detail' detailSteamSvg [ textAnchor "detail-url" ("https://steamcommunity.com/profiles/" <> steamId) "Steam profile" ]

riotIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (riotId :: Slot__String | slots) (Async left)
riotIdDetail riotId' = riotId' <#> \riotId ->
    fieldDetail' detailRiotSvg "Riot ID"
    [ copyable (Proxy :: _ "riotId") riotId riotId ]

battleTagDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (battleTag :: Slot__String | slots) (Async left)
battleTagDetail battleTag' = battleTag' <#> \battleTag ->
    fieldDetail' detailBattleNetSvg "BattleTag"
    [ copyable (Proxy :: _ "battleTag") battleTag battleTag ]

eaIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (eaId :: Slot__String | slots) (Async left)
eaIdDetail eaId' = eaId' <#> \eaId ->
    fieldDetail' detailOriginSvg "EA ID"
    [ copyable (Proxy :: _ "eaId") eaId eaId ]

ubisoftUsernameDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (ubisoftUsername :: Slot__String | slots) (Async left)
ubisoftUsernameDetail ubisoftUsername' = ubisoftUsername' <#> \ubisoftUsername ->
    fieldDetail' detailUbisoftSvg "Ubisoft Connect username"
    [ copyable (Proxy :: _ "ubisoftUsername") ubisoftUsername ubisoftUsername ]

psnIdDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (psnId :: Slot__String | slots) (Async left)
psnIdDetail psnId' = psnId' <#> \psnId ->
    fieldDetail' detailPlayStationSvg "PSN ID"
    [ copyable (Proxy :: _ "psnId") psnId psnId ]

gamerTagDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (gamerTag :: Slot__String | slots) (Async left)
gamerTagDetail gamerTag' = gamerTag' <#> \gamerTag ->
    fieldDetail' detailXboxSvg "Gamertag"
    [ copyable (Proxy :: _ "gamerTag") gamerTag gamerTag ]

friendCodeDetail :: ∀ left slots action.
    Maybe String -> Maybe $ HH.ComponentHTML action (friendCode :: Slot__String | slots) (Async left)
friendCodeDetail friendCode' = friendCode' <#> \friendCode ->
    fieldDetail' detailSwitchSvg "Friend code"
    [ copyable (Proxy :: _ "friendCode") friendCode friendCode ]

contacts :: ∀ fields action slots left.
    PlayerContactsOpen (nickname :: String | fields) -> Array (HH.ComponentHTML action (ContactsSlots slots) (Async left))
contacts conts =
    Array.catMaybes
    [ discordTagDetail conts.nickname conts.discordTag
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
    PlayerContactsOpen (nickname :: String, platform :: Platform | fields) -> Array (HH.ComponentHTML action (ContactsSlots slots) (Async left))
profileContacts conts @ { platform } =
    contacts conts
    { steamId = if platform == Steam then conts.steamId else Nothing
    , riotId = if platform == Riot then conts.riotId else Nothing
    , battleTag = if platform == BattleNet then conts.battleTag else Nothing
    , eaId = if platform == Origin then conts.eaId else Nothing
    , ubisoftUsername = if platform == Ubisoft then conts.ubisoftUsername else Nothing
    , psnId = if platform == PlayStation then conts.psnId else Nothing
    , gamerTag = if platform == Xbox then conts.gamerTag else Nothing
    , friendCode = if platform == Switch then conts.friendCode else Nothing
    }
