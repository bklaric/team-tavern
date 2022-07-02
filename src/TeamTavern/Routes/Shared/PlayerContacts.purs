module TeamTavern.Routes.Shared.PlayerContacts where

import Data.Maybe (Maybe)
import Data.Variant (Variant)

type PlayerContactsRow fields =
    ( discordTag :: Maybe String
    , steamId :: Maybe String
    , riotId :: Maybe String
    , battleTag :: Maybe String
    , eaId :: Maybe String
    , psnId :: Maybe String
    , gamerTag :: Maybe String
    , friendCode :: Maybe String
    | fields
    )

type PlayerContactsOpen fields = Record (PlayerContactsRow fields)

type PlayerContacts = PlayerContactsOpen ()

type PlayerContactsError = Variant
    ( battleTag :: String
    , discordTag :: String
    , friendCode :: String
    , gamerTag :: String
    , eaId :: String
    , psnId :: String
    , riotId :: String
    , steamId :: String
    )
