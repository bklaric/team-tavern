module TeamTavern.Routes.Shared.Player where

import Data.Maybe (Maybe)
import Data.Variant (Variant)

type Contacts =
    { discordTag :: Maybe String
    , steamId :: Maybe String
    , riotId :: Maybe String
    , battleTag :: Maybe String
    , psnId :: Maybe String
    , gamerTag :: Maybe String
    , friendCode :: Maybe String
    }

type ContactsError = Variant
    ( battleTag :: String
    , discordTag :: String
    , friendCode :: String
    , gamerTag :: String
    , psnId :: String
    , riotId :: String
    , steamId :: String
    )
