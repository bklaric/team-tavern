module TeamTavern.Routes.Shared.TeamContacts where

import Data.Maybe (Maybe)
import Data.Variant (Variant)

type TeamContactsRow fields =
    ( discordTag :: Maybe String
    , discordServer :: Maybe String
    , steamId :: Maybe String
    , riotId :: Maybe String
    , battleTag :: Maybe String
    , eaId :: Maybe String
    , psnId :: Maybe String
    , gamerTag :: Maybe String
    , friendCode :: Maybe String
    | fields
    )

type TeamContactsOpen fields = Record (TeamContactsRow fields)

type TeamContacts = TeamContactsOpen ()

type TeamContactsError = Variant
    ( battleTag :: String
    , discordTag :: String
    , discordServer :: String
    , friendCode :: String
    , gamerTag :: String
    , eaId :: String
    , psnId :: String
    , riotId :: String
    , steamId :: String
    )
