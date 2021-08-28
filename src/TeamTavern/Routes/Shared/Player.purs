module TeamTavern.Routes.Shared.Player where

import Data.Maybe (Maybe)
import Data.Variant (Variant)

type ContactsRow fields =
    ( discordTag :: Maybe String
    , steamId :: Maybe String
    , riotId :: Maybe String
    , battleTag :: Maybe String
    , psnId :: Maybe String
    , gamerTag :: Maybe String
    , friendCode :: Maybe String
    | fields
    )

type Contacts' fields = Record (ContactsRow fields)

type Contacts = Contacts' ()

type ContactsError = Variant
    ( battleTag :: String
    , discordTag :: String
    , friendCode :: String
    , gamerTag :: String
    , psnId :: String
    , riotId :: String
    , steamId :: String
    )
