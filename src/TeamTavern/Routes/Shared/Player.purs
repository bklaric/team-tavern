module TeamTavern.Routes.Shared.Player where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Platform (Platform)

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

type ContactsRequiredRow fields = ContactsRow (requiredPlatforms :: Array Platform | fields)

type ContactsRequired' fields = Record (ContactsRequiredRow fields)

type ContactsRequired = ContactsRequired' ()

type ContactsError = Variant
    ( battleTag :: String
    , discordTag :: String
    , friendCode :: String
    , gamerTag :: String
    , psnId :: String
    , riotId :: String
    , steamId :: String
    )
