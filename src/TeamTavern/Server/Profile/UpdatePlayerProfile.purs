module TeamTavern.Server.Profile.UpdatePlayerProfile where

import Prelude

import Async (Async, examineLeftWithEffect)
import AsyncV as AsyncV
import Data.Bifunctor.Label (label)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Server.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (validateContactsV)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.Infrastructure.PatchPlayerContacts (patchPlayerContacts)
import TeamTavern.Server.Profile.Routes (Identifiers)
import TeamTavern.Server.Profile.UpdatePlayerProfile.LogError (logError)
import TeamTavern.Server.Profile.UpdatePlayerProfile.SendResponse (sendResponse)
import TeamTavern.Server.Profile.UpdatePlayerProfile.UpdateProfile (updateProfile)

updatePlayerProfile :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
updatePlayerProfile pool identifiers cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read cookie info from cookies.
    cookieInfo <- readCookieInfo cookies

    pool # withTransaction (inj (SProxy :: SProxy "databaseError")) \client -> do
        -- Load game fields from database.
        game <- loadFields client identifiers.handle

        -- Read profile from body.
        profile' <- readProfile body

        -- We only want to patch the selected platform contact.
        let contacts' = profile'.contacts
                { steamId    = if profile'.details.platform == Steam       then profile'.contacts.steamId    else Nothing
                , riotId     = if profile'.details.platform == Riot        then profile'.contacts.riotId     else Nothing
                , battleTag  = if profile'.details.platform == BattleNet   then profile'.contacts.battleTag  else Nothing
                , psnId      = if profile'.details.platform == PlayStation then profile'.contacts.psnId      else Nothing
                , gamerTag   = if profile'.details.platform == Xbox        then profile'.contacts.gamerTag   else Nothing
                , friendCode = if profile'.details.platform == Switch      then profile'.contacts.friendCode else Nothing
                }

        -- Validate profile and contacts.
        { profile, contacts } <-
            { profile: _, contacts: _ }
            <$> validateProfileV game profile'.details
            <*> validateContactsV [ profile'.details.platform ] contacts'
            # AsyncV.toAsync
            # label (SProxy :: _ "invalidBody")

        -- Update profile.
        updateProfile client cookieInfo identifiers profile

        -- Update contacts.
        patchPlayerContacts client (unwrap cookieInfo.id) contacts
