module TeamTavern.Server.Profile.AddPlayerProfile where

import Prelude

import Async (Async, examineLeftWithEffect)
import AsyncV as AsyncV
import Data.Bifunctor.Label (label)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Type.Proxy (Proxy(..))
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (validateContactsV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.LogError (logError)
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.SendResponse (sendResponse)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.Infrastructure.CheckPlayerAlerts (checkPlayerAlerts)
import TeamTavern.Server.Profile.Infrastructure.PatchPlayerContacts (patchPlayerContacts)
import TeamTavern.Server.Profile.Routes (Identifiers)

addPlayerProfile :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
addPlayerProfile pool identifiers cookies body =
    sendResponse $ examineLeftWithEffect logError do

    profileId <- pool # transaction \client -> do
        -- Read info info from cookies.
        cookieInfo <- ensureSignedInAs client cookies identifiers.nickname

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
            # label (Proxy :: _ "invalidBody")

        -- Add profile to database.
        profileId <- addProfile client (unwrap cookieInfo.id) identifiers profile

        -- Update contacts.
        patchPlayerContacts client (unwrap cookieInfo.id) contacts

        pure profileId

    -- Check alerts and notify.
    checkPlayerAlerts profileId pool
