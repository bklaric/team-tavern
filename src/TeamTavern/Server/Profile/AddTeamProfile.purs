module TeamTavern.Server.Profile.AddTeamProfile where

import Prelude

import Async (Async, examineLeftWithEffect)
import AsyncV as AsyncV
import Data.Bifunctor.Label (label)
import Data.Foldable (elem)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddTeamProfile.LogError (logError)
import TeamTavern.Server.Profile.AddTeamProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddTeamProfile.SendResponse (sendResponse)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.Infrastructure.CheckTeamAlerts (checkTeamAlerts)
import TeamTavern.Server.Profile.Infrastructure.PatchTeamContacts (patchTeamContacts)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts (validateContactsV)

addTeamProfile :: forall left.
    Pool -> Map String String -> Body -> { teamHandle :: String, gameHandle :: String } -> Async left Response
addTeamProfile pool cookies body { teamHandle, gameHandle } =
    sendResponse $ examineLeftWithEffect logError do
    -- Read info from cookies.
    cookieInfo <- ensureSignedIn pool cookies

    profileId <- pool # transaction \client -> do
        -- Load game fields from database.
        game <- loadFields client gameHandle

        -- Read profile from body.
        profile' <- readProfile body

        -- We only want to patch the selected platforms contacts.
        let contacts' = profile'.contacts
                { steamId    = if Steam       `elem` profile'.details.platforms then profile'.contacts.steamId    else Nothing
                , riotId     = if Riot        `elem` profile'.details.platforms then profile'.contacts.riotId     else Nothing
                , battleTag  = if BattleNet   `elem` profile'.details.platforms then profile'.contacts.battleTag  else Nothing
                , psnId      = if PlayStation `elem` profile'.details.platforms then profile'.contacts.psnId      else Nothing
                , gamerTag   = if Xbox        `elem` profile'.details.platforms then profile'.contacts.gamerTag   else Nothing
                , friendCode = if Switch      `elem` profile'.details.platforms then profile'.contacts.friendCode else Nothing
                }

        -- Validate profile and contacts.
        { profile, contacts } <-
            { profile: _, contacts: _ }
            <$> validateProfileV game profile'.details
            <*> validateContactsV profile'.details.platforms contacts'
            # AsyncV.toAsync
            # label (SProxy :: _ "invalidBody")

        -- Add profile to database.
        profileId <- addProfile client cookieInfo.id teamHandle gameHandle profile

        -- Update contacts.
        patchTeamContacts client (unwrap cookieInfo.id) contacts

        pure profileId

    -- Check alerts and notify.
    checkTeamAlerts profileId pool
