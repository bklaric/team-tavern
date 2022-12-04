module TeamTavern.Server.Profile.AddTeamProfile where

import Prelude

import Async (Async)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Foldable (elem)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Jarilo (badRequest_, noContent_)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Profile.AddTeamProfile as AddTeamProfile
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Server.Infrastructure.EnsureSignedInOwner (ensureSignedInOwner)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.Infrastructure.CheckTeamAlerts (checkTeamAlerts)
import TeamTavern.Server.Profile.Infrastructure.PatchTeamContacts (patchTeamContacts)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts (validateContactsV)
import Type.Proxy (Proxy(..))

addTeamProfile :: forall left.
    Pool -> Map String String -> AddTeamProfile.RouteParams -> _ -> Async left _
addTeamProfile pool cookies { teamHandle, gameHandle } profile' =
    sendResponse "Error creating player profile" do
    -- Read info from cookies.
    { cookieInfo } <- ensureSignedInOwner pool cookies teamHandle

    profileId <- pool # transaction \client -> do
        -- Load game fields from database.
        game <- loadFields client gameHandle

        -- We only want to patch the selected platforms contacts.
        let contacts' = profile'.contacts
                { steamId         = if Steam       `elem` profile'.details.platforms then profile'.contacts.steamId         else Nothing
                , riotId          = if Riot        `elem` profile'.details.platforms then profile'.contacts.riotId          else Nothing
                , battleTag       = if BattleNet   `elem` profile'.details.platforms then profile'.contacts.battleTag       else Nothing
                , eaId            = if Origin      `elem` profile'.details.platforms then profile'.contacts.eaId            else Nothing
                , ubisoftUsername = if Ubisoft     `elem` profile'.details.platforms then profile'.contacts.ubisoftUsername else Nothing
                , psnId           = if PlayStation `elem` profile'.details.platforms then profile'.contacts.psnId           else Nothing
                , gamerTag        = if Xbox        `elem` profile'.details.platforms then profile'.contacts.gamerTag        else Nothing
                , friendCode      = if Switch      `elem` profile'.details.platforms then profile'.contacts.friendCode      else Nothing
                }

        -- Validate profile and contacts.
        { profile, contacts } <-
            { profile: _, contacts: _ }
            <$> validateProfileV game profile'.details (Proxy :: _ "profile")
            <*> validateContactsV profile'.details.platforms contacts' (Proxy :: _ "contacts")
            # AsyncV.toAsync
            # lmap (map badRequest_)

        -- Add profile to database.
        profileId <- addProfile client cookieInfo.id teamHandle gameHandle profile

        -- Update contacts.
        patchTeamContacts client teamHandle contacts

        pure profileId

    -- Check alerts and notify.
    checkTeamAlerts profileId pool

    pure noContent_
