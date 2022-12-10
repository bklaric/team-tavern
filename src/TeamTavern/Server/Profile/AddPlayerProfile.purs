module TeamTavern.Server.Profile.AddPlayerProfile where

import Prelude

import Async (Async)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Jarilo (badRequest_, noContent_)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (validateContactsV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.Infrastructure.PatchPlayerContacts (patchPlayerContacts)
import Type.Proxy (Proxy(..))

addPlayerProfile :: ∀ left.
    Pool -> Map String String -> AddPlayerProfile.RouteParams -> _ -> Async left _
addPlayerProfile pool cookies identifiers profile' =
    sendResponse "Error creating player profile" do

    profileId <- pool # transaction \client -> do
        -- Read info info from cookies.
        cookieInfo <- ensureSignedInAs client cookies identifiers.nickname

        -- Load game fields from database.
        game <- loadFields client identifiers.handle

        -- We only want to patch the selected platform contact.
        let contacts' = profile'.contacts
                { steamId         = if profile'.details.platform == Steam       then profile'.contacts.steamId         else Nothing
                , riotId          = if profile'.details.platform == Riot        then profile'.contacts.riotId          else Nothing
                , battleTag       = if profile'.details.platform == BattleNet   then profile'.contacts.battleTag       else Nothing
                , eaId            = if profile'.details.platform == Origin      then profile'.contacts.eaId            else Nothing
                , ubisoftUsername = if profile'.details.platform == Ubisoft     then profile'.contacts.ubisoftUsername else Nothing
                , psnId           = if profile'.details.platform == PlayStation then profile'.contacts.psnId           else Nothing
                , gamerTag        = if profile'.details.platform == Xbox        then profile'.contacts.gamerTag        else Nothing
                , friendCode      = if profile'.details.platform == Switch      then profile'.contacts.friendCode      else Nothing
                }

        -- Validate profile and contacts.
        { profile, contacts } <-
            { profile: _, contacts: _ }
            <$> validateProfileV game profile'.details (Proxy :: _ "profile")
            <*> validateContactsV [ profile'.details.platform ] contacts' (Proxy :: _ "contacts")
            # AsyncV.toAsync
            # lmap (map badRequest_)

        -- Add profile to database.
        profileId <- addProfile client (unwrap cookieInfo.id) identifiers profile

        -- Update contacts.
        patchPlayerContacts client (unwrap cookieInfo.id) contacts

        pure profileId

    -- Check alerts and notify.
    -- checkPlayerAlerts profileId pool

    pure noContent_
