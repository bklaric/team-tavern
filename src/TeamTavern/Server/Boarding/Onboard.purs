module TeamTavern.Server.Boarding.Onboard (onboard) where

import Prelude

import Async (Async)
import Async as Async
import AsyncV as AsyncV
import Data.Array (elem)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (inj)
import Jarilo (badRequest_, ok_)
import Postgres.Pool (Pool)
import Record.Extra (pick)
import TeamTavern.Routes.Boarding.Onboard as Onboard
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (validateContactsV)
import TeamTavern.Server.Player.UpdateContacts.WriteContacts (writeContacts)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as Player
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile as AddTeamProfile
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as Team
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as TeamProfile
import TeamTavern.Server.Team.Create (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts as TeamCont
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (validateTeamV)
import TeamTavern.Server.Team.Infrastructure.WriteContacts as TeamIdunno
import Type.Proxy (Proxy(..))

onboard :: ∀ left. Pool -> Cookies -> Onboard.RequestContent -> Async left _
onboard pool cookies content =
    sendResponse "Error onboarding" do

    -- Ensure the player is signed in.
    cookieInfo <- ensureSignedIn pool cookies

    -- Start the transaction.
    result <- pool # transaction \client ->
        case content of
        { ilk: 1, player: Just player, playerProfile: Just profile, playerContacts: Just contacts } -> do
            -- Read fields from database.
            game <- Player.loadFields client content.gameHandle

            -- We only want to patch the selected platform contact.
            let contactsCleaned = contacts
                    { steamId         = if profile.platform == Steam       then contacts.steamId         else Nothing
                    , riotId          = if profile.platform == Riot        then contacts.riotId          else Nothing
                    , battleTag       = if profile.platform == BattleNet   then contacts.battleTag       else Nothing
                    , eaId            = if profile.platform == Origin      then contacts.eaId            else Nothing
                    , ubisoftUsername = if profile.platform == Ubisoft     then contacts.ubisoftUsername else Nothing
                    , psnId           = if profile.platform == PlayStation then contacts.psnId           else Nothing
                    , gamerTag        = if profile.platform == Xbox        then contacts.gamerTag        else Nothing
                    , friendCode      = if profile.platform == Switch      then contacts.friendCode      else Nothing
                    }

            { player', profile', contacts' } <-
                { player': _, profile': _, contacts': _ }
                <$> validatePlayerV player
                <*> validateProfileV game profile (Proxy :: _ "playerProfile")
                <*> validateContactsV [ profile.platform ] contactsCleaned (Proxy :: _ "playerContacts")
                # AsyncV.toAsync
                # lmap (map badRequest_)
            updateDetails client (unwrap cookieInfo.id) player'
            profileId <- addProfile client (unwrap cookieInfo.id)
                { handle: content.gameHandle
                , nickname: unwrap cookieInfo.nickname
                }
                profile'
            writeContacts client (unwrap cookieInfo.id) contacts'
            pure { teamHandle: Nothing, profileId }
        { ilk: 2, team: Just team, teamProfile: Just profile, teamContacts: Just contacts } -> do
            -- Read fields from database.
            game <- Team.loadFields client content.gameHandle

            -- We only want to patch the selected platforms contacts.
            let contactsCleaned = contacts
                    { steamId         = if Steam       `elem` profile.platforms then contacts.steamId         else Nothing
                    , riotId          = if Riot        `elem` profile.platforms then contacts.riotId          else Nothing
                    , battleTag       = if BattleNet   `elem` profile.platforms then contacts.battleTag       else Nothing
                    , eaId            = if Origin      `elem` profile.platforms then contacts.eaId            else Nothing
                    , ubisoftUsername = if Ubisoft     `elem` profile.platforms then contacts.ubisoftUsername else Nothing
                    , psnId           = if PlayStation `elem` profile.platforms then contacts.psnId           else Nothing
                    , gamerTag        = if Xbox        `elem` profile.platforms then contacts.gamerTag        else Nothing
                    , friendCode      = if Switch      `elem` profile.platforms then contacts.friendCode      else Nothing
                    }

            { team', profile', contacts' } <-
                { team': _, profile': _, contacts': _ }
                <$> validateTeamV team
                <*> TeamProfile.validateProfileV game profile (Proxy :: _ "teamProfile")
                <*> TeamCont.validateContactsV profile.platforms contactsCleaned (Proxy :: _ "teamContacts")
                # AsyncV.toAsync
                # lmap (map badRequest_)
            let generatedHandle = generateHandle team'.organization cookieInfo.nickname
            { id, handle } <- addTeam client cookieInfo.id generatedHandle team'
            profileId <- AddTeamProfile.addProfile
                client cookieInfo.id handle content.gameHandle profile'
            TeamIdunno.writeContacts client id contacts'
            pure { teamHandle: Just handle, profileId }
        _ -> Async.left $ Terror (badRequest_ $ Nea.singleton $ inj (Proxy :: _ "other") {}) []

    -- case result.teamHandle of
    --     Nothing -> checkPlayerAlerts result.profileId pool
    --     Just _ -> checkTeamAlerts result.profileId pool

    pure $ ok_ (pick result :: Onboard.OkContent)
