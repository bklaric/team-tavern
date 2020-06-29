module TeamTavern.Server.Profile.AddPlayerProfile.AddProfile where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (Result)
import Postgres.Result as Result
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary (Summary)
import TeamTavern.Server.Profile.Routes (Identifiers)

type AddProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , identifiers :: Identifiers
        }
    , unreadableProfileId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , emptyResult ::
        { result :: Result
        }
    , unreadableFieldValueId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

addProfileString :: Query
addProfileString = Query """
    insert into player_profile (player_id, game_id, summary, new_or_returning)
    select player.id, game.id, $5, $6
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and game.handle = $3
    and lower(player.nickname) = lower($4)
    returning player_profile.id as "profileId";
    """

addProfileParameters ::
    CookieInfo -> Identifiers -> Summary -> Boolean -> Array QueryParameter
addProfileParameters { id, token } { handle, nickname } summary newOrReturning =
    id : token : handle : nickname : summary :| newOrReturning

addProfile'
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> Summary
    -> Boolean
    -> Async (AddProfileError errors) ProfileId
addProfile' client cookieInfo identifiers summary newOrReturning = do
    result <- client
        # query addProfileString
            (addProfileParameters cookieInfo identifiers summary newOrReturning)
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- Result.rows result
        # head
        # Async.note (inj
            (SProxy :: SProxy "notAuthorized") { cookieInfo, identifiers })
        >>= (read >>> labelMap
            (SProxy :: SProxy "unreadableProfileId") { result, errors: _ })
    pure profileId

addProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> Profile
    -> Async (AddProfileError errors) Unit
addProfile client cookieInfo identifiers (Profile summary fieldValues newOrReturning) = do
    profileId <- addProfile' client cookieInfo identifiers summary newOrReturning
    addFieldValues client profileId fieldValues
    pure unit
