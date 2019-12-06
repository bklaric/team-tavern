module TeamTavern.Server.Profile.Create.AddProfile where

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
import TeamTavern.Server.Profile.Infrastructure.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.Infrastructure.ValidateProfile (Profile(..))
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
    insert into profile (player_id, game_id, type, summary)
    select player.id, game.id, $5, $6
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and game.handle = $3
    and lower(player.nickname) = lower($4)
    returning profile.id as "profileId";
    """

addProfileParameters ::
    CookieInfo -> Identifiers -> Int -> Summary -> Array QueryParameter
addProfileParameters { id, token } { handle, nickname } profileType summary =
    id : token : handle : nickname : profileType :| summary

addProfile'
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> Int
    -> Summary
    -> Async (AddProfileError errors) ProfileId
addProfile' client cookieInfo identifiers profileType summary = do
    result <- client
        # query addProfileString
            (addProfileParameters cookieInfo identifiers profileType summary)
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
addProfile client cookieInfo identifiers (Profile profileType summary fieldValues) = do
    profileId <- addProfile' client cookieInfo identifiers profileType summary
    addFieldValues client profileId fieldValues
    pure unit
