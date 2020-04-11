module TeamTavern.Server.Profile.AddGameTeam.AddProfile where

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
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (nullableTimeFrom, nullableTimeTo)
import TeamTavern.Server.Profile.AddGameTeam.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddGameTeam.ValidateAgeSpan (nullableAgeFrom, nullableAgeTo)
import TeamTavern.Server.Profile.AddGameTeam.ValidateProfile (Profile)
import TeamTavern.Server.Profile.Routes (Handle)

type AddProfileError errors = Variant
    ( databaseError :: Error
    , nothingInserted ::
        { cookieInfo :: CookieInfo
        , handle :: Handle
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

queryString :: Query
queryString = Query """
    insert into team_profile
        ( player_id
        , game_id
        , summary
        , age_from
        , age_to
        , languages
        , regions
        , timezone
        , weekday_from
        , weekday_to
        , weekend_from
        , weekend_to
        , has_microphone
        )
    select $1, game.id, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13
    from game
    where game.handle = $2
    returning team_profile.id as "profileId"
    """

queryParameters :: CookieInfo -> Handle -> Profile -> Array QueryParameter
queryParameters { id } handle profile =
    id
    : handle
    : profile.summary
    : nullableAgeFrom profile.ageSpan
    : nullableAgeTo profile.ageSpan
    : profile.languages
    : profile.regions
    : profile.timezone
    : nullableTimeFrom profile.onlineWeekday
    : nullableTimeTo profile.onlineWeekday
    : nullableTimeFrom profile.onlineWeekend
    : nullableTimeTo profile.onlineWeekend
    :| profile.hasMicrophone

addProfile'
    :: forall errors
    .  Client
    -> CookieInfo
    -> Handle
    -> Profile
    -> Async (AddProfileError errors) ProfileId
addProfile' client cookieInfo handle profile = do
    result <- client
        # query queryString (queryParameters cookieInfo handle profile)
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- Result.rows result
        # head
        # Async.note (inj
            (SProxy :: SProxy "nothingInserted") { cookieInfo, handle })
        >>= (read >>> labelMap
            (SProxy :: SProxy "unreadableProfileId") { result, errors: _ })
    pure profileId

addProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Handle
    -> Profile
    -> Async (AddProfileError errors) Unit
addProfile client cookieInfo handle profile = do
    profileId <- addProfile' client cookieInfo handle profile
    addFieldValues client profileId profile.fieldValues
