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
import TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary (Summary)
import TeamTavern.Server.Profile.Routes (Identifiers)

type AddProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { playerId :: Int
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
    select $1, game.id, $3, $4
    from game
    where game.handle = $2
    returning player_profile.id as "profileId";
    """

addProfileParameters ::
    Int -> Identifiers -> Summary -> Boolean -> Array QueryParameter
addProfileParameters playerId { handle, nickname } summary newOrReturning =
    playerId : handle : summary :| newOrReturning

addProfile'
    :: forall errors
    .  Client
    -> Int
    -> Identifiers
    -> Summary
    -> Boolean
    -> Async (AddProfileError errors) ProfileId
addProfile' client playerId identifiers summary newOrReturning = do
    result <- client
        # query addProfileString
            (addProfileParameters playerId identifiers summary newOrReturning)
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- Result.rows result
        # head
        # Async.note (inj
            (SProxy :: SProxy "notAuthorized") { playerId, identifiers })
        >>= (read >>> labelMap
            (SProxy :: SProxy "unreadableProfileId") { result, errors: _ })
    pure profileId

addProfile
    :: forall errors
    .  Client
    -> Int
    -> Identifiers
    -> Profile
    -> Async (AddProfileError errors) Unit
addProfile client playerId identifiers (Profile summary fieldValues newOrReturning) = do
    profileId <- addProfile' client playerId identifiers summary newOrReturning
    addFieldValues client profileId fieldValues
    pure unit
