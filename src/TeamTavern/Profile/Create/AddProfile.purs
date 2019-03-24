module TeamTavern.Profile.Create.AddProfile
    (AddProfileError, addProfile) where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Profile.Domain.Summary (Summary)

type AddProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , handle :: Handle
        }
    | errors )

queryString :: Query
queryString = Query """
    insert into profile (player_id, game_id, summary)
    select player.id, game.id, $4
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and game.handle = $3
    """

queryParameters ::
    CookieInfo -> Handle -> Summary -> Array QueryParameter
queryParameters { id, token } handle summary = id : token : handle :| summary

addProfile
    :: forall errors
    .  Pool
    -> CookieInfo
    -> Handle
    -> Summary
    -> Async (AddProfileError errors) Unit
addProfile pool cookieInfo handle summary = do
    result <- pool
        # query queryString (queryParameters cookieInfo handle summary)
        # label (SProxy :: SProxy "databaseError")
    if rowCount result == 1
        then pure unit
        else left
            $ inj (SProxy :: SProxy "notAuthorized") { cookieInfo, handle }
