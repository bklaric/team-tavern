module TeamTavern.Server.Profile.Update.UpdateProfile
    (UpdateProfileError, updateProfile) where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Routes (Identifiers)

type UpdateProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { auth :: CookieInfo
        , identifiers ::
            { handle :: Handle
            , nickname :: Nickname
            }
        }
    | errors )

queryString :: Query
queryString = Query """
    update profile
    set summary = $5
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and player.id = profile.player_id
    and game.id = profile.game_id
    and player.nickname = $3
    and game.handle = $4
    """

queryParameters ::
    CookieInfo -> Identifiers -> Summary -> Array QueryParameter
queryParameters { id, token } { nickname, handle } summary =
    id : token : nickname : handle :| summary

updateProfile
    :: forall errors
    .  Pool
    -> CookieInfo
    -> Identifiers
    -> Summary
    -> Async (UpdateProfileError errors) Unit
updateProfile pool auth identifiers summary = do
    result <- pool
        # query queryString
            (queryParameters auth identifiers summary)
        # label (SProxy :: SProxy "databaseError")
    if rowCount result == 1
        then pure unit
        else left $ inj (SProxy :: SProxy "notAuthorized") { auth, identifiers }
