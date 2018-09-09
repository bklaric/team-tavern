module TeamTavern.Profile.Update.UpdateProfile
    (UpdateProfileError, updateProfile) where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Player.Domain.Types (AuthInfo)
import TeamTavern.Profile.Domain.Summary (Summary)
import TeamTavern.Profile.Domain.Types (Identifiers)

type UpdateProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { auth :: AuthInfo
        , identifiers :: Identifiers
        }
    | errors )

updateProfileQuery :: Query
updateProfileQuery = Query """
    update profile
    set summary = $5
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    and session.player_id = player.id
    and player.id = profile.player_id
    and game.id = profile.game_id
    and player.nickname = $3
    and game.handle = $4
    """

updateProfileParameters ::
    AuthInfo -> Identifiers -> Summary -> Array QueryParameter
updateProfileParameters { id, token } { nickname, handle } summary =
    [ toString id
    , unwrap token
    , unwrap nickname
    , unwrap handle
    , unwrap summary
    ]
    <#> QueryParameter

updateProfile
    :: forall errors
    .  Pool
    -> AuthInfo
    -> Identifiers
    -> Summary
    -> Async (UpdateProfileError errors) Unit
updateProfile pool auth identifiers summary = do
    result <- pool
        # query updateProfileQuery
            (updateProfileParameters auth identifiers summary)
        # label (SProxy :: SProxy "databaseError")
    if rowCount result == 1
        then pure unit
        else left $ inj (SProxy :: SProxy "notAuthorized") { auth, identifiers }
