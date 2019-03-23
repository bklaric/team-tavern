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
import Postgres.Query (Query(..), QueryParameter, toQueryParameter)
import Postgres.Result (rowCount)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Player.Domain.Id (toString)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Profile.Domain.Summary (Summary)
import TeamTavern.Profile.Routes (Identifiers)
import Unsafe.Coerce (unsafeCoerce)

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

updateProfileQuery :: Query
updateProfileQuery = Query """
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

updateProfileParameters ::
    CookieInfo -> Identifiers -> Summary -> Array QueryParameter
updateProfileParameters { id, token } { nickname, handle } summary =
    [ toString id
    , unwrap token
    , unwrap nickname
    , unwrap handle
    , unwrap summary <#> unwrap # unsafeCoerce
    ]
    <#> toQueryParameter

updateProfile
    :: forall errors
    .  Pool
    -> CookieInfo
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
