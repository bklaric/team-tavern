module TeamTavern.Profile.Create.AddProfile
    (AddProfileError, addProfile) where

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
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Player.Domain.Types (AuthInfo)
import TeamTavern.Profile.Domain.Summary (Summary)

type AddProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { auth :: AuthInfo
        , handle :: Handle
        }
    | errors )

addProfileQuery :: Query
addProfileQuery = Query """
    insert into profile (player_id, game_id, summary)
    select player.id, game.id, $4
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    and session.player_id = player.id
    and game.handle = $3
    """

addProfileParameters ::
    AuthInfo -> Handle -> Summary -> Array QueryParameter
addProfileParameters { id, token } handle summary =
    [ toString id
    , unwrap token
    , unwrap handle
    , unwrap summary
    ]
    <#> QueryParameter

addProfile
    :: forall errors
    .  Pool
    -> AuthInfo
    -> Handle
    -> Summary
    -> Async (AddProfileError errors) Unit
addProfile pool auth handle summary = do
    result <- pool
        # query addProfileQuery (addProfileParameters auth handle summary)
        # label (SProxy :: SProxy "databaseError")
    if rowCount result == 1
        then pure unit
        else left $ inj (SProxy :: SProxy "notAuthorized") { auth, handle }
