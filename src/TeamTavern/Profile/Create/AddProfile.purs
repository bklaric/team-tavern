module TeamTavern.Profile.Create.AddProfile where

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
import TeamTavern.Player.Domain.Types (IdentifiedToken')
import TeamTavern.Profile.Domain.Summary (Summary)
import TeamTavern.Profile.Domain.Types (Identifiers)

type AddProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { auth :: IdentifiedToken'
        , identifiers :: Identifiers
        }
    | errors )

addProfileQuery :: Query
addProfileQuery = Query """
    insert into profile (player_id, game_id, summary)
    select player.id, game.id, $5
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    and session.player_id = player.id
    and player.nickname = $3
    and game.handle = $4
    """

addProfileParameters ::
    IdentifiedToken' -> Identifiers -> Summary -> Array QueryParameter
addProfileParameters { id, token } { nickname, handle } summary =
    [ toString id
    , unwrap token
    , unwrap nickname
    , unwrap handle
    , unwrap summary
    ]
    <#> QueryParameter

addProfile
    :: forall errors
    .  Pool
    -> IdentifiedToken'
    -> Identifiers
    -> Summary
    -> Async (AddProfileError errors) Unit
addProfile pool auth identifiers summary = do
    result <- pool
        # query addProfileQuery (addProfileParameters auth identifiers summary)
        # label (SProxy :: SProxy "databaseError")
    if rowCount result == 1
        then pure unit
        else left $ inj (SProxy :: SProxy "notAuthorized") { auth, identifiers }
