module TeamTavern.Game.Create.AddGame where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Name (Name)
import TeamTavern.Game.Domain.Types (Details)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Player.Domain.Types (AuthInfo)

addGameQuery :: Query
addGameQuery = Query """
    insert into game (administrator_id, name, handle, description)
    select player.id, $3, $4, $5
    from player
    join session on session.player_id = player.id
    where player.nickname = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    """

addGameQueryParameters :: AuthInfo -> Details -> Array QueryParameter
addGameQueryParameters { id, token } { name, handle, description } =
    [ toString id
    , unwrap token
    , unwrap name
    , unwrap handle
    , unwrap description
    ]
    <#> QueryParameter

type AddGameError errors = Variant
    ( nameTaken ::
        { name :: Name
        , error :: Error
        }
    , handleTaken ::
        { handle :: Handle
        , error :: Error
        }
    , databaseError :: Error
    , notAuthorized :: AuthInfo
    | errors )

addGame :: forall errors.
    Pool -> AuthInfo -> Details -> Async (AddGameError errors) Unit
addGame pool auth details = do
    result <- pool
        # query addGameQuery (addGameQueryParameters auth details)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "game_name_key" ->
                inj (SProxy :: SProxy "nameTaken")
                { name: details.name, error }
            true | constraint error == Just "game_handle_key" ->
                inj (SProxy :: SProxy "handleTaken")
                { handle: details.handle, error }
            _ -> inj (SProxy :: SProxy "databaseError") error)
    if rowCount result == 1
        then pure unit
        else Async.left $ inj (SProxy :: SProxy "notAuthorized") auth
