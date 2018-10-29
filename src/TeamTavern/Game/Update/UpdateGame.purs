module TeamTavern.Game.Update.UpdateGame where

import Prelude

import Async (Async)
import Async (left) as Async
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
import TeamTavern.Game.Domain.Title (Title)
import TeamTavern.Game.Domain.Types (Details)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Player.Domain.Types (AuthInfo)

updateGameQuery :: Query
updateGameQuery = Query """
    update game
    set title = $4, handle = $5, description = $6
    from session
    where game.administrator_id = $1
    and game.handle = $3
    and session.player_id = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    """

updateGameQueryParameters ::
    AuthInfo -> Handle -> Details -> Array QueryParameter
updateGameQueryParameters
    { id, token } targetHandle { title, handle, description } =
    [ toString id
    , unwrap token
    , unwrap targetHandle
    , unwrap title
    , unwrap handle
    , unwrap description
    ]
    <#> QueryParameter

type UpdateGameError errors = Variant
    ( titleTaken ::
        { title :: Title
        , error :: Error
        }
    , handleTaken ::
        { handle :: Handle
        , error :: Error
        }
    , databaseError :: Error
    , notAuthorized :: AuthInfo
    | errors )

updateGame
    :: forall errors
    .  Pool
    -> AuthInfo
    -> Handle
    -> Details
    -> Async (UpdateGameError errors) Unit
updateGame pool auth targetHandle details = do
    result <- pool
        # query updateGameQuery
            (updateGameQueryParameters auth targetHandle details)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "game_title_key" ->
                inj (SProxy :: SProxy "titleTaken")
                { title: details.title, error }
            true | constraint error == Just "game_handle_key" ->
                inj (SProxy :: SProxy "handleTaken")
                { handle: details.handle, error }
            _ -> inj (SProxy :: SProxy "databaseError") error)
    if rowCount result == 1
        then pure unit
        else Async.left $ inj (SProxy :: SProxy "notAuthorized") auth
