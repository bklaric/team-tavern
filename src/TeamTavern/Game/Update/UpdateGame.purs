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
import Postgres.Query (Query(..), QueryParameter, toQueryParameter)
import Postgres.Result (rowCount)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Title (Title)
import TeamTavern.Game.Infrastructure.ReadModel (GameModel)
import TeamTavern.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Player.Domain.Id (toString)

updateGameQuery :: Query
updateGameQuery = Query """
    update game
    set title = $4, handle = $5, description = $6
    from session
    where game.administrator_id = $1
    and game.handle = $3
    and session.player_id = $1
    and session.token = $2
    and session.revoked = false
    """

updateGameQueryParameters ::
    CookieInfo -> Handle -> GameModel -> Array QueryParameter
updateGameQueryParameters
    { id, token } targetHandle { title, handle, description } =
    [ toString id
    , unwrap token
    , unwrap targetHandle
    , unwrap title
    , unwrap handle
    , unwrap description
    ]
    <#> toQueryParameter

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
    , notAuthorized :: CookieInfo
    | errors )

updateGame
    :: forall errors
    .  Pool
    -> CookieInfo
    -> Handle
    -> GameModel
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
