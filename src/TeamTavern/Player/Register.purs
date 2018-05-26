module TeamTavern.Player.Register where

import Prelude

import TeamTavern.Architecture.Perun.Request.Body (readBody)
import Async (Async, runAsync)
import Data.Either (either)
import Data.Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Effect (Effect)
import Node.Errors (Error)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Query (class Querier)
import TeamTavern.Player.Register.Database (addPlayer)
import TeamTavern.Player.Register.PlayerToRegister (PlayerToRegister, ValidationErrors)
import TeamTavern.Player.Register.PlayerToRegister as PlayerToRegister
import TeamTavern.Player.Register.PlayerToRegisterModel (readPlayerToRegisterModel)

type RegisterPlayerErrors errors = (Variant
    ( model :: NonEmptyList ForeignError
    , validation :: ValidationErrors
    , token :: Error
    , database :: Error
    | errors))

register
    :: forall querier errors
    .  Querier querier
    => querier
    -> Body
    -> Async (RegisterPlayerErrors errors) PlayerToRegister
register querier body = do
    bodyString <- readBody body
    playerToRegisterModel <- readPlayerToRegisterModel bodyString
    playerToRegister <- PlayerToRegister.create playerToRegisterModel
    addPlayer querier playerToRegister

registerPlayerHandler :: forall querier. Querier querier =>
    querier -> Body -> (Response -> Effect Unit) -> Effect Unit
registerPlayerHandler querier body respond = (runAsync $ register querier body)
    (either
        (\error -> respond { statusCode: 400, content: "Error inserting in the database."  })
        (\player -> respond { statusCode: 200, content: "Looks good: " <> unwrap player.email <> ", " <> unwrap player.nickname }))
