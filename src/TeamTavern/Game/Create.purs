module TeamTavern.Game.Create where

import Prelude

import Async (Async)
import Async (examineLeftWithEffect, left, note) as Async
import Async.Validated (fromValidated) as Async
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..))
import Data.Variant as Variant
import Node.Errors.Class (code)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Error as PgE
import Postgres.Error.Codes as PgEC
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result as PgR
import Simple.JSON.Async (readJSON) as Async
import TeamTavern.Architecture.Perun.Request.Body as Perun
import TeamTavern.Architecture.Postgres.Query as Pg
import Data.Validated.Label as Validated
import TeamTavern.Game.Create.LogError (logError) as Create
import TeamTavern.Game.Create.Response (response) as Create
import TeamTavern.Game.Create.Types (CreateError, DetailsError)
import TeamTavern.Game.Domain.Description (Description)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Name (Name)
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Game.Domain.Types (Details)
import TeamTavern.Infrastructure.Cookie as Cookie
import TeamTavern.Player.Domain.Types (NicknamedToken)
import Data.Validated (Validated)

readAdmin :: Map String String -> Async CreateError NicknamedToken
readAdmin cookies = Cookie.lookupAuthCookies cookies
    # Async.note (Variant.inj (SProxy :: SProxy "cookiesNotPresent") cookies)

validateName :: String -> Validated (NonEmptyList DetailsError) Name
validateName name = Name.create name # Validated.label (SProxy :: SProxy "name")

validateHandle :: String -> Validated (NonEmptyList DetailsError) Handle
validateHandle handle = Handle.create handle
    # Validated.label (SProxy :: SProxy "handle")

validateDescription ::
    String -> Validated (NonEmptyList DetailsError) Description
validateDescription description = Description.create description
    # Validated.label (SProxy :: SProxy "description")

readDetails :: Body -> Async CreateError Details
readDetails body = do
    content <- Perun.readBody body
    { name, handle, description } ::
        { name :: String, handle :: String, description :: String } <-
        Async.readJSON content # labelMap
            (SProxy :: SProxy "cantReadDetailsModel") { content, errors: _ }
    { name: _, handle: _, description: _ }
        <$> validateName name
        <*> validateHandle handle
        <*> validateDescription description
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "cantValidateDetails")
            { name, handle, description, errors: _ }

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

addGameQueryParameters :: NicknamedToken -> Details -> Array QueryParameter
addGameQueryParameters { nickname, token } { name, handle, description } =
    [ unwrap nickname
    , unwrap token
    , unwrap name
    , unwrap handle
    , unwrap description
    ]
    <#> QueryParameter

addGame :: Pool -> NicknamedToken -> Details -> Async CreateError Unit
addGame pool nicknamedToken details = do
    result <- pool
        # Pg.query addGameQuery (addGameQueryParameters nicknamedToken details)
        # lmap (\error ->
            case code error == PgEC.unique_violation of
            true | PgE.constraint error == Just "game_name_key" ->
                Variant.inj (SProxy :: SProxy "nameTaken")
                { name: details.name, error }
            _ -> Variant.inj (SProxy :: SProxy "databaseError") error)
    if PgR.rowCount result == 1
        then pure unit
        else Async.left $ Variant.inj (SProxy :: SProxy "notAuthorized") unit

handleCreate
    :: Pool
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleCreate pool cookies body =
    Create.response $ Async.examineLeftWithEffect Create.logError do
    -- Read player creating the game.
    nicknamedToken <- readAdmin cookies

    -- Read game name and description.
    details <- readDetails body

    -- Add game to database.
    addGame pool nicknamedToken details
