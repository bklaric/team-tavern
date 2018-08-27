module TeamTavern.Player.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async (fromEither, left, note) as Async
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Data.Variant (SProxy(..), inj)
import Node.Errors.Class (code)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Error (constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async (fromValidated, label) as Async
import TeamTavern.Architecture.Either as Either
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Architecture.Validated as Validated
import TeamTavern.Infrastructure.Cookie (lookupAuthCookies)
import TeamTavern.Player.Domain.About (About)
import TeamTavern.Player.Domain.About as About
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Types (Update, NicknamedToken)
import TeamTavern.Player.Update.LogError (logError) as Update
import TeamTavern.Player.Update.Response (response) as Update
import TeamTavern.Player.Update.Types (UpdateError', UpdateError)
import Validated (Validated)

readTargetNickname :: NonEmptyString -> Async UpdateError Nickname
readTargetNickname nickname' =
    nickname'
    # Nickname.fromNonEmpty'
    # Either.label (SProxy :: SProxy "cantValidateTargetNickname")
    # Async.fromEither

readRequestor :: Map String String -> Async UpdateError NicknamedToken
readRequestor cookies = lookupAuthCookies cookies
    # Async.note (inj (SProxy :: SProxy "cookiesNotPresent") unit)

ensureNicknamesSame :: Nickname -> Nickname -> Async UpdateError Unit
ensureNicknamesSame targetNickname requestorNickname =
    if targetNickname == requestorNickname
    then pure unit
    else Async.left $ inj (SProxy :: SProxy "nicknamesNotSame") unit

validateNickname :: String -> Validated (NonEmptyList UpdateError') Nickname
validateNickname nickname =
    Nickname.create nickname # Validated.label (SProxy :: SProxy "nickname")

validateAbout :: String -> Validated (NonEmptyList UpdateError') About
validateAbout about =
    About.create about # Validated.label (SProxy :: SProxy "about")

readUpdate :: Body -> Async UpdateError Update
readUpdate body = do
    content <- readBody body
    { nickname, about } :: { nickname :: String, about :: String } <-
        readJSON content
        # Either.label (SProxy :: SProxy "cantReadUpdateModel")
        # Async.fromEither
    { nickname: _, about: _ }
        <$> validateNickname nickname
        <*> validateAbout about
        # Async.fromValidated
        # Async.label (SProxy :: SProxy "cantValidateUpdate")

updatePlayerQuery :: Query
updatePlayerQuery = Query """
    update player
    set nickname = $3, about = $4
    from session
    where player.id = session.player_id
    and player.nickname = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    """

updatePlayerQueryParameters ::
    NicknamedToken -> Update -> Array QueryParameter
updatePlayerQueryParameters nicknamedToken update =
    [ unwrap nicknamedToken.nickname
    , unwrap nicknamedToken.token
    , unwrap update.nickname
    , unwrap update.about
    ]
    <#> QueryParameter

updatePlayer :: Pool -> NicknamedToken -> Update -> Async UpdateError Unit
updatePlayer pool nicknamedToken update = do
    result <- pool
        # query updatePlayerQuery
            (updatePlayerQueryParameters nicknamedToken update)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "player_nickname_key" ->
                inj (SProxy :: SProxy "nicknameTaken")
                { nickname: nicknamedToken.nickname, error }
            _ -> inj (SProxy :: SProxy "databaseError") error)
    if rowCount result == 1
        then pure unit
        else Async.left $ inj (SProxy :: SProxy "notAuthorized") unit

handleUpdate
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleUpdate pool targetNickname' cookies body =
    Update.response $ examineLeftWithEffect Update.logError do
    -- Read target nickname from route.
    targetNickname <- readTargetNickname targetNickname'

    -- Read requestor nickname and token from cookie.
    nicknamedToken @ { nickname, token } <- readRequestor cookies

    -- Ensure target and requestor nickname are the same.
    ensureNicknamesSame targetNickname nickname

    -- Read update from body.
    update <- readUpdate body

    -- Update player.
    updatePlayer pool nicknamedToken update
