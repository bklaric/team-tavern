module TeamTavern.Player.Register where

import Prelude

import Async (Async, runAsync)
import Data.Array (fromFoldable)
import Data.Either (either)
import Data.Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(SProxy), Variant, inj, match)
import Effect (Effect)
import Node.Errors (Error)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Query (class Querier)
import Simple.JSON (writeJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Email (EmailError)
import TeamTavern.Player.Nickname (NicknameError)
import TeamTavern.Player.Register.Database (addPlayer)
import TeamTavern.Player.Register.PlayerToRegister (PlayerToRegister, ValidationError)
import TeamTavern.Player.Register.PlayerToRegister as PlayerToRegister
import TeamTavern.Player.Register.PlayerToRegisterModel (readPlayerToRegisterModel)

type RegisterPlayerError = Variant
    ( model :: NonEmptyList ForeignError
    , validation :: NonEmptyList ValidationError
    , token :: Error
    , database :: Error
    )

type RegisterPlayerErrorsModel = Variant
    ( validation :: Array (Variant
        ( email ∷ Array EmailError
        , nickname ∷ Array NicknameError
        ))
    , emailTaken :: { email :: String }
    , nicknameTaken :: { nickname :: String }
    , other :: {}
    )

fromRegisterPlayerErrors :: RegisterPlayerError -> RegisterPlayerErrorsModel
fromRegisterPlayerErrors = match
    { model: const $ inj (SProxy :: SProxy "other") {}
    , validation: fromFoldable
        >>> map (match
            { email: fromFoldable >>> inj (SProxy :: SProxy "email")
            , nickname: fromFoldable >>> inj (SProxy :: SProxy "nickname")
            })
        >>> inj (SProxy :: SProxy "validation")
    , token: const $ inj (SProxy :: SProxy "other") {}
    , database: const $ inj (SProxy :: SProxy "other") {}
    }

register :: forall querier. Querier querier =>
    querier -> Body -> Async RegisterPlayerError PlayerToRegister
register querier body = do
    bodyString <- readBody body
    playerToRegisterModel <- readPlayerToRegisterModel bodyString
    playerToRegister <- PlayerToRegister.create playerToRegisterModel
    addPlayer querier playerToRegister

registerPlayerHandler :: forall querier. Querier querier =>
    querier -> Body -> (Response -> Effect Unit) -> Effect Unit
registerPlayerHandler querier body respond = (runAsync $ register querier body)
    (either
        (\error -> respond { statusCode: 400, content: error # fromRegisterPlayerErrors # writeJSON })
        (\player -> respond { statusCode: 200, content: "Looks good: " <> unwrap player.email <> ", " <> unwrap player.nickname }))
