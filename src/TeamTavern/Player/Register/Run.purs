module TeamTavern.Player.Register.Run where

import Prelude

import Async (Async, alwaysRight, fromEffect)
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), on)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Run (interpret)
import Run as VariantF
import Simple.JSON (writeJSON)
import TeamTavern.Architecture.Async (examineErrorWith)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Register (RegisterF(..), register)
import TeamTavern.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Player.Register.Error (RegisterError, logError)
import TeamTavern.Player.Register.ErrorModel (fromRegisterPlayerErrors)
import TeamTavern.Player.Register.ReadIdentifiers (readIdentifiers)
import TeamTavern.Player.Register.SendEmail (sendRegistrationEmail)
import TeamTavern.Player.Register.GenerateToken (generateToken)
import TeamTavern.Player.Register.ValidateIdentifiers (validateIdentifiers)
import Unsafe.Coerce (unsafeCoerce)

interpretRegister ::
    Pool -> Maybe Client -> Body -> Async RegisterError Credentials
interpretRegister pool client body = register # interpret (VariantF.match
    { register: case _ of
        ReadIdentifiers sendModel ->
            readIdentifiers body <#> sendModel
        ValidateIdentifiers model sendIdentifiers ->
            validateIdentifiers model <#> sendIdentifiers
        GenerateToken identifiers sendToken ->
            generateToken identifiers <#> sendToken
        AddPlayer credentials send -> do
            addPlayer pool credentials <#> const send
        SendEmail credentials send ->
            case client of
            Just client' ->
                sendRegistrationEmail client' credentials <#> const send
            Nothing ->
                "Sent email *wink wink* to " <> unwrap credentials.email
                # unsafeCoerce log # fromEffect <#> const send
    })

errorResponse :: RegisterError -> Response
errorResponse error =
    fromRegisterPlayerErrors error
    # on
        (SProxy :: SProxy "sendEmail")
        (\{ credentials } ->
            { statusCode: 200
            , content: writeJSON
                { email: unwrap credentials.email
                , nickname: unwrap credentials.nickname
                , sendEmailError: true
                }
            })
        (\rest ->
            { statusCode: 400
            , content: writeJSON rest
            })

successResponse :: Credentials -> Response
successResponse credentials =
    { statusCode: 200
    , content: writeJSON
        { email: unwrap credentials.email
        , nickname: unwrap credentials.nickname
        }
    }

respondRegister ::
    Async RegisterError Credentials -> (forall left. Async left Response)
respondRegister = alwaysRight errorResponse successResponse

handleRegister ::
    Pool -> Maybe Client -> Body -> (forall left. Async left Response)
handleRegister pool client body =
    interpretRegister pool client body
    # examineErrorWith logError
    # respondRegister
