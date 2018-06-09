module TeamTavern.Player.Register.Run where

import Prelude

import Async (Async, alwaysRight, fromEffect)
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), on)
import Data.Variant as Variant
import Effect (Effect)
import Error.Class (message, name)
import Node.Errors.Class (code)
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
import TeamTavern.Player.Register.Database (addPlayer)
import TeamTavern.Player.Register.Errors (RegisterError, fromRegisterPlayerErrors)
import TeamTavern.Player.Register.Identifiers (readIdentifiers, validateIdentifiers)
import TeamTavern.Player.Register.SendEmail (sendRegistrationEmail)
import TeamTavern.Player.Register.Token (generateToken)
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

logError :: RegisterError -> Effect Unit
logError registerError = unsafeCoerce do
    log "Error registering player"
    registerError # Variant.match
        { model: \{ errors, body } -> do
            log $ "\tCouldn't read identifiers from body: " <> show body
            log $ "\tParsing resulted in these errors: " <> show errors
        , validation: \{ errors, model } ->
            log $ "\tFailed identifiers validation for identifiers: "
                <> model.email <> ", " <> model.nickname
        , token: \{ error, identifiers } -> do
            log $ "\tCouldn't generate token for identifiers: "
                <> unwrap identifiers.email <> ", "
                <> unwrap identifiers.nickname
            log $ "\tGenerating token resulted in this error: "
                <> code error <> ", " <> name error <> ", "
                <> message error
        , database: Variant.match
            { other: \{ error, credentials } -> do
                log $ "\tCouldn't add to database player with credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
                log $ "\tAdding to database resulted in this error: "
                    <> code error <> ", " <> name error <> ", "
                    <> message error
            , emailTaken: \{ error, credentials } ->
                log $ "\tEmail is already taken for credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
            , nicknameTaken: \{ error, credentials } ->
                log $ "\tNickname is already taken for credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
            }
        , sendEmail: \{ error, credentials } -> do
            log $ "\tCouldn't send email to player with credentials: "
                <> unwrap credentials.email <> ", "
                <> unwrap credentials.nickname <> ", "
                <> unwrap credentials.token
            log $ "\tEmail sending resulted in this error: "
                <> show error.status <> ", " <> show error.code <> ", "
                <> error.message
        }

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
