module TeamTavern.Player.Register.Run where

import Prelude

import Async (Async, alwaysRight, fromEffect)
import Effect.Console (log)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (onMatch)
import MultiMap (empty)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Run (interpret)
import Run as VariantF
import Simple.JSON (writeJSON)
import TeamTavern.Architecture.Async (examineErrorWith)
import TeamTavern.Infrastructure.Cookie (setCookieHeader)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF(..))
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (ensureNotSignedIn)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Register (RegisterF(..), register)
import TeamTavern.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Player.Register.Error (RegisterError, logError)
import TeamTavern.Player.Register.ErrorModel (fromRegisterPlayerErrors)
import TeamTavern.Player.Register.GenerateToken (generateToken)
import TeamTavern.Player.Register.ReadIdentifiers (readIdentifiers)
import TeamTavern.Player.Register.SendEmail (sendRegistrationEmail)
import TeamTavern.Player.Register.ValidateIdentifiers (validateIdentifiers)
import Unsafe.Coerce (unsafeCoerce)

interpretRegister ::
    Pool -> Maybe Client -> Map String String -> Body -> Async RegisterError Credentials
interpretRegister pool client cookies body = register # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , register: case _ of
        ReadIdentifiers sendModel ->
            readIdentifiers body <#> sendModel
        ValidateIdentifiers model sendIdentifiers ->
            validateIdentifiers model <#> sendIdentifiers
        GenerateToken identifiers sendToken ->
            generateToken identifiers <#> sendToken
        AddPlayer credentials send ->
            addPlayer pool credentials <#> const send
        NotifyPlayer credentials send ->
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
    # onMatch
        { ensureNotSignedIn: \error' ->
            { statusCode: 403
            , headers: empty
            , content: mempty
            }
        , sendEmail: \{ credentials: { email, nickname, token } } ->
            { statusCode: 200
            , headers: setCookieHeader token
            , content: writeJSON
                { email: unwrap email
                , nickname: unwrap nickname
                , sendEmailError: true
                }
            }
        }
        (\rest ->
            { statusCode: 400
            , headers: empty
            , content: writeJSON rest
            })

successResponse :: Credentials -> Response
successResponse { email, nickname, token } =
    { statusCode: 200
    , headers: setCookieHeader token
    , content: writeJSON
        { email: unwrap email
        , nickname: unwrap nickname
        , sendEmailError: false
        }
    }

respondRegister ::
    Async RegisterError Credentials -> (forall left. Async left Response)
respondRegister = alwaysRight errorResponse successResponse

handleRegister ::
    Pool -> Maybe Client -> Map String String -> Body -> (forall left. Async left Response)
handleRegister pool client cookies body =
    interpretRegister pool client cookies body
    # examineErrorWith logError
    # respondRegister
