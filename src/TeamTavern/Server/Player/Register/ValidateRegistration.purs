module TeamTavern.Server.Player.Register.ValidateRegistration where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as AsyncVal
import Data.Bifunctor (lmap)
import Data.Variant (Variant, inj, match)
import Jarilo (badRequest_)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNea)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import TeamTavern.Server.Infrastructure.ValidateEmail (Email, validateEmail)
import TeamTavern.Server.Player.Domain.Nickname (Nickname, validateNickname)
import TeamTavern.Server.Player.Domain.Password (Password, validatePassword)
import Type.Proxy (Proxy(..))

type Registration = Variant
    ( password ::
        { email :: Email
        , nickname :: Nickname
        , password :: Password
        }
    , discord ::
        { nickname :: Nickname
        , accessToken :: String
        }
    )

validateRegistration'
    :: RegisterPlayer.RequestContent
    -> ValidatedTerrorNea RegisterPlayer.RegistrationError Registration
validateRegistration' = match
    { password: \{email, nickname, password} ->
        { email: _, nickname: _, password: _ }
        <$> validateEmail email
        <*> validateNickname nickname
        <*> validatePassword password
        <#> inj (Proxy :: _ "password")
    , discord: \{nickname, accessToken} ->
        {nickname: _, accessToken}
        <$> validateNickname nickname
        <#> inj (Proxy :: _ "discord")
    }

validateRegistration
    :: ∀ errors
    .  RegisterPlayer.RequestContent
    -> Async (BadRequestTerror RegisterPlayer.BadContent errors) Registration
validateRegistration identifiers =
    validateRegistration' identifiers
    # AsyncVal.fromValidated
    # lmap
        (Terror.label ((Proxy :: _ "registration"))
        >>> map badRequest_)
