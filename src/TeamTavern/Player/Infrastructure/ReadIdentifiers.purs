module TeamTavern.Player.Infrastructure.ReadIdentifiers where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Foreign (ForeignError)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Domain.Email (EmailError)
import TeamTavern.Player.Domain.Email as Email
import TeamTavern.Player.Domain.Nickname (NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Types (Identifiers)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)

type IdentifiersError = Variant
    ( email :: NonEmptyList EmailError
    , nickname :: NonEmptyList NicknameError
    )

type ReadIdentifiersError errors = Variant
    ( unreadableIdentifiers ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidIdentifiers ::
        { identifiers :: IdentifiersModel
        , errors :: NonEmptyList IdentifiersError
        }
    | errors )

readIdentifiers :: forall errors.
    Body -> Async (ReadIdentifiersError errors) Identifiers
readIdentifiers body = do
    content <- readBody body
    identifiers @ { email, nickname } :: IdentifiersModel <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableIdentifiers")
            { content, errors: _ }
    { email: _, nickname: _ }
        <$> (Email.create email
            # Validated.label (SProxy :: SProxy "email"))
        <*> (Nickname.create nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidIdentifiers")
            { identifiers, errors: _ }
