module TeamTavern.Profile.Infrastructure.ReadIdentifiers where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label as Label
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import TeamTavern.Game.Domain.Handle (HandleError)
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Player.Domain.Nickname (NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Profile.Domain.Types (Identifiers)
import TeamTavern.Profile.Infrastructure.Types (IdentifiersModel)

type ReadIdentifiersError errors = Variant
    ( invalidIdentifiers ::
        { identifiers :: IdentifiersModel
        , errors :: NonEmptyList (Variant
            ( invalidNickname :: NonEmptyList NicknameError
            , invalidHandle :: NonEmptyList HandleError
            ))
        }
    | errors )

readIdentifiers
    :: forall errors
    .  { nickname :: String, handle :: String }
    -> Async (ReadIdentifiersError errors) Identifiers
readIdentifiers identifiers @ { nickname, handle } =
    { nickname: _, handle: _ }
    <$> (Nickname.create nickname
        # Validated.label (SProxy :: SProxy "invalidNickname"))
    <*> (Handle.create handle
        # Validated.label (SProxy :: SProxy "invalidHandle"))
    # Async.fromValidated
    # Label.labelMap (SProxy :: SProxy "invalidIdentifiers")
        { identifiers, errors: _ }
