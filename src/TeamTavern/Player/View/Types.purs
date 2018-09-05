module TeamTavern.Player.View.Types where

import Data.List.Types (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)

type ViewError = Variant
    ( nicknameInvalid ::
        { nickname :: NonEmptyString
        , errors :: NonEmptyList NicknameError
        }
    , databaseError :: Error
    , unreadableResult :: NonEmptyList ForeignError
    , notFound :: Nickname
    , invalidView ::
        { nickname :: Nickname
        , view ::
            { about :: String
            , profiles :: Array
                { handle :: String
                , name :: String
                , summary :: String
                }
            }
        }
    )
