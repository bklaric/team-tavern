module TeamTavern.Server.Team.Infrastructure.ValidateName (Name, validateName, toString) where

import Prelude

import Data.String (trim)
import Data.Validated.Label (ValidatedVariants, Variants)
import Data.Validated.Label as Validated
import Type.Proxy (Proxy(..))
import Wrapped.String (Empty, NotPrintable, TooLong, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Name = Name String

maxLength :: Int
maxLength = 40

type NameErrors = Variants (empty :: Empty, notPrintable :: NotPrintable, tooLong :: TooLong)

validateName :: forall errors. String -> ValidatedVariants (name :: Array String | errors) Name
validateName name
    = Wrapped.create trim [empty, tooLong maxLength, notPrintable] Name name
    # Validated.labelMap (Proxy :: _ "name") \(errors :: NameErrors) ->
        [ "Error validating name: " <> show errors ]

toString :: Name -> String
toString (Name name) = name
