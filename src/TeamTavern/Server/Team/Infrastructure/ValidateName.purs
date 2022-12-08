module TeamTavern.Server.Team.Infrastructure.ValidateName (Name, validateName, toString) where

import Prelude

import Data.Array.NonEmpty as Nea
import Data.String (trim)
import Data.Validated as Validated
import Data.Variant (inj)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar, NeaVar)
import Type.Proxy (Proxy(..))
import Wrapped.String (Empty, NotPrintable, TooLong, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Name = Name String

maxLength :: Int
maxLength = 40

type NameErrors = NeaVar
    ( empty :: Empty
    , notPrintable :: NotPrintable
    , tooLong :: TooLong
    )

validateName :: forall errors.
    String -> ValidatedTerrorNeaVar (name :: {} | errors) Name
validateName name
    = Wrapped.create trim [empty, tooLong maxLength, notPrintable] Name name
    # Validated.lmap \(errors :: NameErrors) -> Terror
        (Nea.singleton $ inj (Proxy :: _ "name") {})
        [ "Name is invalid: " <> name
        , "Failed with following errors: " <> show errors
        ]

toString :: Name -> String
toString (Name name) = name
