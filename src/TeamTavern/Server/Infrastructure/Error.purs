module TeamTavern.Server.Infrastructure.Error where

import Data.Variant (Variant)

type InternalRow errors = (internal :: Array String | errors)

type NotFoundRow errors = (notFound :: Array String | errors)

type InternalError errors = Variant (internal :: Array String | errors)

type LoadSingleError errors = Variant (InternalRow (NotFoundRow errors))
