module TeamTavern.Server.Infrastructure.Error where

import Data.Variant (Variant)

type InternalRow errors = (internal :: Array String | errors)

type NotFoundRow errors = (notFound :: Array String | errors)

type NotAuthorizedRow errors = (notAuthorized :: Array String | errors)

type ClientRow errors = (client :: Array String | errors)

type InternalError errors = Variant (internal :: Array String | errors)

type LoadSingleError errors = Variant
    ( internal :: Array String
    , notFound :: Array String
    | errors
    )

type ChangeSingleError errors = Variant
    ( internal :: Array String
    , notAuthorized :: Array String
    | errors
    )

type ClientError errors = Variant (client :: Array String | errors)
