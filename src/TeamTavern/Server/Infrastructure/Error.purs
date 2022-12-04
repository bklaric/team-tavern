module TeamTavern.Server.Infrastructure.Error where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.List.NonEmpty as Nel
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Validated (Validated)
import Data.Variant (Variant, inj)
import Jarilo.Router.Response (AppResponse)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Error errors = Error errors (Array String)

type ErrorVar errors = Error (Variant errors)

type ErrorVarNel errors = Error (NonEmptyList (Variant errors))

type ValidatedErrorVarNel errors = Validated (ErrorVarNel errors)

instance (Semigroup errors) => Semigroup (Error errors) where
    append (Error errors lines) (Error errors' lines') =
        Error (errors <> errors') (lines <> lines')













data TavernError errors = TavernError (Variant errors) (Array String)

data TavernErrorMany errors = TavernErrorMany (NonEmptyList (Variant errors)) (Array String)

instance Semigroup (TavernErrorMany errors) where
    append (TavernErrorMany errors lines) (TavernErrorMany errors' lines') =
        TavernErrorMany (errors <> errors') (lines <> lines')

toMany (TavernError error lines) = TavernErrorMany (Nel.singleton error) lines

singleton :: forall errors. Variant errors -> String -> TavernErrorMany errors
singleton error line = TavernErrorMany (Nel.singleton error) [line]

concat :: forall errors. NonEmptyList (TavernErrorMany errors) -> TavernErrorMany errors
concat (NonEmptyList (error :| errors)) = foldl (<>) error errors

collect :: forall errors. NonEmptyList (TavernError errors) -> TavernErrorMany errors
collect = map toMany >>> concat

mapErrorMany mapper (TavernErrorMany errors lines) = TavernErrorMany (mapper errors) lines

mapErrorMany' mapper (TavernErrorMany errors lines) = TavernError (mapper errors) lines

type ValidatedNel left right = Validated (NonEmptyList left) right

type ValidatedNelTavern errors right = ValidatedNel (TavernError errors) right

type ValidatedTavern errors right = Validated (TavernErrorMany errors) right

errorResponse :: forall errors. TavernError errors -> Variant errors
errorResponse (TavernError response _) = response

mergeAll label' (NonEmptyList ((TavernError firstError firstLines) :| errors)) =
    errors
    # foldr
        (\(TavernError nextError nextLines) (Tuple errorsSoFar linesSoFar) ->
            Tuple (nextError : errorsSoFar) (nextLines <> linesSoFar)
        )
        (Tuple [firstError] firstLines)
    # \(Tuple errors lines) -> TavernError (inj label' errors) lines

mapError mapper (TavernError response lines) = TavernError (mapper response) lines

type BadRequestError body errors = TavernError (badRequest :: AppResponse body | errors)

type BadRequestError_ errors = TavernError (badRequest :: AppResponse Unit | errors)

type ForbiddenError body errors = TavernError (forbidden :: AppResponse body | errors)

type ForbiddenError_ errors = TavernError (forbidden :: AppResponse Unit | errors)

type InternalError body errors = TavernError (internal :: AppResponse body | errors)

type InternalError_ errors = TavernError (internal :: AppResponse Unit | errors)


type InternalBadRequestError errors = TavernError (InternalRow_ + BadRequestRow_ + errors)

type InternalNotAuthorizedError errors = TavernError (InternalRow_ + NotAuthorizedRow_ + errors)

type InternalNotAuthorizedForbiddenError errors = TavernError (InternalRow_ + NotAuthorizedRow_ + ForbiddenRow_ + errors)



type BadRequestRow body errors = (badRequest :: AppResponse body | errors)

type NotFoundRow body errors = (notFound :: AppResponse body | errors)

type NotAuthorizedRow body errors = (notAuthorized :: AppResponse body | errors)

type ForbiddenRow body errors = (forbidden :: AppResponse body | errors)

type InternalRow body errors = (internal :: AppResponse body | errors)



type BadRequestRow_ errors = (badRequest :: AppResponse Unit | errors)

type NotFoundRow_ errors = (notFound :: AppResponse Unit | errors)

type NotAuthorizedRow_ errors = (notAuthorized :: AppResponse Unit | errors)

type ForbiddenRow_ errors = (forbidden :: AppResponse Unit | errors)

type InternalRow_ errors = (internal :: AppResponse Unit | errors)












type ClientRow errors = (client :: AppResponse Unit | errors)

type ClientError errors = Variant (client :: Array String | errors)

type CommonError = Variant
    ( client :: Array String
    , internal :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , notFound :: Array String
    )
