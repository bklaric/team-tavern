module Jarilo.Response where

foreign import data Response :: Type

foreign import data Ok :: Type -> Response

foreign import data Ok_ :: Response

foreign import data NoContent :: Response

foreign import data BadRequest :: Type -> Response

foreign import data BadRequest_ :: Response

foreign import data Forbidden :: Type -> Response

foreign import data Forbidden_ :: Response

foreign import data Internal :: Type -> Response

foreign import data Internal_ :: Response

foreign import data ResponseChain :: Response -> Response -> Response

infixr 9 type ResponseChain as :!
