module Jarilo.Path where

foreign import data Path :: Type

foreign import data Literal :: Symbol -> Path

foreign import data Capture :: Symbol -> Type -> Path

foreign import data Sub :: Path -> Path -> Path

infixr 9 type Sub as :>
