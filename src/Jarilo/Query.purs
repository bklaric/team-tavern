module Jarilo.Query where

foreign import data Query :: Type

foreign import data NoQuery :: Query

foreign import data Optional :: Symbol -> Type -> Query

foreign import data Mandatory :: Symbol -> Type -> Query

foreign import data Many :: Symbol -> Type -> Query

foreign import data Rest :: Symbol -> Query

foreign import data QueryChain :: Query -> Query -> Query

infixr 9 type QueryChain as :?
