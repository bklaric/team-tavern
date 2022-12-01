module Jarilo.Junction where

import Jarilo.Route (Route)

foreign import data Junction :: Type

foreign import data NamedRoute :: Symbol -> Route -> Junction

infixr 9 type NamedRoute as :=

foreign import data JunctionChain :: Junction -> Junction -> Junction

infixr 8 type JunctionChain as :<|>
