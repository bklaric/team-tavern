module Jarilo.Method where

foreign import data Method :: Type

foreign import data Options :: Method

foreign import data Head :: Method

foreign import data Get :: Method

foreign import data Post :: Type -> Method

foreign import data Put :: Type -> Method

foreign import data Patch :: Type -> Method

foreign import data Delete :: Method
