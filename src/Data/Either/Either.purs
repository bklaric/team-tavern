module Data.Either.Swap where

import Data.Either (Either(..))

swap :: forall left right. Either left right -> Either right left
swap =
    case _ of
    Left left -> Right left
    Right right -> Left right
