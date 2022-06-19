module TeamTavern.Routes.Profile.Shared where

import Prelude

import Data.Array (foldl, mapMaybe)
import Data.MultiMap as MultiMap
import Data.Tuple (Tuple(..))
import TeamTavern.Routes.Shared.Filters (Field)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value, keyToString, valueToString)

pageSize :: Int
pageSize = 10

type ProfilePage = Int

bundleFields :: QueryPairs Key Value -> Array Field
bundleFields (QueryPairs filters) = let
    preparedField fieldKey optionKey = let
        preparedFieldKey = keyToString fieldKey
        preparedOptionKey = valueToString optionKey
        in
        Tuple preparedFieldKey preparedOptionKey
    preparedFields =
        filters # mapMaybe \(Tuple fieldKey optionKey') ->
            optionKey' <#> \optionKey -> preparedField fieldKey optionKey
    groupedFields = preparedFields
        # foldl (\groupedFiltersSoFar (Tuple fieldKey optionKey) ->
            MultiMap.insertOrAppend' fieldKey optionKey groupedFiltersSoFar)
            MultiMap.empty
    in
    groupedFields # MultiMap.toUnfoldable' <#> \(Tuple fieldKey optionKeys) ->
        { fieldKey, optionKeys }
