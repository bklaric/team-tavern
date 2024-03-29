module TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import TeamTavern.Routes.Shared.Field (ValuesSimpleMulti, ValueSimpleMulti)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as LoadFields

-- Field types.

type OptionId = Int

type OptionKey = String

data Option = Option OptionId OptionKey

type FieldId = Int

type FieldKey = String

type FieldOptions = Map OptionKey Option

data Field = Field FieldId FieldKey FieldOptions

-- Field value types.

type FieldValueOptions = Array OptionId

data FieldValue = FieldValue FieldId FieldValueOptions

-- Prepare fields.

prepareOptions :: Array LoadFields.Option -> Map OptionKey Option
prepareOptions options =
    options
    <#> (\{ id, key } -> Tuple key (Option id key))
    # Map.fromFoldable

prepareFields :: Array LoadFields.Field -> Array Field
prepareFields fields =
    fields <#> \{ id, key, options } -> Field id key $ prepareOptions options

-- Validate field values.

validateFieldValue
    :: Array Field
    -> ValueSimpleMulti
    -> Maybe FieldValue
validateFieldValue fields { fieldKey, optionKeys } =
    fields
    # find (\(Field _ key _) -> key == fieldKey)
    >>= \(Field id _ options) -> let
        valueOptionIds =
            optionKeys
            # Array.mapMaybe (flip Map.lookup options)
            <#> \(Option optionId _) -> optionId
        in
        if not $ Array.null valueOptionIds
        then Just $ FieldValue id valueOptionIds
        else Nothing

validateFieldValues
    :: Array LoadFields.Field
    -> ValuesSimpleMulti
    -> Array FieldValue
validateFieldValues fields fieldValues =
    fieldValues # Array.mapMaybe (validateFieldValue $ prepareFields fields)
