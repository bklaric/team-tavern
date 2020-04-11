module TeamTavern.Server.Profile.AddGameTeam.ValidateFieldValues where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import TeamTavern.Server.Profile.AddGameTeam.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddGameTeam.ReadProfile as ReadProfile

-- Field types.

type OptionId = Int

type OptionKey = String

type OptionLabel = String

data Option = Option OptionId OptionKey OptionLabel

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
    <#> (\{ id, key, label } -> Tuple key (Option id key label))
    # Map.fromFoldable

prepareFields :: Array LoadFields.Field -> Array Field
prepareFields fields =
    fields <#> \{ id, key, options } -> Field id key $ prepareOptions options

-- Validate field values.

validateFieldValue
    :: Array Field
    -> ReadProfile.FieldValue
    -> Maybe FieldValue
validateFieldValue fields { fieldKey, optionKeys } =
    fields
    # find (\(Field _ key _) -> key == fieldKey)
    >>= \(Field id _ options) -> let
        valueOptionIds =
            optionKeys
            # Array.mapMaybe (flip Map.lookup options)
            <#> \(Option optionId _ _) -> optionId
        in
        if not $ Array.null valueOptionIds
        then Just $ FieldValue id valueOptionIds
        else Nothing

validateFieldValues
    :: Array LoadFields.Field
    -> Array ReadProfile.FieldValue
    -> Array FieldValue
validateFieldValues fields fieldValues =
    fieldValues # Array.mapMaybe (validateFieldValue $ prepareFields fields)
