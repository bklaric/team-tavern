module TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import TeamTavern.Routes.Shared.Field (ValuesSimple)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields

-- Profile types.

data ProfileType = Player | Team

-- Field types.

type OptionId = Int

type OptionKey = String

data Option = Option OptionId OptionKey

derive instance Eq Option

derive instance Ord Option

derive instance Generic Option _

instance Show Option where show = genericShow

type FieldId = Int

type FieldKey = String

data FieldType
    = SingleField (Map OptionKey Option)
    | MultiField (Map OptionKey Option)

derive instance Generic FieldType _

instance Show FieldType where show = genericShow

data Field = Field FieldId FieldKey FieldType

derive instance Generic Field _

instance Show Field where show = genericShow

-- Field value types.

type FieldValueId = Int

data FieldValueType
    = Single OptionId
    | Multi (Array OptionId)

data FieldValue = FieldValue FieldId FieldValueType

-- Prepare fields.

prepareOptions :: Array LoadFields.Option -> Map OptionKey Option
prepareOptions options = options
    <#> (\{ id, key } -> Tuple key (Option id key))
    # Map.fromFoldable

prepareFields :: Array LoadFields.Field -> Array Field
prepareFields fields =
    fields # Array.mapMaybe \field ->
        case field.ilk, field.options of
        2, options -> Just $
            Field field.id field.key $ SingleField (prepareOptions options)
        3, options -> Just $
            Field field.id field.key $ MultiField (prepareOptions options)
        _, _ -> Nothing

-- Validate field values.

validateField
    :: ValuesSimple
    -> Field
    -> Maybe FieldValue
validateField fieldValues (Field id key (SingleField options)) =
    case fieldValues # Array.find \{ fieldKey } -> fieldKey == key of
    Just fieldValue | Just optionKey <- fieldValue.optionKey ->
        case Map.lookup optionKey options of
        Just (Option optionId _) -> Just $ FieldValue id $ Single optionId
        Nothing -> Nothing
    _ -> Nothing
validateField fieldValues (Field id key (MultiField options)) =
    case fieldValues # Array.find \{ fieldKey } -> fieldKey == key of
    Just fieldValue | Just optionKeys <- fieldValue.optionKeys -> let
        valueOptionIds =
            optionKeys
            # Array.mapMaybe (flip Map.lookup options)
            <#> \(Option optionId _) -> optionId
        in
        if not $ Array.null valueOptionIds
        then Just $ FieldValue id $ Multi valueOptionIds
        else Nothing
    _ -> Nothing

validateFieldValues
    :: Array LoadFields.Field
    -> ValuesSimple
    -> Array FieldValue
validateFieldValues fields fieldValues
    = prepareFields fields
    # Array.mapMaybe (validateField fieldValues)
