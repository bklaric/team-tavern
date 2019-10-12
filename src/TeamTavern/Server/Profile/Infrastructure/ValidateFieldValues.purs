module TeamTvaern.Server.Profile.Infrastructure.ValidateFieldValues where

import Prelude

import Data.Array (foldl)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (List(..), NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (find, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple4, (/\))
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Variant (SProxy(..), Variant)
import Data.Variant as Variant
import TeamTavern.Server.Profile.Infrastructure.LoadFields as LoadFields
import TeamTavern.Server.Profile.Infrastructure.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl as ValidateUrl

-- Field types.

type OptionId = Int

type OptionKey = String

type OptionText = String

data Option = Option OptionId OptionKey OptionText

derive instance eqOption :: Eq Option

derive instance ordOption :: Ord Option

derive instance genericOption :: Generic Option _

instance showOption :: Show Option where show = genericShow

type FieldId = Int

type FieldKey = String

data FieldType
    = UrlField
    | SingleField (Map OptionKey Option)
    | MultiField (Map OptionKey Option)

derive instance genericFieldType :: Generic FieldType _

instance showFieldType :: Show FieldType where show = genericShow

data Field = Field FieldId FieldKey FieldType

derive instance genericField :: Generic Field _

instance showField :: Show Field where show = genericShow

-- Field value types.

type FieldValueId = Int

data FieldValueType
    = Url ValidateUrl.Url
    | Single OptionId
    | Multi (Array OptionId)

data FieldValue = FieldValue FieldId FieldValueType

-- Error type.

type ValidateFieldValuesError = Variant
    ( unrelatedFieldValue ::
        { fields :: Array Field
        , fieldValue :: ReadProfile.FieldValue
        }
    , invalidFieldValue ::
        { field :: Field
        , fieldValue :: ReadProfile.FieldValue
        }
    , invalidUrlFieldValue ::
        { errors :: NonEmptyList ValidateUrl.UrlError
        , field :: Field
        , fieldValue :: ReadProfile.FieldValue
        }
    , invalidSingleFieldValue ::
        { field :: Field
        , fieldValue :: ReadProfile.FieldValue
        }
    , invalidMultiFieldValue ::
        { field :: Field
        , fieldValue :: ReadProfile.FieldValue
        })

-- Prepare fields.

prepareOptions :: Array LoadFields.Option -> Map OptionKey Option
prepareOptions options = options
    <#> (\{ id, key, option } -> Tuple key (Option id key option))
    # Map.fromFoldable

prepareFields :: Array LoadFields.Field -> Array Field
prepareFields fields =
    fields # Array.mapMaybe \field ->
        case Tuple field.type field.options of
        Tuple 1 Nothing -> Just $
            Field field.id field.key UrlField
        Tuple 2 (Just options) -> Just $
            Field field.id field.key $ SingleField (prepareOptions options)
        Tuple 3 (Just options) -> Just $
            Field field.id field.key $ MultiField (prepareOptions options)
        _ -> Nothing

-- Validate field values.

validateFieldValue
    :: Array Field
    -> ReadProfile.FieldValue
    -> Either ValidateFieldValuesError FieldValue
validateFieldValue fields fieldValue @ { fieldKey, url, optionKey, optionKeys } =
    case find (\(Field _ key _) -> key == fieldKey) fields of
    Just field @ (Field fieldId _ type') ->
        case tuple4 type' url optionKey optionKeys of
        UrlField /\ Just url' /\ Nothing /\ Nothing /\ unit ->
            case ValidateUrl.create url' of
            Right url'' -> Right $
                FieldValue fieldId $ Url $ url''
            Left errors -> Left $ Variant.inj (SProxy :: SProxy "invalidUrlFieldValue") { field, fieldValue, errors }
        SingleField fieldOptions /\ Nothing /\ Just optionKey' /\ Nothing /\ unit ->
            case Map.lookup optionKey' fieldOptions of
            Just (Option optionId _ _) -> Right $ FieldValue fieldId $ Single optionId
            Nothing -> Left $ Variant.inj (SProxy :: SProxy "invalidSingleFieldValue") { field, fieldValue }
        MultiField fieldOptions /\ Nothing /\ Nothing /\ Just optionKeys' /\ unit -> let
            valueOptionIds = optionKeys'
                # traverse (flip Map.lookup fieldOptions)
                <#> map (\(Option id _ _) -> id)
                <#> Set.fromFoldable
                <#> Array.fromFoldable
            in
            case valueOptionIds of
            Just valueOptionIds' | not $ Array.null valueOptionIds' ->
                Right $ FieldValue fieldId $ Multi valueOptionIds'
            _ -> Left $ Variant.inj (SProxy :: SProxy "invalidMultiFieldValue") { field, fieldValue }
        _ -> Left $ Variant.inj (SProxy :: SProxy "invalidFieldValue") { field, fieldValue }
    Nothing -> Left $ Variant.inj (SProxy :: SProxy "unrelatedFieldValue") { fields, fieldValue }

validateFieldValues
    :: Array LoadFields.Field
    -> Array ReadProfile.FieldValue
    -> Validated (NonEmptyList ValidateFieldValuesError) (List FieldValue)
validateFieldValues fields fieldValues = let
    preparedFields = prepareFields fields
    in
    foldl (\errorsAndValues fieldValue ->
        case validateFieldValue preparedFields fieldValue of
        Right validatedFieldValue ->
            rmap (Cons validatedFieldValue) errorsAndValues
        Left fieldValueError ->
            case errorsAndValues of
            Right _ -> Left
                $ NonEmptyList.singleton fieldValueError
            Left fieldValueErrors -> Left
                $ NonEmptyList.cons fieldValueError fieldValueErrors
        )
        (Right Nil)
        fieldValues
    # Validated.fromEither
