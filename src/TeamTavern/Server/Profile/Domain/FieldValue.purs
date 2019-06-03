module TeamTavern.Server.Profile.Domain.FieldValue
    ( FieldDto
    , FieldType(..)
    , Field(..)
    , createField
    , FieldValueDto
    , FieldValueType(..)
    , FieldValue(..)
    , FieldValueError
    , createFieldValues
    ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (length)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (List(..), NonEmptyList(..), (:))
import Data.Maybe (Maybe(..))
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Data.NonEmpty ((:|))
import Data.Set.NonEmpty (NonEmptySet, subset)
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple4, (/\))
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Profile.Domain.Url (Url, UrlError)
import TeamTavern.Server.Profile.Domain.Url as Url
import Undefined (undefined)

type FieldDto =
    { id :: Int
    , type :: Int
    , options :: Maybe (Array { id :: Int })
    }

data FieldType
    = Url
    | Single (NonEmptySet Id)
    | Multi (NonEmptySet Id)

data Field = Field Id FieldType

derive instance genericFieldType :: Generic FieldType _

instance showFieldType :: Show FieldType where show = genericShow

derive instance genericField :: Generic Field _

instance showField :: Show Field where show = genericShow

createField :: FieldDto -> Maybe Field
createField fieldDto = let
    id = Id fieldDto.id
    in
    case Tuple fieldDto.type fieldDto.options of
    Tuple 1 Nothing -> Just $ Field id Url
    Tuple 2 (Just options) | optionIds <- options <#> _.id >>> Id ->
        case NonEmptySet.fromFoldable optionIds of
        Just nonEmptyOptions -> Just $ Field id $ Single nonEmptyOptions
        Nothing -> Nothing
    Tuple 3 (Just options) | optionIds <- options <#> _.id >>> Id ->
        case NonEmptySet.fromFoldable optionIds of
        Just nonEmptyOptions -> Just $ Field id $ Multi nonEmptyOptions
        Nothing -> Nothing
    _ -> Nothing

type FieldValueDto =
    { fieldId :: Int
    , url :: Maybe String
    , optionId :: Maybe Int
    , optionIds :: Maybe (Array Int)
    }

data FieldValueType
    = UrlValue Url
    | SingleValue Id
    | MultiValue (NonEmptySet Id)

data FieldValue = FieldValue Id FieldValueType

derive instance genericFieldValueType :: Generic FieldValueType _

instance showFieldValueType :: Show FieldValueType where show = genericShow

derive instance genericFieldValue :: Generic FieldValue _

instance showFieldValue :: Show FieldValue where show = genericShow

type FieldValueError = Variant
    ( unrelatedValue :: FieldValueDto
    , invalidUrl ::
        { fieldValueDto :: FieldValueDto
        , errors :: NonEmptyList UrlError
        }
    , invalidOption ::
        { fieldValueDto :: FieldValueDto
        , field :: Field
        }
    , invalidValue ::
        { fieldValueDto :: FieldValueDto
        , field :: Field
        }
    , duplicateValues :: NonEmptyList FieldValue
    )

data FieldValueCheckResult
    = Error FieldValueError
    | Valid FieldValue
    | Unrelated FieldValueDto

type CreateFieldValuesState =
    { errors :: List FieldValueError
    , values :: List FieldValue
    }

checkFieldValue' :: FieldValueDto -> Field -> FieldValueCheckResult
checkFieldValue'
    fieldValueDto @ { fieldId, url, optionId, optionIds }
    field @ (Field id type') = let
    fieldId' = Id fieldId
    in
    if id == fieldId'
    then
        case tuple4 type' url optionId optionIds of
        Url /\ (Just url') /\ Nothing /\ Nothing /\ unit ->
            case Url.create url' of
            Left errors -> Error $ inj (SProxy :: SProxy "invalidUrl")
                { fieldValueDto, errors }
            Right url'' -> Valid $ FieldValue fieldId' $ UrlValue url''
        Single fieldOptionIds /\ Nothing /\ Just valueOptionId /\ Nothing /\ unit
            | valueOptionId' <- Id valueOptionId ->
            if NonEmptySet.member valueOptionId' fieldOptionIds
            then Valid $ FieldValue fieldId' $ SingleValue valueOptionId'
            else Error $ inj (SProxy :: SProxy "invalidOption") { fieldValueDto, field }
        Multi fieldOptionIds /\ Nothing /\ Nothing /\ Just valueOptionIds /\ unit
            | valueOptionIds' <- map Id valueOptionIds ->
                case NonEmptySet.fromFoldable valueOptionIds' of
                Just valueOptionIds''
                    | subset valueOptionIds'' fieldOptionIds
                    && NonEmptySet.size valueOptionIds'' == Array.length valueOptionIds ->
                    Valid $ FieldValue fieldId' $ MultiValue valueOptionIds''
                _ ->
                    Error $ inj (SProxy :: SProxy "invalidValue") { fieldValueDto, field }
        _ -> Error $ inj (SProxy :: SProxy "invalidValue") { fieldValueDto, field }
    else
        Unrelated fieldValueDto

checkFieldValue
    :: FieldValueCheckResult
    -> Field
    -> FieldValueCheckResult
checkFieldValue (Error error) field = Error error
checkFieldValue (Valid value) field = Valid value
checkFieldValue (Unrelated valueDto) field = checkFieldValue' valueDto field

handleFieldValueCheckResult
    :: CreateFieldValuesState
    -> FieldValueCheckResult
    -> CreateFieldValuesState
handleFieldValueCheckResult { errors, values } (Error error) =
    { errors: error : errors, values }
handleFieldValueCheckResult { errors, values } (Valid value) =
    { errors, values: value : values }
handleFieldValueCheckResult { errors, values } (Unrelated valueDto) =
    { errors: inj (SProxy :: SProxy "unrelatedValue") valueDto : errors
    , values
    }

createFieldValue
    :: Array Field
    -> CreateFieldValuesState
    -> FieldValueDto
    -> CreateFieldValuesState
createFieldValue fields state fieldValueDto =
    fields
        # foldl checkFieldValue (Unrelated fieldValueDto)
        # handleFieldValueCheckResult state

checkUniqueFieldValues
    :: MultiMap Id FieldValue
    -> Either (NonEmptyList FieldValueError) (List FieldValue)
checkUniqueFieldValues valuesMap =
    valuesMap # MultiMap.values
        # foldl (\{ uniqueValues, duplicateValues } values ->
            if length values == 1
            then
                { uniqueValues: NonEmptyList.head values : uniqueValues
                , duplicateValues
                }
            else
                { uniqueValues
                , duplicateValues: values : duplicateValues
                })
            { uniqueValues: Nil, duplicateValues: Nil }
        # case _ of
            { duplicateValues: values : otherDuplicateValues } ->
                Left $ inj (SProxy :: SProxy "duplicateValues")
                    <$> NonEmptyList (values :| otherDuplicateValues)
            { duplicateValues: Nil, uniqueValues } -> Right uniqueValues

handleCreateFieldValueResult
    :: CreateFieldValuesState
    -> Either (NonEmptyList FieldValueError) (List FieldValue)
handleCreateFieldValueResult { errors: error : otherErrors } =
    Left $ NonEmptyList $ error :| otherErrors
handleCreateFieldValueResult { errors: Nil, values } =
    values
        # foldl (\map value ->
            let FieldValue id _ = value in MultiMap.insert' id value map)
            MultiMap.empty
        # checkUniqueFieldValues

createFieldValues
    :: Array Field
    -> Array FieldValueDto
    -> Validated (NonEmptyList FieldValueError) (List FieldValue)
createFieldValues fields fieldValueDtos =
    fieldValueDtos
        # foldl (createFieldValue fields) { errors: Nil, values: Nil }
        # handleCreateFieldValueResult
        # Validated.fromEither
