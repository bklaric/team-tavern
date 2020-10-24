module TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues where

import Prelude

import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (validateUrl)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl as ValidateUrl

-- Profile types.

data ProfileType = Player | Team

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

type FieldDomain = String

type FieldRequired = Boolean

data FieldType
    = UrlField FieldDomain
    | SingleField (Map OptionKey Option)
    | MultiField (Map OptionKey Option)

derive instance genericFieldType :: Generic FieldType _

instance showFieldType :: Show FieldType where show = genericShow

data Field = Field FieldId FieldKey FieldRequired FieldType

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

type ValidateFieldValuesError errors = Variant
    ( url ::
        { message :: Array String
        , key :: String
        }
    , missing ::
        { message :: Array String
        , key :: String
        }
    | errors
    )

-- Prepare fields.

prepareOptions :: Array LoadFields.Option -> Map OptionKey Option
prepareOptions options = options
    <#> (\{ id, key, option } -> Tuple key (Option id key option))
    # Map.fromFoldable

prepareFields :: Array LoadFields.Field -> Array Field
prepareFields fields =
    fields # Array.mapMaybe \field ->
        case field.ilk, field.domain, field.options of
        1, Just domain, Nothing -> Just $
            Field field.id field.key field.required $ UrlField domain
        2, _, (Just options) -> Just $
            Field field.id field.key field.required $ SingleField (prepareOptions options)
        3, _, (Just options) -> Just $
            Field field.id field.key field.required $ MultiField (prepareOptions options)
        _, _, _ -> Nothing

-- Validate field values.

validateField :: forall errors.
    Array ReadProfile.FieldValue -> Field -> Maybe (Either (ValidateFieldValuesError errors) FieldValue)
validateField fieldValues (Field id key required (UrlField domain)) =
    case fieldValues # Array.find \{ fieldKey } -> fieldKey == key of
    Just fieldValue | Just url <- fieldValue.url ->
        case validateUrl domain url of
        Right url' -> Just $ Right $ FieldValue id $ Url url'
        Left errors -> Just $ Left $ inj (SProxy :: SProxy "url")
            { message: [ "Field value for field " <> key <> " is invalid: " <> show errors ], key }
    _ | required -> Just $ Left $ inj (SProxy :: SProxy "missing")
        { message: [ "Field " <> key <> " is required." ], key }
    _ -> Nothing
validateField fieldValues (Field id key _ (SingleField options)) =
    case fieldValues # Array.find \{ fieldKey } -> fieldKey == key of
    Just fieldValue | Just optionKey <- fieldValue.optionKey ->
        case Map.lookup optionKey options of
        Just (Option optionId _ _) -> Just $ Right $ FieldValue id $ Single optionId
        Nothing -> Nothing
    _ -> Nothing
validateField fieldValues (Field id key _ (MultiField options)) =
    case fieldValues # Array.find \{ fieldKey } -> fieldKey == key of
    Just fieldValue | Just optionKeys <- fieldValue.optionKeys -> let
        valueOptionIds =
            optionKeys
            # Array.mapMaybe (flip Map.lookup options)
            <#> \(Option optionId _ _) -> optionId
        in
        if not $ Array.null valueOptionIds
        then Just $ Right $ FieldValue id $ Multi valueOptionIds
        else Nothing
    _ -> Nothing

validateFieldValues :: forall errors
    .  Array LoadFields.Field
    -> Array ReadProfile.FieldValue
    -> Validated (NonEmptyList (ValidateFieldValuesError errors)) (Array FieldValue)
validateFieldValues fields fieldValues
    = prepareFields fields
    # Array.mapMaybe (validateField fieldValues)
    # foldl
        ( case _, _ of
            Right values, Right value -> Right $ Array.snoc values value
            Right _, Left error -> Left $ NonEmptyList.singleton error
            Left errors, Right _ -> Left errors
            Left errors, Left error -> Left $ NonEmptyList.snoc errors error
        )
        (Right [])
    # Validated.fromEither
