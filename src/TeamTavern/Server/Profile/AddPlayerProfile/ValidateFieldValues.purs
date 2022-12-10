module TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues where

import Prelude

import Data.Array (foldl)
import Data.Array as Array
import Data.Array.NonEmpty as Nea
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNea)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (validateUrl)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl as ValidateUrl
import Type.Proxy (Proxy(..))

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

type FieldDomain = String

data FieldType
    = UrlField FieldDomain
    | SingleField (Map OptionKey Option)
    | MultiField (Map OptionKey Option)

derive instance Generic FieldType _

instance Show FieldType where show = genericShow

data Field = Field FieldId FieldKey FieldType

derive instance Generic Field _

instance Show Field where show = genericShow

-- Field value types.

type FieldValueId = Int

data FieldValueType
    = Url ValidateUrl.Url
    | Single OptionId
    | Multi (Array OptionId)

data FieldValue = FieldValue FieldId FieldValueType

-- Error type.

type ValidateFieldValuesError errors = Variant
    ( url :: { key :: String }
    | errors
    )

-- Prepare fields.

prepareOptions :: Array LoadFields.Option -> Map OptionKey Option
prepareOptions options = options
    <#> (\{ id, key } -> Tuple key (Option id key))
    # Map.fromFoldable

prepareFields :: Array LoadFields.Field -> Array Field
prepareFields fields =
    fields # Array.mapMaybe \field ->
        case field.ilk, field.domain, field.options of
        1, Just domain, Nothing -> Just $
            Field field.id field.key $ UrlField domain
        2, _, (Just options) -> Just $
            Field field.id field.key $ SingleField (prepareOptions options)
        3, _, (Just options) -> Just $
            Field field.id field.key $ MultiField (prepareOptions options)
        _, _, _ -> Nothing

-- Validate field values.

validateField
    :: forall errors
    .  Array AddPlayerProfile.RequestContentFieldValue
    -> Field
    -> Maybe (Either (Terror (ValidateFieldValuesError errors)) FieldValue)
validateField fieldValues (Field id key (UrlField domain)) =
    case fieldValues # Array.find \{ fieldKey } -> fieldKey == key of
    Just fieldValue | Just url <- fieldValue.url ->
        case validateUrl domain url of
        Right url' -> Just $ Right $ FieldValue id $ Url url'
        Left errors -> Just $ Left $ Terror (inj (Proxy :: _ "url") { key })
            [ "Field value for field " <> key <> " is invalid: " <> show errors ]
    _ -> Nothing
validateField fieldValues (Field id key (SingleField options)) =
    case fieldValues # Array.find \{ fieldKey } -> fieldKey == key of
    Just fieldValue | Just optionKey <- fieldValue.optionKey ->
        case Map.lookup optionKey options of
        Just (Option optionId _) -> Just $ Right $ FieldValue id $ Single optionId
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
        then Just $ Right $ FieldValue id $ Multi valueOptionIds
        else Nothing
    _ -> Nothing

validateFieldValues :: forall errors
    .  Array LoadFields.Field
    -> Array AddPlayerProfile.RequestContentFieldValue
    -> ValidatedTerrorNea (ValidateFieldValuesError errors) (Array FieldValue)
validateFieldValues fields fieldValues
    = prepareFields fields
    # Array.mapMaybe (validateField fieldValues)
    # foldl
        ( case _, _ of
            Right values, Right value -> Right $ Array.snoc values value
            Right _, Left error -> Left $ Nea.singleton error
            Left errors, Right _ -> Left errors
            Left errors, Left error -> Left $ Nea.snoc errors error
        )
        (Right [])
    # Validated.fromEither
    # Validated.lmap Terror.collect
