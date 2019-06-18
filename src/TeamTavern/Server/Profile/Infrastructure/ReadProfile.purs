module TeamTavern.Server.Profile.Infrastructure.ReadProfile where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label (labelMap)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Profile.Domain.FieldValue (Field, FieldValue, FieldValueError)
import TeamTavern.Server.Profile.Domain.FieldValue as FieldValue
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Domain.Summary as Summary

type ProfileDto =
    { summary :: String
    , fieldValues :: Array
        { fieldId :: Int
        , url :: Maybe String
        , optionId :: Maybe Int
        , optionIds :: Maybe (Array Int)
        }
    }

type ProfileModel =
    { summary :: Summary
    , fieldValues :: List FieldValue
    }

type ProfileModelError = Variant
    ( summary :: NonEmptyList NonEmptyTextError
    , fieldValues :: NonEmptyList FieldValueError
    )

type ReadProfileError errors = Variant
    ( unreadableProfileDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidProfileModel ::
        { dto :: ProfileDto
        , errors :: NonEmptyList ProfileModelError
        }
    | errors )

readProfile :: forall errors.
    Array Field -> Body -> Async (ReadProfileError errors) ProfileModel
readProfile fields body = do
    content <- readBody body
    dto @ { summary, fieldValues } :: ProfileDto <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableProfileDto")
            { content, errors: _ }
    { summary: _, fieldValues: _ }
        <$> (Summary.create summary
            # Validated.label (SProxy :: SProxy "summary"))
        <*> (FieldValue.createFieldValues fields fieldValues
            # Validated.label (SProxy :: SProxy "fieldValues"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidProfileModel") { dto, errors: _ }
