module TeamTavern.Server.Profile.Infrastructure.ReadSummary where

import Prelude

import Async (Async)
import Async.Validated (fromValidated)
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Domain.Summary as Summary

type ReadSummaryError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidSummary ::
        { summary :: String
        , errors :: NonEmptyList NonEmptyTextError
        }
    | errors )

readSummary :: forall errors. Body -> Async (ReadSummaryError errors) Summary
readSummary body = do
    content <- readBody body
    { summary } :: { summary :: String } <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto")
            { content, errors: _ }
    Summary.create summary
        # fromValidated
        # labelMap (SProxy :: SProxy "invalidSummary")
            { summary, errors: _ }
