module TeamTavern.Profile.Infrastructure.ReadSummary where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import Foreign (ForeignError)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Profile.Domain.Summary (Summary, SummaryError)
import TeamTavern.Profile.Domain.Summary as Summary

type ReadSummaryError errors = Variant
    ( unreadableSummary ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidSummary ::
        { summary :: String
        , errors :: NonEmptyList SummaryError
        }
    | errors )

readSummary :: forall errors. Body -> Async (ReadSummaryError errors) Summary
readSummary body = do
    content <- readBody body
    { summary } :: { summary :: String } <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableSummary")
            { content, errors: _ }
    Summary.create' summary
        # labelMap (SProxy :: SProxy "invalidSummary")
            { summary, errors: _ }
        # Async.fromEither
