module TeamTavern.Server.Profile.Infrastructure.ReadProfile where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Maybe (Maybe)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

type FieldValue =
    { fieldKey :: String
    , url :: Maybe String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type Profile =
    { summary :: String
    , fieldValues :: Array FieldValue
    }

type ReadProfileError errors = Variant
    ( unreadableProfile ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readProfile :: forall errors. Body -> Async (ReadProfileError errors) Profile
readProfile body = do
    content <- readBody body
    profile <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableProfile") { content, errors: _ }
    pure profile
