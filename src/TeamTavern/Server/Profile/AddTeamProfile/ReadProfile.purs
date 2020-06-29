module TeamTavern.Server.Profile.AddTeamProfile.ReadProfile where

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
    , optionKeys :: Array String
    }

type Profile =
    { summary :: String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , languages :: Array String
    , countries :: Array String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , hasMicrophone :: Boolean
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
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
