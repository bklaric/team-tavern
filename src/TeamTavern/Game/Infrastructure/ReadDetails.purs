module TeamTavern.Game.Infrastructure.ReadDetails where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import Foreign (ForeignError)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Game.Domain.Description (DescriptionError)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle (HandleError)
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Name (NameError)
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Game.Domain.Types (Details)
import TeamTavern.Game.Infrastructure.Types (DetailsModel)

type DetailsError = Variant
    ( name :: NonEmptyList NameError
    , handle :: NonEmptyList HandleError
    , description :: NonEmptyList DescriptionError
    )

type ReadDetailsError errors = Variant
    ( unreadableDetails ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidDetails ::
        { details :: DetailsModel
        , errors :: NonEmptyList DetailsError
        }
    | errors )

readDetails :: forall errors. Body -> Async (ReadDetailsError errors) Details
readDetails body = do
    content <- readBody body
    details @ { name, handle, description } :: DetailsModel <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableDetails") { content, errors: _ }
    { name: _, handle: _, description: _ }
        <$> (Name.create name # Validated.label (SProxy :: SProxy "name"))
        <*> (Handle.create handle # Validated.label (SProxy :: SProxy "handle"))
        <*> (Description.create description
            # Validated.label (SProxy :: SProxy "description"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidDetails")
            { details, errors: _ }
