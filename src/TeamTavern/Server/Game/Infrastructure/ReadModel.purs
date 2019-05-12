module TeamTavern.Server.Game.Infrastructure.ReadModel where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Game.Domain.Description (Description)
import TeamTavern.Server.Game.Domain.Description as Description
import TeamTavern.Server.Game.Domain.Handle (Handle, HandleError)
import TeamTavern.Server.Game.Domain.Handle as Handle
import TeamTavern.Server.Game.Domain.Title (Title, TitleError)
import TeamTavern.Server.Game.Domain.Title as Name

type GameDto =
    { title :: String
    , handle :: String
    , description :: String
    }

type GameModel =
    { title :: Title
    , handle :: Handle
    , description :: Description
    }

type GameModelError = Variant
    ( title :: NonEmptyList TitleError
    , handle :: NonEmptyList HandleError
    , description :: NonEmptyList NonEmptyTextError
    )

type ReadModelError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: GameDto
        , errors :: NonEmptyList GameModelError
        }
    | errors )

readModel :: forall errors. Body -> Async (ReadModelError errors) GameModel
readModel body = do
    content <- readBody body
    dto @ { title, handle, description } :: GameDto <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
    { title: _, handle: _, description: _ }
        <$> (Name.create title # Validated.label (SProxy :: SProxy "title"))
        <*> (Handle.create handle # Validated.label (SProxy :: SProxy "handle"))
        <*> (Description.create description
            # Validated.label (SProxy :: SProxy "description"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel")
            { dto, errors: _ }
