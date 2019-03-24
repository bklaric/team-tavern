module TeamTavern.Player.Update.ReadUpdate where

import Prelude

import Async (Async)
import Async (fromEither) as Async
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Domain.Text (TextError)
import TeamTavern.Player.Domain.About (About)
import TeamTavern.Player.Domain.About as About
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname

type UpdateDto =
    { nickname :: String
    , about :: String
    }

type UpdateModel =
    { nickname :: Nickname
    , about :: About
    }

type UpdateModelError = Variant
    ( nickname :: NonEmptyList NicknameError
    , about :: NonEmptyList TextError
    )

type ReadUpdateError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: UpdateDto
        , errors :: NonEmptyList UpdateModelError
        }
    | errors )

readUpdate :: forall errors.
    Body -> Async (ReadUpdateError errors) UpdateModel
readUpdate body = do
    content <- readBody body
    dto @ { nickname, about } :: UpdateDto <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
        # Async.fromEither
    { nickname: _, about: _ }
        <$> (Nickname.create nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        <*> (About.create about
            # Validated.label (SProxy :: SProxy "about"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel")
            { dto, errors: _ }
