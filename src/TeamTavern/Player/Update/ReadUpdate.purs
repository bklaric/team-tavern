module TeamTavern.Player.Update.ReadUpdate where

import Prelude

import Async (Async)
import Async (fromEither) as Async
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Foreign (ForeignError)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Domain.About (AboutError)
import TeamTavern.Player.Domain.About as About
import TeamTavern.Player.Domain.Nickname (NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Types (NicknamedAbout)
import TeamTavern.Player.Infrastructure.Types (NicknamedAboutModel)

type NicknamedAboutError = Variant
    ( nickname :: NonEmptyList NicknameError
    , about :: NonEmptyList AboutError
    )

type ReadUpdateError errors = Variant
    ( unreadableUpdate ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidUpdate ::
        { nicknamedAbout :: NicknamedAboutModel
        , errors :: NonEmptyList NicknamedAboutError
        }
    | errors )

readUpdate :: forall errors.
    Body -> Async (ReadUpdateError errors) NicknamedAbout
readUpdate body = do
    content <- readBody body
    nicknamedAbout @ { nickname, about } :: NicknamedAboutModel <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableUpdate") { content, errors: _ }
        # Async.fromEither
    { nickname: _, about: _ }
        <$> (Nickname.create nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        <*> (About.create about
            # Validated.label (SProxy :: SProxy "about"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidUpdate")
            { nicknamedAbout, errors: _ }
