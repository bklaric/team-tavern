module TeamTavern.Server.Player.EditSettings.ReadSettings where

import Prelude

import Async (Async)
import Async (fromEither) as Async
import Data.Bifunctor.Label (labelMap)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

type EditSettingsModel = { notify :: Boolean }

type ReadNicknameError errors = Variant
    ( unreadableModel ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readSettings :: forall errors.
    Body -> Async (ReadNicknameError errors) EditSettingsModel
readSettings body = do
    content <- readBody body
    model @ { notify } :: EditSettingsModel <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableModel") { content, errors: _ }
        # Async.fromEither
    pure model
