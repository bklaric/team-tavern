module TeamTavern.Server.Conversation.Start.ReadMessage where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

type Message = { content :: String }

type ReadMessageError errors = Variant
    ( unreadableMessage ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readMessage :: forall errors. Body -> Async (ReadMessageError errors) Message
readMessage body = do
    content <- readBody body
    message <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableMessage") { content, errors: _ }
    pure message
