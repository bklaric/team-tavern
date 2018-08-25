module TeamTaver.Player.View.Run.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Player.View.Run.Types (ViewError)

logError :: ViewError -> Effect Unit
logError viewError = do
    log "Error viewing player"
    viewError # match
        { readNickname: \{ errors, nickname } -> do
            log $ "\tFailed nickname validation for segment: "
                <> toString nickname
            log $ "\tValidation resulted in these errors: "
                <> show errors
        , loadPlayer: \{ nickname, error } -> do
            log $ "\tCouldn't load player " <> unwrap nickname
            error # match
                { notFound: const $ log "\tPlayer couldn't be found"
                , cantReadView: \result ->
                    log "\tCouldn't read player view from result"
                , other: \error' ->
                    log $ "\tLoading player resulted in this error: "
                    <> code error' <> ", " <> name error' <> ", "
                    <> message error'
                }
        }
