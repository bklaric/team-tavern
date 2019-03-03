module TeamTavern.Client.Infrastructure.Player where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Error (Error)
import Foreign (MultipleErrors)
import Simple.JSON.Async as JsonAsync
import TeamTavern.Player.View.SendResponse as View

data LoadPlayerError
    = FetchError Error
    | OkReadError MultipleErrors
    | NotFound
    | UnknownResponse

loadPlayer :: String -> Async LoadPlayerError View.OkContent
loadPlayer nickname = do
    response <- Fetch.fetch_ ("/api/players/" <> nickname) # lmap FetchError
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap OkReadError
        404 -> Async.left NotFound
        _ -> Async.left UnknownResponse
    pure content
