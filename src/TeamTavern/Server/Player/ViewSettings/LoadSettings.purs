module TeamTavern.Server.Player.ViewSettings.LoadSettings
    (LoadSettingsResult, LoadSettingsError, loadSettings) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (rows)
import Simple.JSON.Async (read)

type LoadSettingsResult = { notify :: Boolean }

type LoadSettingsError errors = Variant
    ( notFound :: String
    , unreadableSettings ::
        { settings :: Foreign
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    | errors )

queryString :: Query
queryString = Query """
    select player.notify
    from player
    where lower(player.nickname) = lower($1)
    """

loadSettings
    :: forall errors
    .  Pool
    -> String
    -> Async (LoadSettingsError errors) LoadSettingsResult
loadSettings pool nickname = do
    result <-
        pool
        # query queryString (nickname : [])
        # label (SProxy :: SProxy "databaseError")
    settings <-
        rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "notFound") nickname)
    settings' :: LoadSettingsResult <-
        read settings
        # labelMap (SProxy :: SProxy "unreadableSettings")
            { settings, errors: _ }
    pure settings'
