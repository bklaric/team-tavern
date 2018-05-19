module Main where

import Prelude

import Control.Monad.Cont (ContT(..), lift, runContT)
import Data.Either (Either(..))
import Data.Foreign.Generic.Types (SumEncoding(..))
import Data.Foreign.Generic.Types as Generic
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Options (Options, (:=))
import Data.String.NonEmpty (toString)
import Data.Validation.Semigroup (unV)
import Data.Variant (match)
import Effect (Effect)
import Node.Server (ListenOptions(..))
import Perun.Request (Request)
import Perun.Request.Body (Body, readAsUtf8)
import Perun.Response (Response)
import Perun.Server (run_)
import Perun.Url (Url, pathSegments, queryPairs)
import Postgres.Client.Config (ClientConfig, database, host, password, port, user)
import Postgres.Pool as Pool
import Routing.Junction (JunctionProxy(..), junctionRouter')
import Simple.JSON (readJSON)
import TeamTavern.Player.Register (register)
import TeamTavern.Player.Routes (TeamTavernRoutes)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

teamTavernRoutes = (JunctionProxy :: JunctionProxy TeamTavernRoutes)

genericOptions :: Generic.Options
genericOptions =
    { sumEncoding: TaggedObject
        { tagFieldName: "tag"
        , contentsFieldName: "data"
        , constructorTagTransform: id
        }
    , unwrapSingleConstructors: true
    , unwrapSingleArguments: true
    , fieldTransform: id
    }

clientConfig :: Options ClientConfig
clientConfig =
    user := "bklaric"
    <> password := "bklaric"
    <> host := "localhost"
    <> port := 5432
    <> database := "team_tavern"

registerPlayer :: Body -> (Response -> Effect Unit) -> ContT Unit Effect Unit
registerPlayer body respond = do
    bodyString <- flip readAsUtf8 body >>> void # ContT
    case readJSON bodyString of
        Left errors -> lift $ respond { statusCode: 400, content: "Couldnt parse body '" <> bodyString <> "' because: " <> show errors }
        Right (playerToRegisterModel :: { email :: String, nickname :: String }) -> do
            pool <- lift $ Pool.create mempty clientConfig
            result <- register pool playerToRegisterModel
            lift $ (unV
                (\error -> respond { statusCode: 400, content: "Error inserting in the database."  })
                (\player -> respond { statusCode: 200, content: "Looks good: " <> unwrap player.email <> ", " <> unwrap player.nickname })
                result)

requestHandler :: Either CustomMethod Method -> Url -> Body -> (Response -> Effect Unit) -> Effect Unit
requestHandler method url body respond =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> respond { statusCode: 404, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ respond { statusCode: 200, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> respond { statusCode: 200, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ runContT (registerPlayer body respond) pure
        }

invalidUrlHandler :: Request -> (Response -> Effect Unit) -> Effect Unit
invalidUrlHandler { method, url, body } respond =
    case url of
    Right url' -> requestHandler method url' body respond
    Left url' -> respond { statusCode: 400, content: "Couldn't parse url '" <> url' <> "'." }

main :: Effect Unit
main = run_ listenOptions invalidUrlHandler
