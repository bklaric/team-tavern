module Tasync where

import Prelude

import Async (Async(..))
import Async as Async
import Control.Monad.Reader (class MonadAsk, class MonadReader, Reader, ReaderT(..), ask, asks, lift, local, runReaderT)
import Control.Monad.State (class MonadState, StateT(..), evalStateT, get, gets, modify, runStateT)
import Control.Monad.Writer (class MonadTell)
import Data.Array (head)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Jarilo (InternalRow_, internal__, notFound__)
import Jarilo.Router.Junction (RequestResult)
import Jarilo.Router.Response (AppResponse)
import Postgres.Async.Query as PostgresAsync
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Error as Postgres
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), QueryConfig(..), QueryParameter)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Yoga.JSON (class ReadForeign)
import Yoga.JSON.Async (read)

data Tstate p q b = Tstate Deployment Pool (Maybe Client) (RequestResult p q b)

newtype Tasync p q b l r = Tasync (ReaderT (Tstate p q b) (Async l) r)

fromAsync ∷ ∀ p q b r l. Async l r -> Tasync p q b l r
fromAsync async = Tasync $ lift async

runTasync ∷ ∀ p q b l r. Tasync p q b l r -> Tstate p q b -> Async l r
runTasync (Tasync stateT) = runReaderT stateT

alwaysRight
    ∷  ∀ p q b inLeft inRight outRight
    .  (inLeft -> outRight)
    -> (inRight -> outRight)
    -> Tasync p q b inLeft inRight
    -> (∀ voidLeft. Tasync p q b voidLeft outRight)
alwaysRight leftFunction rightFunction (Tasync (ReaderT async)) =
    Tasync $ ReaderT \state ->
        Async.alwaysRight leftFunction rightFunction $ async state

unify ∷ ∀ p q b r. Tasync p q b r r -> (∀ voidLeft. Tasync p q b voidLeft r)
unify = alwaysRight identity identity

examineLeftWithEffect ∷ ∀ p q b l r.
    (l -> Effect Unit) -> Tasync p q b l r -> Tasync p q b l r
examineLeftWithEffect examiner tasync =
    Tasync $ ReaderT \state ->
        runTasync tasync state # Async.examineLeftWithEffect examiner

note left' maybe = maybe # Either.note left' # Async.fromEither # fromAsync

derive newtype instance Functor (Tasync p q b l)

instance Bifunctor (Tasync p q b) where
    bimap leftFun rightFun (Tasync (ReaderT asyncFun)) =
        Tasync $ ReaderT \state -> asyncFun state # bimap leftFun rightFun

derive newtype instance Apply (Tasync p q b l)

derive newtype instance Applicative (Tasync p q b l)

derive newtype instance Bind (Tasync p q b l)

derive newtype instance Monad (Tasync p q b l)

derive newtype instance MonadAsk (Tstate p q b) (Tasync p q b l)

derive newtype instance MonadReader (Tstate p q b) (Tasync p q b l)

getDeployment ∷ ∀ m p q b. MonadAsk (Tstate p q b) m => m Deployment
getDeployment = asks \(Tstate deployment _ _ _) -> deployment

getQuerier ∷ ∀ m p q b. MonadAsk (Tstate p q b) m => m (Either Pool Client)
getQuerier = asks \(Tstate _ pool client _) ->
    case client of
        Nothing -> Left pool
        Just client' -> Right client'

getRequest ∷ ∀ m p q b. MonadAsk (Tstate p q b) m => m (RequestResult p q b)
getRequest = asks \(Tstate _ _ _ request) -> request

getCookies ∷ ∀ f p q b. Functor f => MonadAsk (Tstate p q b) f => f (Map String String)
getCookies = getRequest <#> _.cookies

putClient ∷ ∀ m a p q b. MonadReader (Tstate p q b) m => Client -> m a -> m a
putClient client = local \(Tstate deployment pool _ request) ->
    Tstate deployment pool (Just client) request

query ∷ ∀ p q r. Query -> Array QueryParameter -> Tasync p q r Error Result
query string parameters = ask >>= \(Tstate _ pool client' _) ->
    case client' of
        Nothing -> PostgresAsync.query string parameters pool # fromAsync
        Just client -> PostgresAsync.query string parameters client # fromAsync

-- queryInternal ∷ ∀ p q b l. Query -> Array QueryParameter -> Tasync p q b (Variant (InternalRow_ l)) Result
-- queryInternal string parameters = query string parameters # lmap (const internal__)

-- reportDatabaseError ∷ ∀ right errors.
--     Async Postgres.Error right -> Async (InternalTerror_ errors) right
reportDatabaseError ∷ ∀ p q b l r.
    Tasync p q b Postgres.Error r -> Tasync p q b (InternalTerror_ l) r
reportDatabaseError = lmap (databaseErrorLines >>> Terror internal__)

queryInternal ∷ ∀ p q b l.
    Query -> Array QueryParameter -> Tasync p q b (InternalTerror_ l) Result
queryInternal queryString parameters =
    query queryString parameters # reportDatabaseError

-- queryMany ∷ ∀ querier errors rows. Querier querier => ReadForeign rows =>
--     querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) (Array rows)
queryMany queryString parameters = do
    result <- queryInternal queryString parameters
    rows result # traverse read # fromAsync # lmap \errors -> Terror internal__
        [ "Error reading result from database: " <> show errors ]

-- -- queryMany_ ∷ ∀ querier errors rows. Querier querier => ReadForeign rows =>
-- --     querier -> Query -> Async (InternalTerror_ errors) (Array rows)
queryMany_ queryString = queryMany queryString []

-- -- queryFirst
-- --     ∷  ∀ row querier errors
-- --     .  Querier querier
-- --     => ReadForeign row
-- --     => Variant (internal ∷ AppResponse Unit | errors)
-- --     -> querier
-- --     -> Query
-- --     -> Array QueryParameter
-- --     -> Async (InternalTerror_ errors) row
queryFirst
    ∷          ∀ b p q r l
    .  ReadForeign r
    => Variant (internal ∷ AppResponse Unit | l)
    -> Query
    -> Array QueryParameter
    -> Tasync p q b (InternalTerror_ l) r
queryFirst response queryString parameters = do
    rows <- queryMany queryString parameters
    rows # head # note (Terror response [ "Expected at least one row from database, got none." ])

-- -- queryFirstInternal ∷ ∀ row errors querier. Querier querier => ReadForeign row =>
-- --     querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) row
-- queryFirstInternal = queryFirst internal__

-- -- queryFirstInternal_ ∷ ∀ row errors querier. Querier querier => ReadForeign row =>
-- --     querier -> Query -> Async (InternalTerror_ errors) row
-- queryFirstInternal_ pool queryString = queryFirst internal__ pool queryString []

-- -- queryFirstBadRequest ∷ ∀ row errors querier. Querier querier => ReadForeign row =>
-- --     querier -> Query -> Array QueryParameter -> Async (TerrorVar (InternalRow_ + BadRequestRow_ + errors)) row
-- queryFirstBadRequest = queryFirst badRequest__

-- -- queryFirstNotFound ∷ ∀ row errors querier. Querier querier => ReadForeign row =>
-- --     querier -> Query -> Array QueryParameter -> Async (LoadSingleError errors) row
queryFirstNotFound = queryFirst notFound__

-- -- queryFirstMaybe ∷ ∀ errors querier row. Querier querier => ReadForeign row =>
-- --     querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) (Maybe row)
-- queryFirstMaybe pool queryString parameters = do
--     rows <- queryMany pool queryString parameters
--     pure $ head rows

-- -- queryFirstNotAuthorized ∷ ∀ row errors querier. Querier querier => ReadForeign row =>
-- --     querier -> Query -> Array QueryParameter -> Async (ChangeSingleError errors) row
-- queryFirstNotAuthorized = queryFirst notAuthorized__

-- -- queryNone ∷ ∀ querier errors. Querier querier =>
-- --     querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) Unit
-- queryNone querier queryString parameters =
--     querier # execute queryString parameters # reportDatabaseError

-- -- transaction ∷ ∀ result errors.
-- --     (Client -> Async (InternalTerror_ errors) result) -> Pool -> Async (InternalTerror_ errors) result
-- transaction = withTransaction (databaseErrorLines >>> Terror internal__)
