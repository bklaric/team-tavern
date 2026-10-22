module TeamTavern.Server.Infrastructure.Bcrypt (Bcrypt, comparePassword, createBcrypt, hashPassword) where

import Prelude

import Async (Async, attempt, fromEffect, fromEffectCont, fromEither)
import Bcrypt.Async as Bcrypt
import Data.CatQueue (CatQueue, empty, snoc, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import JavaScript.Node.Errors (Error)

-- | bcrypt runs on libuv's four threads, which `dns.lookup` and crypto's
-- | `randomBytes` share: every outbound call and new database connection looks
-- | its host up there, and every session token is drawn there. So at most two
-- | bcrypt operations run at once and the rest wait their turn, which leaves
-- | two threads free however many passwords come in.
newtype Bcrypt = Bcrypt { running :: Ref Int, waiting :: Ref (CatQueue (Effect Unit)) }

turns :: Int
turns = 2

createBcrypt :: Effect Bcrypt
createBcrypt = do
    running <- Ref.new 0
    waiting <- Ref.new empty
    pure $ Bcrypt { running, waiting }

takeTurn :: ∀ left. Bcrypt -> Async left Unit
takeTurn (Bcrypt { running, waiting }) = fromEffectCont \start -> do
    count <- Ref.read running
    if count < turns
    then Ref.write (count + 1) running *> start unit
    else Ref.modify_ (_ `snoc` start unit) waiting

-- | Hands the turn to the first in line, if anyone is waiting.
endTurn :: Bcrypt -> Effect Unit
endTurn (Bcrypt { running, waiting }) = do
    queue <- Ref.read waiting
    case uncons queue of
        Just (Tuple next rest) -> Ref.write rest waiting *> next
        Nothing -> Ref.modify_ (_ - 1) running

inTurn :: ∀ right. Bcrypt -> Async Error right -> Async Error right
inTurn bcrypt async = do
    takeTurn bcrypt
    result <- attempt async
    fromEffect $ endTurn bcrypt
    fromEither result

hashPassword :: Bcrypt -> String -> Async Error String
hashPassword bcrypt password = inTurn bcrypt $ Bcrypt.hash_ password

comparePassword :: Bcrypt -> String -> String -> Async Error Boolean
comparePassword bcrypt password hash = inTurn bcrypt $ Bcrypt.compare password hash
