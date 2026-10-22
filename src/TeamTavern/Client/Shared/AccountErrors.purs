module TeamTavern.Client.Shared.AccountErrors
    ( emailError
    , emailInvalid
    , inTime
    , nicknameInvalid
    , nicknameTaken
    , passwordShort
    , somethingWrong
    , tooManySignUps
    , tooYoung
    , tryAgainIn
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, trim)

-- What the account pages say when what was entered is turned down.

emailInvalid :: String
emailInvalid = "Enter your email address."

-- | What the page can tell of an address before the server checks it.
emailError :: String -> Maybe String
emailError email =
    if contains (Pattern "@") email' && contains (Pattern ".") email' then Nothing else Just emailInvalid
    where
    email' = trim email

nicknameInvalid :: String
nicknameInvalid = "Use up to 40 letters, digits, dashes, underscores and dots, without spaces."

nicknameTaken :: String
nicknameTaken = "This nickname is taken. Please pick another one."

passwordShort :: String
passwordShort = "Use at least 8 characters."

somethingWrong :: String
somethingWrong = "Something went wrong. Please try again."

tooManySignUps :: String
tooManySignUps = "Too many sign-ups from here."

tooYoung :: String
tooYoung = "You must be 16 or older to use TeamTavern."

-- | When to try again, `seconds` from now, after the site has turned away
-- | one try too many.
tryAgainIn :: Int -> String
tryAgainIn seconds = "Try again " <> inTime seconds <> "."

-- | `seconds` from now, as "in 5 minutes".
inTime :: Int -> String
inTime seconds
    | seconds <= 60 = "in a minute"
    | seconds <= 90 * 60 = "in " <> show ((seconds + 59) / 60) <> " minutes"
    | otherwise = "in " <> show ((seconds + 1799) / 3600) <> " hours"
