module TeamTavern.Client.Shared.AccountErrors
    ( emailError
    , emailInvalid
    , nicknameInvalid
    , nicknameTaken
    , passwordShort
    , somethingWrong
    , tooYoung
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

tooYoung :: String
tooYoung = "You must be 16 or older to use TeamTavern."
