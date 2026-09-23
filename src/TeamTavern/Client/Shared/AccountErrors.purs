module TeamTavern.Client.Shared.AccountErrors
    ( nicknameInvalid
    , nicknameTaken
    , passwordShort
    , somethingWrong
    ) where

-- What the account pages say when what was entered is turned down.

nicknameInvalid :: String
nicknameInvalid = "Use up to 40 letters, digits, dashes, underscores and dots, without spaces."

nicknameTaken :: String
nicknameTaken = "This nickname is taken. Please pick another one."

passwordShort :: String
passwordShort = "Use at least 8 characters."

somethingWrong :: String
somethingWrong = "Something went wrong. Please try again."
