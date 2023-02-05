module TeamTavern.Client.Components.RegistrationInputDiscord where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel_, requiredTextLineInputNamed)

type Input =
    { nickname :: String
    , nicknameError :: Boolean
    , nicknameTaken :: Boolean
    }

type Output = {nickname :: String}

emptyInput :: Input
emptyInput =
    { nickname: ""
    , nicknameError: false
    , nicknameTaken: false
    }

registrationInputDiscord :: forall s a. Input -> (Output -> a) -> HH.HTML s a
registrationInputDiscord {nickname, nicknameError, nicknameTaken} updateNickname =
    inputGroup $
    [ inputLabel_ "Nickname"
    , requiredTextLineInputNamed "nickname" nickname ({nickname: _} >>> updateNickname)
    ]
    <> inputError nicknameError """Nickname cannot be more than 40 characters long
        and can only contain alphanumeric characters, dashes, underscores and dots."""
    <> inputError nicknameTaken "This nickname is already taken, please pick another one."
