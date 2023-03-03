module TeamTavern.Client.Components.RegistrationInputDiscord where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Input (inputGroup, inputLabel_, requiredTextLineInputNamed)
import TeamTavern.Client.Components.InputError as InputError

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
    <> InputError.nicknameError nicknameError
    <> InputError.nicknameTaken nicknameTaken
