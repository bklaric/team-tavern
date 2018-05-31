module TeamTavern.Player.Credentials where

import TeamTavern.Player.Email (Email)
import TeamTavern.Player.Nickname (Nickname)
import TeamTavern.Player.Token (Token)

type Credentials = { email :: Email, nickname :: Nickname, token :: Token }
