module TeamTavern.Client.Pages.Player.Types where

type Nickname = String

data PlayerStatus = SamePlayer | SignedIn Nickname | SignedOut
