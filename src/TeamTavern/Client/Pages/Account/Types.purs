module TeamTavern.Client.Pages.Account.Types where

type Nickname = String

data PlayerStatus = SamePlayer | SignedIn Nickname | SignedOut
