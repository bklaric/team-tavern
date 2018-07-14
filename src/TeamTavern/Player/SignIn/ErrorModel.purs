module TeamTavern.Player.SignIn.ErrorModel
    ( SignInErrorModel
    , fromSignInError
    ) where

import Prelude

import Data.Variant (SProxy(..), Variant, inj, match)
import TeamTavern.Player.SignIn.Error (SignInError)

type SignInErrorModel = Variant
    ( signedIn :: {}
    , invalidNickname :: {}
    , invalidToken :: {}
    , noTokenToConsume :: {}
    , other :: {}
    )

_signedIn = SProxy :: SProxy "signedIn"

_invalidNickname = SProxy :: SProxy "invalidNickname"

_invalidToken = SProxy :: SProxy "invalidToken"

_noTokenToConsume = SProxy :: SProxy "noTokenToConsume"

_other = SProxy :: SProxy "other"

fromSignInError :: SignInError -> SignInErrorModel
fromSignInError = match
    { ensureNotSignedIn: const $ inj _signedIn {}
    , readNickname: const $ inj _invalidNickname {}
    , readToken: match
        { invalidBody: const $ inj _other {}
        , invalidToken: const $ inj _invalidToken {}
        }
    , consumeToken: match
        { noTokenToConsume: const $ inj _noTokenToConsume {}
        , other: const $ inj _other {}
        }
    }
