module TeamTavern.Player.Session.Start.Types.ErrorModel
    ( SignInErrorModel
    , fromSignInError
    ) where

import Prelude

import Data.Variant (SProxy(..), Variant, inj, match)
import TeamTavern.Player.Session.Start.Types.Error (SignInError)

type SignInErrorModel = Variant
    ( signedIn :: {}
    , invalidNickname :: {}
    , invalidNonce :: {}
    , noTokenToConsume :: {}
    , other :: {}
    )

_signedIn = SProxy :: SProxy "signedIn"

_invalidNickname = SProxy :: SProxy "invalidNickname"

_invalidNonce = SProxy :: SProxy "invalidNonce"

_noTokenToConsume = SProxy :: SProxy "noTokenToConsume"

_other = SProxy :: SProxy "other"

fromSignInError :: SignInError -> SignInErrorModel
fromSignInError = match
    { ensureNotSignedIn: const $ inj _signedIn {}
    , readNickname: const $ inj _invalidNickname {}
    , readNonce: match
        { invalidBody: const $ inj _other {}
        , invalidNonce: const $ inj _invalidNonce {}
        }
    , consumeToken: match
        { noTokenToConsume: const $ inj _noTokenToConsume {}
        , cantReadIdentifiedToken: const $ inj _other {}
        , other: const $ inj _other {}
        }
    }
