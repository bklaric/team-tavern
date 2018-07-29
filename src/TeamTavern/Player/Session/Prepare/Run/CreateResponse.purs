module TeamTavern.Player.Session.Prepare.Run.CreateResponse
    (prepareResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import MultiMap (empty)
import Perun.Response (Response)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Session.Prepare.Run.Types (PrepareError)

type ErrorResponseContent = Variant
    ( invalidNickname :: {}
    , unknownNickname :: {}
    )

invalidNickname :: ErrorResponseContent
invalidNickname = inj (SProxy :: SProxy "invalidNickname") {}

unknownNickname :: ErrorResponseContent
unknownNickname = inj (SProxy :: SProxy "unknownNickname") {}

errorResponse :: PrepareError -> Response
errorResponse = match
    { ensureNotSignedIn: const
        { statusCode: 403
        , headers: empty
        , content: mempty
        }
    , readNickname: const
        { statusCode: 400
        , headers: empty
        , content: writeJSON invalidNickname
        }
    , generateSecrets: const
        { statusCode: 500
        , headers: empty
        , content: mempty
        }
    , createSession: _.error >>> match
        { unknownNickname : const
            { statusCode: 400
            , headers: empty
            , content: writeJSON unknownNickname
            }
        , invalidEmail: const
            { statusCode: 500
            , headers: empty
            , content: mempty
            }
        , other: const
            { statusCode: 500
            , headers: empty
            , content: mempty
            }
        }
    , notifyPlayer: const
        { statusCode: 500
        , headers: empty
        , content: mempty
        }
    }

successResponse :: Response
successResponse =
    { statusCode: 204
    , headers: empty
    , content: mempty
    }

prepareResponse ::
    Async PrepareError Unit -> (forall left. Async left Response)
prepareResponse = alwaysRight errorResponse (const successResponse)
