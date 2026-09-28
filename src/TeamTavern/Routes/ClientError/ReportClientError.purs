module TeamTavern.Routes.ClientError.ReportClientError where

import Data.Maybe (Maybe)
import Jarilo (type (/), type (==>), Literal, NoContent, PostJson_)

-- | A page telling the server it failed where it didn't expect to: a request
-- | answered with a failure the page doesn't handle, a response that isn't
-- | what the route declares, or its own code throwing. The server only logs
-- | it, so nothing answers but no content.
type ReportClientError =
    PostJson_ (Literal "client" / Literal "errors") RequestContent
    ==> NoContent

-- | Paths only, without their query strings, which carry the nonces of the
-- | links in emails.
type RequestContent =
    { page :: String
    , request :: Maybe String
    , failure :: String
    , bundle :: String
    }
