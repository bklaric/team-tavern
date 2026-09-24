module TeamTavern.Routes.Account.UpdateFacts where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Internal_, Literal, NoContent, NotAuthorized_, PutJson_)
import TeamTavern.Routes.Shared.Post (AccountContent)

-- | The facts and contacts every post of the player's shows, written as given:
-- | one left empty is cleared. No post is renewed (brief 6, step 3).
type UpdateFacts =
    PutJson_ (Literal "account" / Literal "facts") RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

type RequestContent =
    { nickname :: String
    , account :: AccountContent
    }

-- | `field` is a fact by its key: `location`, `languages`, `birthday` or
-- | `timezone`.
type FactError = Variant
    ( nickname :: {}
    , contact :: { kind :: String }
    , field :: { key :: String }
    )

type BadContent = Variant
    ( facts :: NonEmptyArray FactError
    , nicknameTaken :: {}
    )
