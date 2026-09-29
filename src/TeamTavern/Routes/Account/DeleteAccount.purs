module TeamTavern.Routes.Account.DeleteAccount where

import Jarilo (type (!), type (==>), Delete_, Internal_, Literal, NoContent, NotAuthorized_)

-- | Deletes the account with its posts and every conversation it is in, on its
-- | posts and on others' (brief 10), and signs the browser out.
type DeleteAccount =
    Delete_ (Literal "account")
    ==> NoContent ! NotAuthorized_ ! Internal_
