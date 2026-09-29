module TeamTavern.Routes.LlmsTxt.ViewLlmsTxt where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, OkText)

-- | Caddy hands `/llms.txt` to node as it is, outside `/api`.
type ViewLlmsTxt =
    Get_ (Literal "llms.txt")
    ==> OkText "text/plain; charset=utf-8" ! Internal_
