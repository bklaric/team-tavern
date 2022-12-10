module Perun.Async.Request.Body where

import Prelude

import Async (Async, fromEffectCont)
import Perun.Request.Body (Body, readAsUtf8)

readBody :: ∀ left. Body -> Async left String
readBody body = flip readAsUtf8 body >>> void # fromEffectCont
