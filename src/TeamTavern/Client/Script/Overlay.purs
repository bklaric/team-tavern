module TeamTavern.Client.Script.Overlay (hold) where

import Prelude

import Effect (Effect)
import Web.HTML (HTMLElement)

-- Holds the page while the overlay in the layer is open: moves the focus into
-- it, closes it on Escape and, unless it is modal, on a press outside it.
-- A modal one also makes the page behind it inert, locks its scroll and keeps
-- Tab inside it. The effect returned lets go of all of that and gives the focus
-- back to the element that had it before.
foreign import hold :: HTMLElement -> Boolean -> Effect Unit -> Effect (Effect Unit)
