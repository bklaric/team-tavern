module TeamTavern.Client.Script.PopStateEvent (create, module PES) where

import Effect (Effect)
import Foreign (Foreign)
import Web.HTML.Event.PopStateEvent (PopStateEvent)
import Web.HTML.Event.PopStateEvent (PopStateEvent, fromEvent, state, toEvent) as PES

foreign import create :: Foreign -> Effect PopStateEvent
