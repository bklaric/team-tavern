module TeamTavern.Client.Script.PopStateEvent (create, module PES) where

import Effect (Effect)
import Web.HTML.Event.PopStateEvent (PopStateEvent)
import Web.HTML.Event.PopStateEvent (PopStateEvent, fromEvent, state, toEvent) as PES

foreign import create :: Effect PopStateEvent
