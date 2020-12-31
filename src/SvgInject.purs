module SvgInject where

import Prelude

import Effect (Effect)
import Web.Event.Internal.Types (EventTarget)

foreign import svgInject :: EventTarget -> Effect Unit
