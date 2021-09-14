module TeamTavern.Client.Shared.Slot where

import Prelude

import Data.Const (Const)
import Halogen as H

type SimpleSlot = H.Slot (Const Void) Void Unit
