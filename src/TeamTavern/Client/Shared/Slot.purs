module TeamTavern.Client.Shared.Slot where

import Prelude

import Data.Const (Const)
import Halogen as H

type Slot___ = H.Slot (Const Void) Void Unit

type Slot__I index = H.Slot (Const Void) Void index

type Slot_O_ output = H.Slot (Const Void) output Unit

type SlotQ__ query = H.Slot query Void Unit

type Slot_OI output index = H.Slot (Const Void) output index

type SlotQ_I query index = H.Slot query Void index

type Slot__String = Slot__I String
