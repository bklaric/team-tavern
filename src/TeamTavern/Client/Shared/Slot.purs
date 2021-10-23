module TeamTavern.Client.Shared.Slot where

import Prelude

import Data.Const (Const)
import Halogen as H

type SimpleSlot = H.Slot (Const Void) Void Unit

type IndexedSlot index = H.Slot (Const Void) Void index

type StringSlot = IndexedSlot String

type QuerylessSlot output index = H.Slot (Const Void) output index
