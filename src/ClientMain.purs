module ClientMain where

import Prelude

import Async (Async, runAsync)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff)
import Halogen (hoist)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import TeamTavern.Client.SignIn (signIn)

asyncToAff
    :: forall left right
    .  (left -> Error)
    -> Async left right
    -> Aff right
asyncToAff toError async = async # lmap toError # flip runAsync # (\cont ->
    \callback -> cont callback <#> mempty) # makeAff

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (hoist (asyncToAff absurd) signIn) unit body
