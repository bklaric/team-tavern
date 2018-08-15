module TeamTavern.Client.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { isOn :: Boolean, toggleCount :: Int }

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

home :: forall m. H.Component HH.HTML Query Unit Message m
home =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = { isOn: false, toggleCount: 0 }

  render :: State -> H.ComponentHTML Query () m
  render { isOn, toggleCount } =
    let
      label = if isOn then "On, after being toggled " <> show toggleCount <> " times"  else "Off"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Query ~> H.HalogenM State Query () Message m
  eval = case _ of
    Toggle next -> do
      -- H.modify (_ { isOn: not isOn, toggleCount: if isOn then toggleCount else toggleCount + 1 })
      { isOn, toggleCount } <- H.get
      let nextState = { isOn: not isOn, toggleCount: if isOn then toggleCount else toggleCount + 1 }
      H.put nextState
      H.raise $ Toggled nextState.isOn
      pure next
    IsOn reply -> do
      { isOn } <- H.get
      pure (reply isOn)
