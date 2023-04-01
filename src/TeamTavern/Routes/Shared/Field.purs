module TeamTavern.Routes.Shared.Field where

import Data.Maybe (Maybe)

type Option =
    { key :: String
    , label :: String
    }

type Options = Array Option

type Field =
    { ilk :: Int
    , key :: String
    , label :: String
    , icon :: String
    , options :: Options
    }

type Fields = Array Field

type Value =
    { field ::
        { ilk :: Int
        , key :: String
        , label :: String
        , icon :: String
        }
    , option :: Maybe Option
    , options :: Maybe Options
    }

type Values = Array Value

type ValueMulti =
    { field ::
        { ilk :: Int
        , key :: String
        , label :: String
        , icon :: String
        }
    , options :: Options
    }

type ValuesMulti = Array ValueMulti

type ValueSimple =
    { fieldKey :: String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type ValuesSimple = Array ValueSimple

type ValueSimpleMulti =
    { fieldKey :: String
    , optionKeys :: Array String
    }

type ValuesSimpleMulti = Array ValueSimpleMulti
