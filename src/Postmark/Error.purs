module Postmark.Error where

type Error =
    { status :: Int
    , message :: String
    , code :: Int
    }
