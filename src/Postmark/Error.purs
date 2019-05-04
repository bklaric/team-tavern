module Postmark.Error where

type Error =
    { code :: Int
    , message :: String
    , statusCode :: Int
    }
