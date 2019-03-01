module TeamTavern.Session.Start.GenerateToken
    (Token, tokenCharCount, generateToken, unToken) where

import Prelude

import Async (Async)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import TeamTavern.Infrastructure.Cookie as Cookie
import TeamTavern.Infrastructure.GenerateHexString (class HexString, GenerateHexStringError, generateHexString)

newtype Token = Token String

derive instance genericToken :: Generic Token _

instance showToken :: Show Token where show = genericShow

instance cookieToken :: Cookie.Token Token where
    fromToken (Token token) = token

instance hexStringToken :: HexString Token where
    fromHexString = Token

tokenCharCount :: Int
tokenCharCount = 40

generateToken :: forall errors. Async (GenerateHexStringError errors) Token
generateToken = generateHexString tokenCharCount

unToken :: Token -> String
unToken (Token token) = token
