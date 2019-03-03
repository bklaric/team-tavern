module TeamTavern.Session.Domain.NicknameOrEmail where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

newtype NicknameOrEmail = NicknameOrEmail String

derive instance newtypeNicknameOrString :: Newtype NicknameOrEmail _

derive instance genericNicknameOrEmail :: Generic NicknameOrEmail _

instance showNicknameOrEmail :: Show NicknameOrEmail where show = genericShow
