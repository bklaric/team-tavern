module TeamTavern.Player.View where

import Prelude

import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.PlayerId (PlayerId)

newtype About = About String

derive instance newtypeAbout :: Newtype About _

type PlayerView = { id :: PlayerId, nickname :: Nickname, about :: About }

data ViewF result
    = ReadNickname (Nickname -> result)
    | LoadPlayer Nickname (PlayerView -> result)

derive instance functorViewF :: Functor ViewF

_view = SProxy :: SProxy "view"

readNickname :: forall run. Run (view :: FProxy ViewF | run) Nickname
readNickname = lift _view (ReadNickname identity)

loadPlayer :: forall run.
    Nickname -> Run (view :: FProxy ViewF | run) PlayerView
loadPlayer nickname = lift _view (LoadPlayer nickname identity)

view :: forall run. Run (view :: FProxy ViewF | run) PlayerView
view = readNickname >>= loadPlayer
