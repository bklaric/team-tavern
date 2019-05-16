module TeamTavern.Client.Components.WelcomeBanner (Slot, welcomeBanner) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

data Action = Init | Close

data State = Shown | Closed

type Slot = H.Slot (Const Void) Void

render :: forall monad. State -> H.ComponentHTML Action () monad
render Shown = HH.div
    [ HP.class_ (H.ClassName "welcome-banner") ]
    [ HH.p_ [ HH.text "Welcome to TeamTavern!" ]
    , HH.p_ [ HH.text "TeamTavern is a place to find teammates to play online games with." ]
    , HH.p_ [ HH.text "Select a game below, browse profiles of other players and find your ideal teammates." ]
    , HH.p_ [ HH.text "To also let other players find you, register and create your own profile." ]
    , HH.p_ [ HH.text "If a game you're looking for is not listed, you can create an entry for it after registering." ]
    , HH.p_ [ HH.text "Happy playing!" ]
    , HH.button [ HE.onClick $ const $ Just Close ] [ HH.text "Close" ]
    ]
render Closed = HH.div [ HP.class_ (H.ClassName "empty") ] []

welcomedKey :: String
welcomedKey = "welcomed"

isWelcomed :: Effect Boolean
isWelcomed = window >>= localStorage >>= getItem welcomedKey <#> isJust

setWelcomed :: Effect Unit
setWelcomed = window >>= localStorage >>= setItem welcomedKey "true"

handleAction :: forall output monad. MonadEffect monad =>
    Action -> H.HalogenM State Action () output monad Unit
handleAction Init = do
    isWelcomed # H.liftEffect <#> (if _ then Closed else Shown) >>= H.put
    pure unit
handleAction Close = do
    H.liftEffect setWelcomed
    H.put Closed
    pure unit

component :: forall query input output monad. MonadEffect monad =>
    H.Component HH.HTML query input output monad
component = H.mkComponent
    { initialState: const Closed
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

welcomeBanner :: forall query children monad. MonadEffect monad =>
    HH.ComponentHTML query (welcomeBanner :: Slot Unit | children) monad
welcomeBanner =
    HH.slot (SProxy :: SProxy "welcomeBanner") unit component unit absurd
