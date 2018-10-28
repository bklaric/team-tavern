module TeamTavern.Client.Blocks.WelcomeBanner (Query, Slot, welcomeBanner) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

data Query send = Init send | Close send

data State = Shown | Closed

type Slot = H.Slot Query Void

render :: forall left. State -> H.ComponentHTML Query () (Async left)
render Shown = HH.div
    [ HP.class_ (H.ClassName "welcome-banner") ]
    [ HH.p_ [ HH.text "Welcome to TeamTavern!" ]
    , HH.p_ [ HH.text $ "Tired of random matchmaking? "
        <> "Missing a teammate for a co-op play? "
        <> "So are other players and you can meet them here." ]
    , HH.p_ [ HH.text $ "Choose a game, browse player profiles "
        <> "and find your ideal teammates. "
        <> "Or make your own profile and let them find you." ]
    , HH.p_ [ HH.text "Happy playing!" ]
    , HH.button [ HE.onClick $ HE.input_ Close] [ HH.text "Close" ]
    ]
render Closed = HH.div [ HP.class_ (H.ClassName "empty") ] []

welcomedKey :: String
welcomedKey = "welcomed"

isWelcomed :: Effect Boolean
isWelcomed = window >>= localStorage >>= getItem welcomedKey <#> isJust

setWelcomed :: Effect Unit
setWelcomed = window >>= localStorage >>= setItem welcomedKey "true"

eval :: forall left. Query ~> H.HalogenM State Query () Void (Async left)
eval (Init send) = do
    isWelcomed # H.liftEffect <#> (if _ then Closed else Shown) >>= H.put
    pure send
eval (Close send) = do
    H.liftEffect setWelcomed
    H.put Closed
    pure send

component :: forall input left.
    H.Component HH.HTML Query input Void (Async left)
component = H.component
    { initialState: const Closed
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }

welcomeBanner :: forall query children left.
    HH.ComponentHTML query (welcomeBanner :: Slot Unit | children) (Async left)
welcomeBanner =
    HH.slot (SProxy :: SProxy "welcomeBanner") unit component unit absurd
