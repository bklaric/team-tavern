module TeamTavern.Client.Components.Ads (Slot, around) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toNullable)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))
import Web.HTML (HTMLElement)

type Slot = Slot__String

foreign import mount :: String -> String -> Nullable HTMLElement -> Effect (Effect Unit)

desktop :: String
desktop = "(min-width: 1024px)"

-- Where a 160 px skyscraper fits on either side of the feed's column.
rails :: String
rails = "(min-width: 1100px)"

phone :: String
phone = "(max-width: 639px)"

adRef :: H.RefLabel
adRef = H.RefLabel "ad"

-- A Venatus placement shown into its element while the media query matches.
inPage :: ∀ query input output m. MonadEffect m => String -> String -> H.Component query input output m
inPage placement media = Hooks.component \_ _ -> Hooks.do
    Hooks.useLifecycleEffect do
        element <- Hooks.getHTMLElementRef adRef
        unmount <- liftEffect $ mount placement media $ toNullable element
        pure $ Just $ liftEffect unmount
    Hooks.pure $ HH.div [ HP.ref adRef, HS.class_ "ad" ] []

-- A Venatus placement on the window's floor, which Venatus draws itself.
floor :: ∀ query input output m. MonadEffect m => String -> String -> H.Component query input output m
floor placement media = Hooks.component \_ _ -> Hooks.do
    Hooks.useLifecycleEffect do
        unmount <- liftEffect $ mount placement media null
        pure $ Just $ liftEffect unmount
    Hooks.pure $ HH.text ""

ad :: ∀ action slots m. MonadEffect m =>
    String -> H.Component (Const Void) Unit Void m -> HH.ComponentHTML action (ad :: Slot | slots) m
ad key component = HH.slot_ (Proxy :: _ "ad") key component unit

rail :: ∀ action slots m. MonadEffect m => String -> HH.ComponentHTML action (ad :: Slot | slots) m
rail side =
    HH.aside [ HS.class_ $ "ad-rail ad-rail-" <> side, HPA.label "Advertisement" ]
    [ ad ("rail-" <> side) $ inPage "skyscraper" rails ]

-- | A page's ads around its content: a takeover above it and a skyscraper on
-- | either side on a desktop, and a sticky on the window's floor on a desktop
-- | and on a phone. A page keeps its units while it renders, and a new visit
-- | mounts the page, and so its units, again.
around :: ∀ action slots m. MonadEffect m =>
    HH.ComponentHTML action (ad :: Slot | slots) m -> HH.ComponentHTML action (ad :: Slot | slots) m
around content =
    HH.div [ HS.class_ "ads-page" ]
    [ HH.div [ HS.class_ "ad-takeover" ] [ ad "takeover" $ inPage "desktop_takeover" desktop ]
    , rail "left"
    , HH.div [ HS.class_ "ads-content" ] [ content ]
    , rail "right"
    , ad "bottom" $ floor "horizontal_sticky" desktop
    , ad "phone-bottom" $ floor "mobile_horizontal_sticky" phone
    ]
