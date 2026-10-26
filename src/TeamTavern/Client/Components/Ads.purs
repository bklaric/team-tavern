module TeamTavern.Client.Components.Ads (Slot, Spot(..), around, banner, beside, spot, video) where

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

-- Venatus's own breakpoints, which every width falls in one of. A placement
-- has sizes for one of them only, so it is asked for only there, and
-- Ads.scss keeps room for a unit by the same queries.
desktop :: String
desktop = "(width > 1024px)"

mobile :: String
mobile = "(width <= 1024px)"

-- Where the rails' units fit on either side of a column, a 160 px skyscraper
-- and then a 300 px double MPU.
type Rails = { skyscraper :: String, doubleMpu :: String }

-- Beside the feed's column.
pageRails :: Rails
pageRails =
    { skyscraper: "(1140px <= width < 1420px)"
    , doubleMpu: "(width >= 1420px)"
    }

-- Beside the inbox, which narrows to keep them in view down to the width a
-- conversation's messages need.
inboxRails :: Rails
inboxRails =
    { skyscraper: "(1336px <= width < 1616px)"
    , doubleMpu: "(width >= 1616px)"
    }

-- The video floats over the window's bottom right corner, which clears a
-- rail's 600 px unit, below the takeover at the top of the page, only in a
-- window this tall.
videoMedia :: String
videoMedia = "(width > 1024px) and (height >= 1300px)"

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

-- A Venatus placement over the window, which Venatus draws itself.
floating :: ∀ query input output m. MonadEffect m => String -> String -> H.Component query input output m
floating placement media = Hooks.component \_ _ -> Hooks.do
    Hooks.useLifecycleEffect do
        unmount <- liftEffect $ mount placement media null
        pure $ Just $ liftEffect unmount
    Hooks.pure $ HH.text ""

ad :: ∀ action slots m. MonadEffect m =>
    String -> H.Component (Const Void) Unit Void m -> HH.ComponentHTML action (ad :: Slot | slots) m
ad key component = HH.slot_ (Proxy :: _ "ad") key component unit

rail :: ∀ action slots m. MonadEffect m => Rails -> String -> HH.ComponentHTML action (ad :: Slot | slots) m
rail { skyscraper, doubleMpu } side =
    HH.aside [ HS.class_ $ "ad-rail ad-rail-" <> side, HPA.label "Advertisement" ]
    [ ad ("rail-" <> side <> "-skyscraper") $ inPage "skyscraper" skyscraper
    , ad ("rail-" <> side <> "-double-mpu") $ inPage "double_mpu" doubleMpu
    ]

-- | A page's ads around its content: a takeover above it and a rail on either
-- | side on a desktop, and a sticky on the window's floor on a desktop and on
-- | a phone. A page keeps its units while it renders, and a new visit mounts
-- | the page, and so its units, again.
-- |
-- | An ad blocker hides elements by class names its filter lists know, so the
-- | units keep names that say ad, and go with the ads, while the frame and the
-- | page's own content carry none.
around :: ∀ action slots m. MonadEffect m =>
    HH.ComponentHTML action (ad :: Slot | slots) m -> HH.ComponentHTML action (ad :: Slot | slots) m
around content =
    HH.div [ HS.class_ "page-frame" ]
    [ HH.div [ HS.class_ "ad-takeover" ] [ ad "takeover" $ inPage "desktop_takeover" desktop ]
    , rail pageRails "left"
    , HH.div [ HS.class_ "page-frame-main" ] [ content ]
    , rail pageRails "right"
    , ad "bottom" $ floating "horizontal_sticky" desktop
    , ad "phone-bottom" $ floating "mobile_horizontal_sticky" mobile
    ]

-- | The inbox's ads: a rail on either side and nothing else, since the inbox
-- | fills the window and a sticky on its floor would cover the composer.
beside :: ∀ action slots m. MonadEffect m =>
    HH.ComponentHTML action (ad :: Slot | slots) m -> HH.ComponentHTML action (ad :: Slot | slots) m
beside content =
    HH.div [ HS.class_ "inbox-frame" ]
    [ rail inboxRails "left"
    , HH.div [ HS.class_ "inbox-frame-main" ] [ content ]
    , rail inboxRails "right"
    ]

-- | The two places in a page's content that take a unit, each a desktop
-- | placement and the mobile one in its place.
data Spot = First | Second

-- | A unit among a page's content, which shows as wide as it comes, wider than
-- | the column on the narrowest phones.
spot :: ∀ action slots m. MonadEffect m => Spot -> HH.ComponentHTML action (ad :: Slot | slots) m
spot First =
    HH.div [ HS.class_ "ad-spot" ]
    [ ad "billboard" $ inPage "billboard" desktop
    , ad "mobile-takeover" $ inPage "mobile_takeover" mobile
    ]
spot Second =
    HH.div [ HS.class_ "ad-spot" ]
    [ ad "leaderboard" $ inPage "leaderboard" desktop
    , ad "mobile-mpu" $ inPage "mobile_mpu" mobile
    ]

-- | A banner near the top of a page on a phone, whose room is kept from the
-- | start, as the takeover's is.
banner :: ∀ action slots m. MonadEffect m => HH.ComponentHTML action (ad :: Slot | slots) m
banner = HH.div [ HS.class_ "ad-banner" ] [ ad "mobile-banner" $ inPage "mobile_banner" mobile ]

-- | A video floating over the window's bottom right corner on a desktop.
video :: ∀ action slots m. MonadEffect m => HH.ComponentHTML action (ad :: Slot | slots) m
video = ad "video" $ floating "video_slider" videoMedia
