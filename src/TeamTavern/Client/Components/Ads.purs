module TeamTavern.Client.Components.Ads where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable, toNullable)
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Seconds(..), negateDuration, toDuration)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Now (now)
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Subscription as HSub
import Prim.Row (class Cons)
import TeamTavern.Client.Shared.Slot (SlotQ__)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))
import Web.HTML (HTMLElement)

type AdSlots slots =
    ( desktopTakeover :: SlotQ__ Query
    , mobileTakeover :: SlotQ__ Query
    , billboard :: SlotQ__ Query
    , leaderboard :: SlotQ__ Query
    , mobileBanner :: SlotQ__ Query
    , mobileMpu :: SlotQ__ Query
    , video :: SlotQ__ Query
    , verticalSticky :: SlotQ__ Query
    , horizontalSticky :: SlotQ__ Query
    , mobileHorizontalSticky :: SlotQ__ Query
    | slots
    )

desktopTakeover :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
desktopTakeover = slot (Proxy :: _ "desktopTakeover") "desktop_takeover"

mobileTakeover :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
mobileTakeover = slot (Proxy :: _ "mobileTakeover") "mobile_takeover"

billboard :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
billboard = slot (Proxy :: _ "billboard") "billboard"

leaderboard :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
leaderboard = slot (Proxy :: _ "leaderboard") "leaderboard"

mobileBanner :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
mobileBanner = slot (Proxy :: _ "mobileBanner") "mobile_banner"

mobileMpu :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
mobileMpu = slot (Proxy :: _ "mobileMpu") "mobile_mpu"

video :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
video = slot (Proxy :: _ "video") "video"

verticalSticky :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
verticalSticky = slot (Proxy :: _ "verticalSticky") "vertical_sticky"

horizontalSticky :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
horizontalSticky = slot (Proxy :: _ "horizontalSticky") "horizontal_sticky"

mobileHorizontalSticky :: forall monad action slots. MonadEffect monad =>
    HH.ComponentHTML action (AdSlots slots) monad
mobileHorizontalSticky = slot (Proxy :: _ "mobileHorizontalSticky") "mobile_horizontal_sticky"

slot :: forall action output slots monad label slots'
    .  Cons label (H.Slot Query output Unit) slots' slots
    => IsSymbol label
    => MonadEffect monad
    => Proxy label
    -> String
    -> HH.ComponentHTML action slots monad
slot proxy placementName = HH.slot_ proxy unit (component placementName) unit

foreign import data Placement :: Type

foreign import createAd :: String -> Nullable HTMLElement -> (Placement -> Effect Unit) -> Effect Unit

foreign import removeAd :: String -> Nullable Placement -> Effect Unit

foreign import refreshAd :: String -> Nullable Placement -> (Placement -> Effect Unit) -> Effect Unit

data Query send = Refresh send

component :: forall input output monad. MonadEffect monad =>
    String -> H.Component Query input output monad
component placementName = Hooks.component \{queryToken} _ -> Hooks.do
    lastRefreshInstant /\ lastRefreshInstantId <- Hooks.useState Nothing
    placement /\ placementId <- Hooks.useState Nothing
    pubSub /\ pubSubId <- Hooks.useState Nothing
    let writePlacement listener placement' = HSub.notify listener (Hooks.put placementId (Just placement'))
    Hooks.useQuery queryToken \(Refresh send) -> do
        -- Let's try refreshing only if the ad has been up for at least 5 seconds.
        now' <- now <#> unInstant <#> toDuration # liftEffect
        lastRefreshInstant' <- lastRefreshInstant # maybe now pure <#> unInstant <#> toDuration # liftEffect
        when ((now' <> negateDuration lastRefreshInstant') > (Seconds 5.0)) do
            now <#> Just # liftEffect >>= Hooks.put lastRefreshInstantId
            pubSub # foldMap \{listener} ->
                refreshAd placementName (toNullable placement) (writePlacement listener) # liftEffect
        pure $ Just $ send
    Hooks.useLifecycleEffect do
        now <#> Just # liftEffect >>= Hooks.put lastRefreshInstantId

        pubSub' @ {emitter, listener} <- HSub.create # liftEffect
        Hooks.put pubSubId $ Just pubSub'
        _ <- Hooks.subscribe emitter

        elementMaybe <- Hooks.getHTMLElementRef (RefLabel "ad")

        createAd placementName (toNullable elementMaybe) (writePlacement listener) # liftEffect
        pure $ Just do
            placementMaybe <- Hooks.get placementId
            removeAd placementName (toNullable placementMaybe) # liftEffect
    Hooks.pure $ HH.div [HP.ref $ RefLabel "ad", HS.class_ $ "ad " <> placementName] []

-- Utils

insertAdsInMiddle :: forall  action monad slots. MonadEffect monad =>
    Array (HH.ComponentHTML action (AdSlots slots) monad) -> Array (HH.ComponentHTML action (AdSlots slots) monad)
insertAdsInMiddle array =
    -- Try to insert after the third element.
    case Array.insertAt 3 billboard array >>= Array.insertAt 4 mobileTakeover of
    Nothing -> array
    Just arrayWithAds ->
    -- Try to insert after the sixth element, which is now the eight.
        case Array.insertAt 8 leaderboard arrayWithAds >>= Array.insertAt 9 mobileMpu of
        Nothing -> arrayWithAds
        Just arrayWithMoreAds -> arrayWithMoreAds

videoIfWideEnough :: forall monad action slots. MonadEffect monad =>
    Int -> Array (HH.ComponentHTML action (AdSlots slots) monad)
videoIfWideEnough windowWidth = guard (windowWidth >= 1000) [video]
