module TeamTavern.Client.Components.Overlay
    ( Presentation(..)
    , UseOverlay
    , overlay
    , useOverlay
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (type (<>), Hook, HookM, HookType, Pure, UseEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (class HookNewtype)
import Halogen.Subscription as Subscription
import TeamTavern.Client.Components.Button (iconButton)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Overlay (hold)
import TeamTavern.Client.Snippets.Class as HS

-- One overlay, presented as what the screen calls for. A modal is centred over
-- the page and a side panel stands at its right edge, both from 640 px and full
-- screen below it; a bottom sheet holds a menu of a few short rows on a phone.
-- All four are modal. A dropdown hangs from the button that opened it, inside
-- the element carrying the class it is given, and the page stays usable.
data Presentation
    = Modal
    | Side
    | FullScreen
    | Bottom
    | Dropdown { className :: String, role :: String }

isModal :: Presentation -> Boolean
isModal (Dropdown _) = false
isModal _ = true

type Overlay i =
    { ref :: H.RefLabel
    , presentation :: Presentation
    , title :: String
    , onClose :: i
    }

-- The overlay with its body and, for the modal ones, a footer that may be
-- empty. A dropdown has no header and no footer; the title is its label.
overlay :: ∀ w i. Overlay i -> Array (HH.HTML w i) -> Array (HH.HTML w i) -> HH.HTML w i
overlay { ref, presentation, title, onClose } body footer =
    HH.div [ HP.ref ref, HS.class_ "overlay-layer" ]
    case presentation of
    Dropdown { className, role } ->
        [ HH.div [ HS.class_ className, HPA.role role, HPA.label title ] body ]
    _ ->
        backdrop <>
        [ HH.div
            [ HS.class_ $ "overlay" <> presentationClass
            , HPA.role "dialog"
            , HPA.modal "true"
            , HPA.labelledBy titleId
            ]
            ( [ HH.div [ HS.class_ "overlay-header" ]
                [ HH.h2 [ HP.id titleId ] [ HH.text title ]
                , iconButton "Close" onClose Icons.x
                ]
              , HH.div [ HS.class_ "overlay-body" ] body
              ]
              <> case footer of
                [] -> []
                _ -> [ HH.div [ HS.class_ "overlay-footer" ] footer ]
            )
        ]
    where
    titleId = unwrap ref <> "-title"
    backdrop = case presentation of
        FullScreen -> []
        _ -> [ HH.div [ HS.class_ "backdrop", HE.onClick $ const onClose ] [] ]
    presentationClass = case presentation of
        Modal -> " overlay-modal"
        Side -> " overlay-side"
        Bottom -> " overlay-bottom"
        _ -> ""

foreign import data UseOverlay :: HookType

instance HookNewtype UseOverlay (UseEffect <> Pure)

-- Holds the page while the overlay under the ref is open, as `hold` describes,
-- and calls onClose on Escape and, for a dropdown, on a press outside it.
-- onClose is the one given when the overlay opened, so it should close by
-- setting state rather than read it.
useOverlay :: ∀ m. MonadEffect m =>
    H.RefLabel -> Presentation -> Boolean -> HookM m Unit -> Hook m UseOverlay Unit
useOverlay ref presentation open onClose = Hooks.wrap Hooks.do
    Hooks.captures { open, modal } Hooks.useTickEffect do
        layer <- if open then Hooks.getHTMLElementRef ref else pure Nothing
        case layer of
            Nothing -> pure Nothing
            Just layer' -> do
                subscription <- Hooks.subscribe $ Subscription.makeEmitter \push ->
                    hold layer' modal (push onClose)
                pure $ Just $ Hooks.unsubscribe subscription

    Hooks.pure unit
    where
    modal = isModal presentation
