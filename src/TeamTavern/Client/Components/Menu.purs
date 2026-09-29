module TeamTavern.Client.Components.Menu
    ( menuDivider
    , menuItem
    , menuItemDestructive
    , menuLabel
    , menuLink
    , sheetMenu
    ) where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

-- The rows of a menu: the body of a dropdown with the class "menu" on a
-- desktop, and of a bottom sheet, inside sheetMenu, on a phone. They are links
-- and buttons that Tab goes through, not an ARIA menu, whose arrow keys the
-- rows don't take.

-- Whose menu it is, such as the nickname heading the account menu.
menuLabel :: ∀ w i. String -> HH.HTML w i
menuLabel label = HH.p [ HS.class_ "menu-label" ] [ HH.text label ]

menuDivider :: ∀ w i. HH.HTML w i
menuDivider = HH.hr [ HS.class_ "menu-divider" ]

item :: ∀ w i. String -> i -> Array (HH.HTML w i) -> HH.HTML w i
item class_ onClick =
    HH.button [ HS.class_ class_, HP.type_ HP.ButtonButton, HE.onClick $ const onClick ]

menuItem :: ∀ w i. i -> Array (HH.HTML w i) -> HH.HTML w i
menuItem = item "menu-item"

-- A row that goes to a page of the site without reloading it.
menuLink :: ∀ w m. MonadEffect m => String -> Array (HH.HTML w (m Unit)) -> HH.HTML w (m Unit)
menuLink path =
    HH.a [ HS.class_ "menu-item", HP.href path, HE.onClick $ navigateWithEvent_ path ]

menuItemDestructive :: ∀ w i. i -> Array (HH.HTML w i) -> HH.HTML w i
menuItemDestructive = item "menu-item menu-item-destructive"

-- The same rows as touch targets, in a phone's bottom sheet.
sheetMenu :: ∀ w i. Array (HH.HTML w i) -> HH.HTML w i
sheetMenu = HH.div [ HS.class_ "sheet-menu" ]
