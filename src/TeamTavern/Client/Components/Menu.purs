module TeamTavern.Client.Components.Menu
    ( menuDivider
    , menuItem
    , menuItemDestructive
    , menuLabel
    , sheetMenu
    ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Snippets.Class as HS

-- The rows of a menu: the body of a dropdown with the class "menu" and role
-- "menu" on a desktop, and of a bottom sheet, inside sheetMenu, on a phone.

-- Whose menu it is, such as the nickname heading the account menu.
menuLabel :: ∀ w i. String -> HH.HTML w i
menuLabel label = HH.p [ HS.class_ "menu-label" ] [ HH.text label ]

menuDivider :: ∀ w i. HH.HTML w i
menuDivider = HH.div [ HS.class_ "menu-divider" ] []

item :: ∀ w i. String -> i -> Array (HH.HTML w i) -> HH.HTML w i
item class_ onClick =
    HH.button
    [ HS.class_ class_, HP.type_ HP.ButtonButton, HPA.role "menuitem", HE.onClick $ const onClick ]

menuItem :: ∀ w i. i -> Array (HH.HTML w i) -> HH.HTML w i
menuItem = item "menu-item"

menuItemDestructive :: ∀ w i. i -> Array (HH.HTML w i) -> HH.HTML w i
menuItemDestructive = item "menu-item menu-item-destructive"

-- The same rows as touch targets, in a phone's bottom sheet.
sheetMenu :: ∀ w i. Array (HH.HTML w i) -> HH.HTML w i
sheetMenu = HH.div [ HS.class_ "sheet-menu", HPA.role "menu" ]
