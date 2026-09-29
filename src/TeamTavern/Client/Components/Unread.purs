module TeamTavern.Client.Components.Unread (badge, unreadDot) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Snippets.Class as HS

-- The count on an icon button. It is hidden from screen readers, which read it
-- from the button's label instead, "Messages, 2 unread".
badge :: ∀ w i. Int -> HH.HTML w i
badge count = HH.span [ HS.class_ "badge", HPA.hidden "true" ] [ HH.text $ show count ]

-- The dot on an unread row.
unreadDot :: ∀ w i. Array (HH.HTML w i)
unreadDot =
    [ HH.span [ HS.class_ "unread-dot" ] []
    , HH.span [ HS.class_ "visually-hidden" ] [ HH.text "Unread" ]
    ]
