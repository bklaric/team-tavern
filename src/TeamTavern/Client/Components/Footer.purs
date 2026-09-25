module TeamTavern.Client.Components.Footer (footer) where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

-- The site's own pages, linked from every page so that search finds them.
footer :: ∀ w m. MonadEffect m => HH.HTML w (m Unit)
footer =
    HH.footer [ HS.class_ "site-footer" ]
    [ HH.div [ HS.class_ "site-footer-inner" ]
        [ HH.span_ [ HH.text "TeamTavern" ]
        , HH.nav [ HS.class_ "site-footer-links", HPA.label "Site" ]
            [ link "/privacy" "Privacy" ]
        ]
    ]
    where
    link path text = HH.a [ HP.href path, HE.onClick $ navigateWithEvent_ path ] [ HH.text text ]
