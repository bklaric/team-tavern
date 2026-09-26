module TeamTavern.Client.Pages.Document (adminEmail, document, link, section) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

-- A page of running text: the site's about page, contact page and policies.
-- A policy says when it last changed.
document :: ∀ w i. { title :: String, updated :: Maybe String } -> Array (HH.HTML w i) -> HH.HTML w i
document { title, updated } content =
    HH.article [ HS.class_ "document" ] $
    [ HH.h1_ [ HH.text title ] ]
    <> foldMap (\date -> [ HH.p [ HS.class_ "muted" ] [ HH.text $ "Last updated " <> date ] ]) updated
    <> content

section :: ∀ w i. String -> Array (HH.HTML w i) -> HH.HTML w i
section heading content = HH.section_ $ [ HH.h2_ [ HH.text heading ] ] <> content

link :: ∀ w m. MonadEffect m => String -> String -> HH.HTML w (m Unit)
link path text = HH.a [ HP.href path, HE.onClick $ navigateWithEvent_ path ] [ HH.text text ]

adminEmail :: ∀ w i. HH.HTML w i
adminEmail = HH.a [ HP.href "mailto:admin@teamtavern.net" ] [ HH.text "admin@teamtavern.net" ]
