module TeamTavern.Client.Pages.Document (adminEmail, document, link, longDate, section) where

import Prelude

import Data.Array ((!!))
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

-- A page of running text: the site's about page, contact page, policies and
-- guides. A policy or a guide says when it last changed.
document :: ∀ w i. { title :: String, updated :: Maybe String } -> Array (HH.HTML w i) -> HH.HTML w i
document { title, updated } content =
    HH.article [ HS.class_ "document" ] $
    [ HH.h1_ [ HH.text title ] ]
    <> foldMap (\date -> [ HH.p [ HS.class_ "muted" ] [ HH.text $ "Last updated " <> date ] ]) updated
    <> content

-- | An ISO date as a document gives it: 2026-09-28 is 28 September 2026.
longDate :: String -> String
longDate iso = case split (Pattern "-") iso of
    [ year, month, day ]
        | Just month' <- Int.fromString month >>= \number -> months !! (number - 1)
        , Just day' <- Int.fromString day ->
            show day' <> " " <> month' <> " " <> year
    _ -> iso
    where
    months =
        [ "January", "February", "March", "April", "May", "June"
        , "July", "August", "September", "October", "November", "December"
        ]

section :: ∀ w i. String -> Array (HH.HTML w i) -> HH.HTML w i
section heading content = HH.section_ $ [ HH.h2_ [ HH.text heading ] ] <> content

link :: ∀ w m. MonadEffect m => String -> String -> HH.HTML w (m Unit)
link path text = HH.a [ HP.href path, HE.onClick $ navigateWithEvent_ path ] [ HH.text text ]

adminEmail :: ∀ w i. HH.HTML w i
adminEmail = HH.a [ HP.href "mailto:admin@teamtavern.net" ] [ HH.text "admin@teamtavern.net" ]
