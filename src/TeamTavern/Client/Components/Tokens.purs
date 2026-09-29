module TeamTavern.Client.Components.Tokens (Input, Slot, tokens) where

import Prelude

import Data.Array (filter, find, notElem, null, snoc)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (iconButton)
import TeamTavern.Client.Components.Field (labelId)
import TeamTavern.Client.Components.Input (Option, select)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Shared.Slot (Slot_OI)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLElement (focus)

-- `one` names a single option in the add select, "Add a language".
type Input = { id :: String, one :: String, options :: Array Option, chosen :: Array String }

type Slot = Slot_OI (Array String) String

addRef :: H.RefLabel
addRef = H.RefLabel "add"

-- A chosen few out of a long list, each with its Remove, and a select adding
-- another. Removing one gives the focus to the select, since the button that
-- had it is gone.
component :: ∀ query m. MonadEffect m => H.Component query Input (Array String) m
component = Hooks.component \{ outputToken } { id, one, options, chosen } -> Hooks.do
    let labelOf value = options # find (_.value >>> eq value) # maybe value _.label
        remove value = do
            Hooks.getHTMLElementRef addRef >>= traverse_ (focus >>> liftEffect)
            Hooks.raise outputToken $ filter (notEq value) chosen
        add value = when (value /= "") $ Hooks.raise outputToken $ snoc chosen value
        token value =
            HH.span [ HS.class_ "token" ]
            [ HH.text $ labelOf value
            , iconButton ("Remove " <> labelOf value) (remove value) Icons.x
            ]

    Hooks.pure $
        HH.div [ HS.class_ "tokens" ] $
        (chosen <#> token)
        <> [ select [ HP.id id, HP.ref addRef, HPA.labelledBy $ labelId id ]
                { options: options # filter (_.value >>> flip notElem chosen)
                , value: ""
                , placeholder: Just if null chosen then "Add a " <> one else "Add another"
                , onChange: add
                }
           ]

tokens :: ∀ action slots m. MonadEffect m =>
    Input -> (Array String -> action) -> HH.ComponentHTML action (tokens :: Slot | slots) m
tokens input handler = HH.slot (Proxy :: _ "tokens") input.id component input handler
