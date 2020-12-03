module TeamTavern.Client.Components.Profile where

import Prelude

import Data.Symbol (class IsSymbol, SProxy)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Prim.Row (class Cons)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Snippets.Class as HS

profileHeader :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
profileHeader = HH.div [ HS.class_ "profile-header" ]

profileHeaderItem :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
profileHeaderItem = HH.div [ HS.class_ "profile-header-item" ]

profileHeading
    :: forall action index monad slots' slots label
    .  Cons label (NavigationAnchor.Slot index) slots' slots
    => IsSymbol label
    => Ord index
    => MonadEffect monad
    => SProxy label -> index -> String -> String -> HH.ComponentHTML action slots monad
profileHeading label index path heading =
    HH.h3
    [ HS.class_ "profile-heading" ]
    [ navigationAnchorIndexed label index { path, content: HH.text heading } ]

profileSubheading :: forall slots action. String -> HH.HTML slots action
profileSubheading subheading = HH.span [ HS.class_ "profile-subheading" ] [ HH.text subheading ]
