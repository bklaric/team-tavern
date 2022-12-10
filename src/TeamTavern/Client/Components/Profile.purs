module TeamTavern.Client.Components.Profile where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy)

profileHeader :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
profileHeader = HH.div [ HS.class_ "profile-header" ]

profileHeading
    :: ∀ action index monad slots' slots label
    .  Cons label (NavigationAnchor.Slot index) slots' slots
    => IsSymbol label
    => Ord index
    => MonadEffect monad
    => Proxy label -> index -> String -> String -> HH.ComponentHTML action slots monad
profileHeading label index path heading =
    HH.h3
    [ HS.class_ "profile-heading" ]
    [ navigationAnchorIndexed label index { path, content: HH.text heading } ]

profileHeading'
    :: ∀ action monad slots' slots label
    .  Cons label (NavigationAnchor.Slot String) slots' slots
    => IsSymbol label
    => MonadEffect monad
    => Proxy label -> String -> String -> String -> HH.ComponentHTML action slots monad
profileHeading' label handle path heading =
    HH.h3
    [ HS.class_ "profile-heading" ]
    [ navigationAnchorIndexed label handle
        { path
        , content: HH.span_
            [ HH.img
                [ HS.class_ "profile-heading-icon"
                , HP.src $ "/images/" <> handle <> "/icon-orange.png"
                , HP.alt $ heading <> " logo"
                ]
            , HH.text heading
            ]
        }
    ]

profileSubheading :: ∀ slots action. String -> HH.HTML slots action
profileSubheading subheading = HH.span [ HS.class_ "profile-subheading" ] [ HH.text subheading ]
