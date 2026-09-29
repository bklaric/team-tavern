module TeamTavern.Client.Components.TypeCards (typeCards) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS

-- The three post types in the words players use, each with a short example
-- (brief 6, step 1).
types :: ∀ w i. Array { type :: String, icon :: HH.HTML w i, choice :: String, example :: String }
types =
    [ { type: "player", icon: Icons.user, choice: "I'm a player looking for a group"
      , example: "Groups, communities and other players find you" }
    , { type: "group", icon: Icons.users, choice: "We're a group looking for players"
      , example: "“Three of us play most nights, need a fifth”" }
    , { type: "community", icon: Icons.castle, choice: "We're a community looking for members"
      , example: "“Our server runs weekly events, all welcome”" }
    ]

-- | A link card per type, leading where `href` says for it, with the line
-- | `note` gives it under the example.
typeCards :: ∀ w m. MonadEffect m =>
    { href :: String -> String, note :: String -> Maybe String } -> HH.HTML w (m Unit)
typeCards { href, note } =
    HH.div [ HS.class_ "type-cards" ] $ types <#> \type_ -> let
        path = href type_.type
        in
        HH.a [ HS.class_ "type-card", HP.href path, HE.onClick $ navigateWithEvent_ path ]
        [ type_.icon
        , HH.span [ HS.class_ "type-card-text" ] $
            [ HH.span [ HS.class_ "type-card-title" ] [ HH.text type_.choice ]
            , HH.span [ HS.class_ "type-card-example" ] [ HH.text type_.example ]
            ]
            <> maybe [] (\mine -> [ HH.span [ HS.class_ "type-card-mine" ] [ HH.text mine ] ]) (note type_.type)
        , Icons.chevronRight
        ]
