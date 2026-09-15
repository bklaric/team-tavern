module TeamTavern.Client.Components.GameCover (gameCover) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

-- The 600x900 cover every game has under Static/Images/Games. The parent sets
-- the width; the intrinsic size keeps the box shaped before the file arrives.
gameCover :: ∀ slots action rest.
    { handle :: String, title :: String | rest } -> HH.HTML slots action
gameCover { handle, title } =
    HH.img
    [ HS.class_ "game-cover"
    , HP.src $ "/images/games/" <> handle <> ".webp"
    , HP.alt $ title <> " cover"
    , HP.width 600
    , HP.height 900
    , HP.attr (HH.AttrName "loading") "lazy"
    ]
