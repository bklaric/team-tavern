module TeamTavern.Routes.Shared.Field where

type Option =
    { key :: String
    , label :: String
    }

-- | A game's field as posts answer it. `ilk` is `single`, `multi` or
-- | `boolean`; `appliesTo` names the post types that answer it; options come
-- | in the game's order, worst to best where the field is `ordered`.
type Field =
    { key :: String
    , label :: String
    , ilk :: String
    , ordered :: Boolean
    , slotted :: Boolean
    , appliesTo :: Array String
    , onCard :: Boolean
    , options :: Array Option
    }
