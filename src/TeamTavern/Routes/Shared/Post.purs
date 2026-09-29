module TeamTavern.Routes.Shared.Post where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Foreign.Object (Object)
import TeamTavern.Routes.Shared.Description (Range)

-- | A post as its owner writes it on the post screen (brief 6, step 3).
-- | `options`, `ranges` and `flags` answer the game's fields by their keys, as
-- | a description does. `languages` are a group's or a community's own; a
-- | player post shows the account's. The hours are in the account's timezone.
type PostContent =
    { options :: Object (Array String)
    , ranges :: Object Range
    , flags :: Array String
    , name :: Maybe String
    , groupSize :: Maybe Int
    , groupWantedFrom :: Maybe Int
    , groupWantedTo :: Maybe Int
    , regions :: Array String
    , languages :: Array String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , online :: Maybe { from :: String, to :: String }
    , microphone :: Boolean
    , summary :: String
    , contactPreference :: String
    , discordServer :: Maybe String
    , website :: Maybe String
    }

-- | The facts about the player and the contacts every post of theirs shows,
-- | which live on the account. `contacts` are keyed by `game_contact` kind.
type AccountContent =
    { country :: Maybe String
    , languages :: Array String
    , birthday :: Maybe String
    , timezone :: Maybe String
    , contacts :: Object String
    }

type RequestContent =
    { post :: PostContent
    , account :: AccountContent
    }

-- | What the post screen names beside a field. `field` is a value only a
-- | request the screen didn't make can hold, named by its key.
type PostError = Variant
    ( name :: {}
    , summary :: {}
    , hours :: {}
    , reach :: {}
    , discordServer :: {}
    , website :: {}
    , contact :: { kind :: String }
    , field :: { key :: String }
    )

-- | `exists` is a post of the type the player already has in the game.
type BadContent = Variant
    ( post :: NonEmptyArray PostError
    , exists :: {}
    )
