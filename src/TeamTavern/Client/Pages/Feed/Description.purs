module TeamTavern.Client.Pages.Feed.Description
    ( Stored
    , current
    , describes
    , emptyDescription
    , emptyStored
    , isEmpty
    , loadStored
    , saveStored
    , setCurrent
    , storeDescription
    , storedFrom
    ) where

import Prelude

import Data.Array (all, find, null, sort)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Effect (Effect)
import Foreign.Object as Object
import TeamTavern.Routes.Feed.ViewOwnDescriptions (OwnDescription)
import TeamTavern.Routes.Shared.Description (Description)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Yoga.JSON (readJSON_, writeJSON)

-- | The feed's description of each type, and the type chosen, kept in local
-- | storage per game so seeing what fits needs no account (brief 7.1). The post
-- | screen reads it when the player publishes it.
type Stored =
    { type :: String
    , player :: Description
    , group :: Description
    , community :: Description
    }

emptyDescription :: String -> Description
emptyDescription type_ =
    { type: type_
    , options: Object.empty
    , ranges: Object.empty
    , flags: []
    , country: Nothing
    , age: Nothing
    , regions: []
    , ageFrom: Nothing
    , ageTo: Nothing
    , languages: []
    , online: Nothing
    , timezone: Nothing
    , microphone: false
    }

emptyStored :: Stored
emptyStored =
    { type: "player"
    , player: emptyDescription "player"
    , group: emptyDescription "group"
    , community: emptyDescription "community"
    }

-- | The viewer's own posts as a description: their player post if they have
-- | one, otherwise their group or community post (brief 7.1). Each type starts
-- | from the viewer's post of that type.
storedFrom :: Array OwnDescription -> Maybe Stored
storedFrom own = own # find (const true) <#> \first ->
    { type: first.type
    , player: ofType "player"
    , group: ofType "group"
    , community: ofType "community"
    }
    where
    ofType type_ = own # find (_.type >>> eq type_) <#> _.description # case _ of
        Just description -> description
        Nothing -> emptyDescription type_

current :: Stored -> Description
current stored = case stored.type of
    "group" -> stored.group
    "community" -> stored.community
    _ -> stored.player

setCurrent :: Description -> Stored -> Stored
setCurrent description stored = case stored.type of
    "group" -> stored { group = description }
    "community" -> stored { community = description }
    _ -> stored { player = description }

storageKey :: String -> String
storageKey handle = "tt-description-" <> handle

loadStored :: String -> Effect (Maybe Stored)
loadStored handle =
    window >>= localStorage >>= getItem (storageKey handle) <#> (_ >>= readJSON_)

saveStored :: String -> Stored -> Effect Unit
saveStored handle stored =
    window >>= localStorage >>= setItem (storageKey handle) (writeJSON stored)

-- | Makes a description of the type the one the game's feed opens with, as
-- | See what fits does with a post's (brief 11.2).
storeDescription :: String -> String -> Description -> Effect Unit
storeDescription handle type_ description = do
    stored <- loadStored handle <#> fromMaybe emptyStored
    saveStored handle $ setCurrent description stored { type = type_ }

-- | A description that gives nothing shows every post by activity, as the
-- | feed's query decides.
isEmpty :: Description -> Boolean
isEmpty description =
    all null (Object.values description.options)
    && all (\{ from, to } -> isNothing from && isNothing to) (Object.values description.ranges)
    && null description.flags
    && isNothing description.country
    && isNothing description.age
    && null description.regions
    && isNothing description.ageFrom
    && isNothing description.ageTo
    && null description.languages
    && not description.microphone
    && not (description.online # maybe' \{ from, to } -> isJust from && isJust to)
    where
    maybe' f = case _ of
        Just value -> f value
        Nothing -> false

-- Lists compare as sets and empty values as absent, and the hours in whatever
-- timezone they were given in.
normalized :: Description -> Description
normalized description = description
    { options = description.options # Object.filter (not <<< null) <#> sort
    , ranges = description.ranges # Object.filter \{ from, to } -> isJust from || isJust to
    , flags = sort description.flags
    , regions = sort description.regions
    , languages = sort description.languages
    , online = description.online >>= \online ->
        if isNothing online.from && isNothing online.to then Nothing else Just online
    , timezone = Nothing
    }

-- | Whether the description says what the post's does.
describes :: Description -> Description -> Boolean
describes post description = normalized post == normalized description
