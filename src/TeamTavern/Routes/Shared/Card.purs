module TeamTavern.Routes.Shared.Card where

import Data.Maybe (Maybe)
import Foreign.Object (Object)

-- | A post as a card shows it: a row of `Server/Feed/Feed.sql`, labelled with
-- | the query's own column names so a row reads straight into it, and of the
-- | queries that share its columns (`Server/Post/Infrastructure/CardColumns.purs`).
-- | The feed, the post page and the post screen's preview all render one.
-- |
-- | A player post takes its age, country and languages from its owner's
-- | account; `options` holds each field's chosen option keys, `ranges` a group's
-- | or community's ends of an ordered field, `flags` the booleans answered yes.
-- | `marks` holds, for each fact the viewer's description compares, `fit`,
-- | `miss` or `missing`, keyed by the game field's key or the fact's name
-- | (`location`, `languages`, `mic`, `hours`, `age`, `ages`).
type CardRow =
    { id :: Int
    , type :: String
    , name :: Maybe String
    , owner :: String
    , own :: Boolean
    , messaged :: Maybe String
    , updated :: String
    , expired :: Boolean
    , summary :: Array String
    , age :: Maybe Number
    , country :: Maybe String
    , languages :: Array String
    , regions :: Array String
    , age_from :: Maybe Int
    , age_to :: Maybe Int
    , group_size :: Maybe Int
    , group_wanted_from :: Maybe Int
    , group_wanted_to :: Maybe Int
    , timezone :: Maybe String
    , online_from :: Maybe String
    , online_to :: Maybe String
    , microphone :: Boolean
    , contact_preference :: String
    , contacts :: Array String
    , trackers :: Array { title :: String, template :: String, account :: String }
    , has_discord_server :: Boolean
    , has_website :: Boolean
    , options :: Object (Array String)
    , ranges :: Object { from :: Maybe String, to :: Maybe String }
    , flags :: Array String
    , marks :: Object String
    }
