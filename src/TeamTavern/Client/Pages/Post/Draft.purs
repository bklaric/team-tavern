module TeamTavern.Client.Pages.Post.Draft
    ( Draft
    , clearDraft
    , emptyDraft
    , fromContent
    , fromDescription
    , gameContacts
    , loadDraft
    , saveDraft
    , toCard
    , toRequest
    , withAccount
    ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (elem, filter, mapMaybe, null, sort)
import Data.Date (Date, day, month, year)
import Data.Enum (fromEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Description (Description, Hours, Range)
import TeamTavern.Routes.Shared.Post (AccountContent, PostContent, RequestContent)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Yoga.JSON (readJSON_, writeJSON)

-- | What the post screen holds while it is written (brief 6, step 3): the
-- | post's fields and the account's facts and contacts, which publishing
-- | writes to the account. `editing` is whether it will replace the player's
-- | post rather than publish a new one; `signedOut` whether it was last
-- | written signed out, so signing in checks it against the account's post.
-- | `languages` are the account's on a player post, the post's own on a group
-- | or community.
type Draft =
    { editing :: Boolean
    , signedOut :: Boolean
    , options :: Object (Array String)
    , ranges :: Object Range
    , flags :: Array String
    , name :: String
    , size :: Int
    , wantedFrom :: Int
    , wantedTo :: Int
    , regions :: Array String
    , languages :: Array String
    , country :: Maybe String
    , birthday :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , online :: Hours
    , timezone :: Maybe String
    , microphone :: Boolean
    , text :: String
    , reach :: String
    , contacts :: Object String
    , discordServer :: String
    , website :: String
    }

-- | A new post: reached by message, a community joined on Discord, and a
-- | group of two wanting one more.
emptyDraft :: String -> Draft
emptyDraft type_ =
    { editing: false
    , signedOut: false
    , options: Object.empty
    , ranges: Object.empty
    , flags: []
    , name: ""
    , size: 2
    , wantedFrom: 1
    , wantedTo: 1
    , regions: []
    , languages: []
    , country: Nothing
    , birthday: Nothing
    , ageFrom: Nothing
    , ageTo: Nothing
    , online: { from: Nothing, to: Nothing }
    , timezone: Nothing
    , microphone: false
    , text: ""
    , reach: if type_ == "community" then "discord" else "message"
    , contacts: Object.empty
    , discordServer: ""
    , website: ""
    }

-- One draft per game and type, keyed as the feed keeps its description.
storageKey :: String -> String -> String
storageKey handle type_ = "tt-draft-" <> handle <> "-" <> type_

loadDraft :: String -> String -> Effect (Maybe Draft)
loadDraft handle type_ = window >>= localStorage >>= getItem (storageKey handle type_) <#> (_ >>= readJSON_)

saveDraft :: String -> String -> Draft -> Effect Unit
saveDraft handle type_ draft = window >>= localStorage >>= setItem (storageKey handle type_) (writeJSON draft)

clearDraft :: String -> String -> Effect Unit
clearDraft handle type_ = window >>= localStorage >>= removeItem (storageKey handle type_)

-- | The player's post as the screen edits it.
fromContent :: String -> PostContent -> Draft
fromContent type_ post = (emptyDraft type_)
    { editing = true
    , options = post.options
    , ranges = post.ranges
    , flags = post.flags
    , name = fromMaybe "" post.name
    , size = fromMaybe 2 post.groupSize
    , wantedFrom = fromMaybe 1 post.groupWantedFrom
    , wantedTo = fromMaybe 1 post.groupWantedTo
    , regions = post.regions
    , languages = post.languages
    , ageFrom = post.ageFrom
    , ageTo = post.ageTo
    , online = { from: post.online <#> _.from, to: post.online <#> _.to }
    , microphone = post.microphone
    , text = post.summary
    , reach = post.contactPreference
    , discordServer = fromMaybe "" post.discordServer
    , website = fromMaybe "" post.website
    }

-- | The feed's description laid over the draft, as Publish post and Update
-- | post open the screen (brief 7.1). An age isn't a birthday, so it stays
-- | behind. Updating a post takes the description as it is, fields cleared
-- | from it included; a new post keeps what the draft holds where the
-- | description gives nothing.
fromDescription :: Description -> Draft -> Draft
fromDescription description draft = draft
    { options = if clearing then description.options else Object.union description.options draft.options
    , ranges = if clearing then description.ranges else Object.union description.ranges draft.ranges
    , flags = list description.flags draft.flags
    , country = description.country <|> kept draft.country
    , regions = list description.regions draft.regions
    , ageFrom = description.ageFrom <|> kept draft.ageFrom
    , ageTo = description.ageTo <|> kept draft.ageTo
    , languages = list description.languages draft.languages
    , online = if hoursGiven then hours else if clearing then { from: Nothing, to: Nothing } else draft.online
    , timezone = if hoursGiven then description.timezone <|> draft.timezone else draft.timezone
    , microphone = description.microphone || not clearing && draft.microphone
    }
    where
    clearing = draft.editing
    hours = fromMaybe { from: Nothing, to: Nothing } description.online
    hoursGiven = isJust hours.from || isJust hours.to
    kept :: ∀ a. Maybe a -> Maybe a
    kept value = if clearing then Nothing else value
    list described drafted = if clearing || not null described then described else drafted

-- | What the account holds fills in what the draft doesn't have: its facts and
-- | contacts, and for a group or a community the languages and the region of
-- | the owner's country (brief 6, step 3).
withAccount :: String -> (String -> Maybe String) -> AccountContent -> Draft -> Draft
withAccount type_ regionOf account draft = draft
    { country = draft.country <|> (if player then account.country else Nothing)
    , birthday = draft.birthday <|> (if player then account.birthday else Nothing)
    , languages = if null draft.languages then account.languages else draft.languages
    , regions =
        if not player && null draft.regions
        then maybe [] pure (account.country >>= regionOf)
        else draft.regions
    , timezone = draft.timezone <|> account.timezone
    , contacts = Object.union (draft.contacts # Object.filter (isJust <<< blank)) account.contacts
    }
    where
    player = type_ == "player"

blank :: String -> Maybe String
blank value = if trim value == "" then Nothing else Just $ trim value

-- | The game's contacts the draft gives, which a community's post doesn't
-- | show.
gameContacts :: ViewGame.OkContent -> String -> Draft -> Object String
gameContacts game type_ draft =
    if type_ == "community" then Object.empty
    else draft.contacts # Object.filterWithKey \kind value -> elem kind game.contacts && isJust (blank value)

bothHours :: Hours -> Maybe { from :: String, to :: String }
bothHours { from, to } = { from: _, to: _ } <$> from <*> to

-- | The request publishing the draft sends. The account facts go with it where
-- | the post type shows them, and the timezone always, since the hours are in
-- | it.
toRequest :: ViewGame.OkContent -> String -> String -> Draft -> RequestContent
toRequest game type_ timezone draft =
    { post:
        { options: draft.options
        , ranges: draft.ranges
        , flags: draft.flags
        , name: blank draft.name
        , groupSize: Just draft.size
        , groupWantedFrom: Just draft.wantedFrom
        , groupWantedTo: Just draft.wantedTo
        , regions: draft.regions
        , languages: if player then [] else draft.languages
        , ageFrom: draft.ageFrom
        , ageTo: draft.ageTo
        , online: bothHours draft.online
        , microphone: draft.microphone
        , summary: draft.text
        , contactPreference: draft.reach
        , discordServer: blank draft.discordServer
        , website: blank draft.website
        }
    , account:
        { country: if player then draft.country else Nothing
        , languages: if player then draft.languages else []
        , birthday: if player then draft.birthday else Nothing
        , timezone: Just timezone
        , contacts: gameContacts game type_ draft <#> trim
        }
    }
    where
    player = type_ == "player"

-- Whole years from a birthday to a day.
ageOn :: Date -> String -> Maybe Number
ageOn today birthday = case split (Pattern "-") birthday <#> Int.fromString of
    [ Just year', Just month', Just day' ] -> let
        years = fromEnum (year today) - year'
        before = fromEnum (month today) < month' || fromEnum (month today) == month' && fromEnum (day today) < day'
        in Just $ Int.toNumber if before then years - 1 else years
    _ -> Nothing

-- | The card the draft makes, as the feed would show it. Its hours are as
-- | written, which is how its owner sees them. Until the player has an
-- | account, the card is named after "you".
toCard :: ViewGame.OkContent -> String -> { nickname :: Maybe String, updated :: String, today :: Date } -> Draft -> CardRow
toCard game type_ { nickname, updated, today } draft =
    { id: 0
    , type: type_
    , name: case blank draft.name of
        Just name | not player -> Just name
        _ | player || isJust nickname -> Nothing
        _ -> Just if type_ == "group" then "Your group" else "Your community"
    , owner: fromMaybe "You" nickname
    , own: false
    , messaged: Nothing
    , updated
    , expired: false
    , summary: draft.text # split (Pattern "\n\n") <#> trim # filter (_ /= "")
    , age: if player then draft.birthday >>= ageOn today else Nothing
    , country: if player then draft.country else Nothing
    , languages: draft.languages
    , regions: if player then [] else draft.regions
    , age_from: if player then Nothing else draft.ageFrom
    , age_to: if player then Nothing else draft.ageTo
    , group_size: if type_ == "group" then Just draft.size else Nothing
    , group_wanted_from: if type_ == "group" then Just draft.wantedFrom else Nothing
    , group_wanted_to: if type_ == "group" then Just draft.wantedTo else Nothing
    , timezone: Nothing
    , online_from: bothHours draft.online <#> _.from
    , online_to: bothHours draft.online <#> _.to
    , microphone: draft.microphone
    , contact_preference: draft.reach
    , contacts: sort $ Object.keys contacts
    , trackers:
        if not player then []
        else game.trackers # mapMaybe \{ contact, title, template } ->
            Object.lookup contact contacts <#> \account -> { title, template, account: trim account }
    , has_discord_server: not player && isJust (blank draft.discordServer)
    , has_website: not player && isJust (blank draft.website)
    , options: draft.options # Object.filter (not <<< null)
    , ranges: draft.ranges
    , flags: draft.flags
    , marks: Object.empty
    }
    where
    player = type_ == "player"
    contacts = gameContacts game type_ draft
