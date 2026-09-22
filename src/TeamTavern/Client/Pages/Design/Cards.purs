module TeamTavern.Client.Pages.Design.Cards (Fixtures, Posts, fixtures) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Record as Record
import TeamTavern.Routes.Shared.Card (CardRow)

-- Handwritten posts for the design page, the prototype's fixtures.js as rows of
-- the feed query over the seeded Valorant and Valheim fields. Every mark is
-- against one viewer: a Valorant player, a Controller at Diamond 1 in Croatia,
-- speaking English, online 19:00–23:00, and looking for ranked games.

foreign import minutesBefore :: Number -> Number -> String

type Posts r =
    ( shadowFox :: CardRow
    , nightOwls :: CardRow
    , radiantRising :: CardRow
    , afterglow :: CardRow
    , lumen :: CardRow
    , expiredPlayer :: CardRow
    , expiredGroup :: CardRow
    , farlands :: CardRow
    , valheimGroup :: CardRow
    , stress :: CardRow
    , sparse :: CardRow
    | r
    )

type Fixtures = { | Posts (ownNightOwls :: CardRow, messagedShadowFox :: CardRow) }

blank :: CardRow
blank =
    { id: 0
    , type: "player"
    , name: Nothing
    , owner: ""
    , own: false
    , messaged: Nothing
    , updated: ""
    , expired: false
    , summary: []
    , age: Nothing
    , country: Nothing
    , languages: []
    , regions: []
    , age_from: Nothing
    , age_to: Nothing
    , group_size: Nothing
    , group_wanted_from: Nothing
    , group_wanted_to: Nothing
    , timezone: Nothing
    , online_from: Nothing
    , online_to: Nothing
    , microphone: false
    , contact_preference: "offsite"
    , contacts: [ "discord" ]
    , trackers: []
    , has_discord_server: false
    , has_website: false
    , options: Object.empty
    , ranges: Object.empty
    , flags: []
    , marks: Object.empty
    }

options :: Array (Tuple String (Array String)) -> Object.Object (Array String)
options = Object.fromFoldable

range :: String -> String -> String -> Object.Object { from :: Maybe String, to :: Maybe String }
range key from to = Object.singleton key { from: Just from, to: Just to }

marks :: Array (Tuple String String) -> Object.Object String
marks = Object.fromFoldable

fixtures :: Instant -> Fixtures
fixtures now = let
    ago minutes = minutesBefore (unwrap $ unInstant now) minutes
    posts = posted ago
    in
    Record.union
        { ownNightOwls: posts.nightOwls { own = true, marks = Object.empty }
        , messagedShadowFox: posts.shadowFox { messaged = Just $ ago $ 2.0 * 1440.0 }
        }
        posts

posted :: (Number -> String) -> { | Posts () }
posted ago = let
    hours = (_ * 60.0)
    days = (_ * 1440.0)
    in
    { shadowFox: blank
        { id = 1
        , owner = "ShadowFox"
        , updated = ago $ days 2.0
        , summary = [ "Peak Immortal last act, looking for a consistent duo. Chill but I want to improve, happy to review VODs together. I play most evenings after work and I'm free all weekend." ]
        , age = Just 24.0
        , country = Just "Croatia"
        , languages = [ "English", "German" ]
        , online_from = Just "19:00"
        , online_to = Just "23:00"
        , microphone = true
        , trackers = [ { title: "tracker.gg", template: "https://tracker.gg/valorant/profile/riot/", account: "ShadowFox#EUW" } ]
        , options = options
            [ Tuple "rank" [ "diamond-2" ], Tuple "role" [ "duelist", "initiator" ]
            , Tuple "platform" [ "pc" ], Tuple "looking-for" [ "ranked" ]
            ]
        , flags = [ "in-game-leader" ]
        , marks = marks
            [ Tuple "rank" "fit", Tuple "role" "fit", Tuple "location" "fit", Tuple "languages" "fit"
            , Tuple "mic" "fit", Tuple "looking-for" "fit", Tuple "hours" "fit"
            ]
        }
    , nightOwls: blank
        { id = 2
        , type = "group"
        , name = Just "Night Owls"
        , owner = "Kestrel"
        , updated = ago $ hours 5.0
        , summary = [ "Three friends who play most nights, we want to stop solo queuing for the last two spots. No tilt, comms on, we review our losses on Sundays." ]
        , languages = [ "English" ]
        , regions = [ "Europe" ]
        , age_from = Just 18
        , group_size = Just 3
        , group_wanted_from = Just 2
        , group_wanted_to = Just 2
        , online_from = Just "21:00"
        , online_to = Just "01:00"
        , microphone = true
        , contact_preference = "message"
        , options = options [ Tuple "role" [ "controller", "sentinel" ], Tuple "looking-for" [ "ranked" ] ]
        , ranges = range "rank" "platinum-1" "diamond-3"
        , flags = [ "in-game-leader" ]
        , marks = marks
            [ Tuple "rank" "fit", Tuple "role" "fit", Tuple "location" "fit", Tuple "languages" "fit"
            , Tuple "mic" "fit", Tuple "ages" "fit", Tuple "looking-for" "fit", Tuple "hours" "fit"
            ]
        }
    , radiantRising: blank
        { id = 3
        , type = "community"
        , name = Just "Radiant Rising"
        , owner = "Mira"
        , updated = ago $ days 3.0
        , summary = [ "An EU Valorant community of about 400 players. We run in-house 10-mans every Friday, a monthly cup with small prizes, and coaching nights where our Immortal and Radiant members review your VODs. Find a duo in #lfg, join a scrim team, or just hang out in voice. New players are paired with a buddy for their first week so nobody gets lost." ]
        , languages = [ "English" ]
        , regions = [ "Europe" ]
        , age_from = Just 16
        , online_from = Just "18:00"
        , online_to = Just "01:00"
        , microphone = true
        , contact_preference = "discord"
        , contacts = []
        , has_discord_server = true
        , options = options
            [ Tuple "platform" [ "pc" ], Tuple "looking-for" [ "ranked", "scrims-tournaments", "premier" ] ]
        , marks = marks
            [ Tuple "location" "fit", Tuple "languages" "fit", Tuple "mic" "fit", Tuple "ages" "fit"
            , Tuple "looking-for" "fit"
            ]
        }
    , afterglow: blank
        { id = 4
        , type = "group"
        , name = Just "Afterglow"
        , owner = "Tomo"
        , updated = ago $ days 1.0
        , summary = [ "Four of us from the same uni, looking for a Controller who can make it three evenings a week." ]
        , languages = [ "English" ]
        , regions = [ "Europe" ]
        , group_size = Just 4
        , group_wanted_from = Just 1
        , group_wanted_to = Just 1
        , online_from = Just "20:00"
        , online_to = Just "23:00"
        , contact_preference = "either"
        , options = options [ Tuple "role" [ "controller" ], Tuple "looking-for" [ "ranked" ] ]
        , ranges = range "rank" "platinum-1" "platinum-2"
        , marks = marks
            [ Tuple "rank" "miss", Tuple "role" "fit", Tuple "location" "fit", Tuple "languages" "fit"
            , Tuple "looking-for" "fit", Tuple "hours" "fit"
            ]
        }
    , lumen: blank
        { id = 5
        , owner = "Lumen"
        , updated = ago $ days 6.0
        , summary = [ "Jogo de manhã antes das aulas, procuro alguém para jogar sem pressão." ]
        , country = Just "Portugal"
        , languages = [ "Portuguese" ]
        , online_from = Just "08:00"
        , online_to = Just "12:00"
        , options = options [ Tuple "role" [ "sentinel" ], Tuple "looking-for" [ "casual" ] ]
        , marks = marks
            [ Tuple "rank" "missing", Tuple "role" "miss", Tuple "location" "fit", Tuple "languages" "miss"
            , Tuple "mic" "miss", Tuple "looking-for" "miss", Tuple "hours" "miss"
            ]
        }
    , expiredPlayer: blank
        { id = 6
        , owner = "Quill"
        , updated = ago $ days 90.0
        , expired = true
        , summary = [ "LF non-toxic mates for comp" ]
        , country = Just "Slovenia"
        , languages = [ "English" ]
        , options = options [ Tuple "rank" [ "gold-3" ], Tuple "role" [ "sentinel" ], Tuple "looking-for" [ "ranked" ] ]
        , marks = marks
            [ Tuple "rank" "miss", Tuple "role" "fit", Tuple "location" "fit", Tuple "languages" "fit"
            , Tuple "looking-for" "fit"
            ]
        }
    , expiredGroup: blank
        { id = 7
        , type = "group"
        , owner = "Brann"
        , updated = ago $ days 365.0
        , expired = true
        , summary = [ "Two of us, chill games in the evening." ]
        , languages = [ "English", "Croatian" ]
        , regions = [ "Europe" ]
        , group_size = Just 2
        , group_wanted_from = Just 2
        , group_wanted_to = Just 3
        , contact_preference = "message"
        , options = options [ Tuple "role" [ "duelist", "initiator", "controller" ], Tuple "looking-for" [ "casual" ] ]
        , ranges = range "rank" "silver-1" "gold-3"
        , marks = marks
            [ Tuple "rank" "miss", Tuple "role" "fit", Tuple "location" "fit", Tuple "languages" "fit"
            , Tuple "looking-for" "miss"
            ]
        }
    , farlands: blank
        { id = 8
        , type = "community"
        , name = Just "The Farlands"
        , owner = "Eirik"
        , updated = ago $ days 7.0
        , summary =
            [ "Looking for a fun and friendly Valheim community server? Join The Farlands: weekly boss raids, a trading hub, building contests and 100-player events. New Vikings get a starter kit and a guide to the first biomes, and our admins are online most evenings to help."
            , ""
            , "We wipe once a year with a big send-off event, and the whole map history is kept in our Discord gallery."
            ]
        , languages = [ "English" ]
        , regions = [ "Europe" ]
        , age_from = Just 18
        , online_from = Just "17:00"
        , online_to = Just "23:00"
        , microphone = true
        , contact_preference = "discord"
        , contacts = []
        , has_discord_server = true
        , options = options
            [ Tuple "server-type" [ "modded" ], Tuple "platform" [ "pc" ]
            , Tuple "looking-for" [ "pve", "building" ], Tuple "server-characters" [ "new-characters" ]
            ]
        }
    , valheimGroup: blank
        { id = 9
        , type = "group"
        , owner = "Sigrun"
        , updated = ago $ hours 3.0
        , summary = [ "My wife and I are starting over on a new server and want a few more players. We build a lot, fight bosses together and don't rush." ]
        , languages = [ "English", "Swedish" ]
        , regions = [ "Europe" ]
        , group_size = Just 2
        , group_wanted_from = Just 2
        , group_wanted_to = Just 3
        , online_from = Just "19:00"
        , online_to = Just "23:00"
        , microphone = true
        , contact_preference = "message"
        , options = options
            [ Tuple "server-type" [ "vanilla" ], Tuple "platform" [ "pc" ], Tuple "looking-for" [ "pve", "building" ] ]
        }
    , stress: blank
        { id = 10
        , owner = "xX_NightmareOfTheEasternFront_Xx"
        , updated = ago 20.0
        , summary = [ "Ищу команду для рейтинговых игр, играю в основном вечером по Москве. Спокойный, без токсичности, микрофон есть. Могу играть на любой роли, но лучше всего на Контроллере и Инициаторе." ]
        , country = Just "Russia"
        , languages = [ "Russian", "English", "Ukrainian" ]
        , microphone = true
        , options = options
            [ Tuple "rank" [ "immortal-3" ], Tuple "role" [ "duelist", "initiator", "controller", "sentinel" ]
            , Tuple "platform" [ "pc" ], Tuple "looking-for" [ "ranked", "scrims-tournaments", "premier" ]
            ]
        }
    , sparse: blank
        { id = 11
        , owner = "Ngọc Anh"
        , updated = ago $ days 4.0
        , country = Just "Vietnam"
        , languages = [ "Vietnamese" ]
        , options = options [ Tuple "rank" [ "silver-1" ] ]
        }
    }
