module TeamTavern.Client.Components.Card (Viewer, card, flagText, tierOf) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes, elem, filter, find, findIndex, head, index, length, mapMaybe, null)
import Data.DateTime.Instant (Instant)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (Pattern(..), joinWith, split, toLower, trim)
import Data.String.CodeUnits as CodeUnits
import Effect.Class (class MonadEffect)
import Foreign.Object as Object
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import JSURI (encodeURIComponent)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Card.Hours (Hours, hoursText, inViewerTime)
import TeamTavern.Client.Components.Card.Regions (regionsText)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Ago (ago)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Field (Field)
import TeamTavern.Shared.Languages (languageCode)
import Web.UIEvent.MouseEvent (MouseEvent)

-- | Whose clock and calendar the card reads times by.
type Viewer = { now :: Instant, timezone :: String }

data Match = Fit | Miss

data FactIcon = Mic | MicOff

-- A fact is words, or an icon with a label for screen readers, and carries a
-- mark where the viewer's description compares it.
data Fact
    = TextFact { text :: String, tabular :: Boolean, match :: Maybe Match }
    | IconFact { icon :: FactIcon, label :: String, match :: Maybe Match }

type Detail = { label :: String, value :: String }

-- A mark the card shows. A field the post left empty is marked missing, and
-- shows as not given in the field's place rather than as a mark.
shownMatch :: String -> Maybe Match
shownMatch "fit" = Just Fit
shownMatch "miss" = Just Miss
shownMatch _ = Nothing

withMatch :: Maybe Match -> Fact -> Fact
withMatch match (TextFact fact) = TextFact fact { match = match }
withMatch match (IconFact fact) = IconFact fact { match = match }

plain :: String -> Fact
plain text = TextFact { text, tabular: false, match: Nothing }

missed :: String -> Fact
missed text = TextFact { text, tabular: false, match: Just Miss }

-- | How a field reads: a player's yes is the field itself, a group's or a
-- | community's is what it needs (brief 5).
flagText :: String -> Field -> Boolean -> String
flagText type_ field yes = let
    label = toLower field.label
    article = if elem (CodeUnits.take 1 label) [ "a", "e", "i", "o", "u" ] then "an" else "a"
    thing = article <> " " <> label
    in
    if type_ == "player"
    then if yes then field.label else "Not " <> thing
    else if yes then "Needs " <> thing else "Doesn't need " <> thing

stepOf :: Field -> String -> Maybe Int
stepOf field key = findIndex (\option -> option.key == key) field.options

optionLabel :: Field -> String -> String
optionLabel field key = find (\option -> option.key == key) field.options <#> _.label # fromMaybe key

stepLabel :: Field -> Int -> String
stepLabel field step = index field.options step <#> _.label # fromMaybe ""

rangeText :: Field -> { from :: Int, to :: Int } -> String
rangeText field { from, to }
    | from == to = stepLabel field from
    | from == 0 = "Up to " <> stepLabel field to
    | to == length field.options - 1 = stepLabel field from <> " and up"
    | otherwise = stepLabel field from <> " – " <> stepLabel field to

-- A post's answer to a field as the card says it. A no to a boolean is no
-- answer to show. A player gives a point on an ordered field, a group or a
-- community a range, either end of which may be open. A slotted field with
-- every option picked plays anything.
answerText :: CardRow -> Field -> Maybe String
answerText post field = let
    chosen = Object.lookup field.key post.options # fromMaybe []
    in
    if field.ilk == "boolean" then
        if elem field.key post.flags then Just $ flagText post.type field true else Nothing
    else if field.ordered && post.type == "player" then
        head chosen >>= stepOf field <#> stepLabel field
    else if field.ordered then do
        { from, to } <- Object.lookup field.key post.ranges
        if isNothing from && isNothing to then Nothing else do
            let from' = from >>= stepOf field # fromMaybe 0
                to' = to >>= stepOf field # fromMaybe (length field.options - 1)
            pure $ rangeText field { from: min from' to', to: max from' to' }
    else if null chosen then Nothing
    else if field.slotted && length chosen == length field.options then Just $ "Any " <> toLower field.label
    else Just $ chosen <#> optionLabel field # joinWith ", "

agesText :: Maybe Int -> Maybe Int -> Maybe String
agesText (Just from) (Just to) = Just $ "Ages " <> show from <> "–" <> show to
agesText (Just from) Nothing = Just $ "Ages " <> show from <> "+"
agesText Nothing (Just to) = Just $ "Ages up to " <> show to
agesText Nothing Nothing = Nothing

allDayOr :: String -> Hours -> String
allDayOr allDay hours = if hours.from == hours.to then allDay else hoursText hours

-- The fact line. Every fact has its place in it: the game's fields that lead
-- the card, in the game's order, then location or regions, languages,
-- microphone and a group's or community's ages. The game's other fields, online
-- hours and a player's age wait behind Details, and join the end of the line
-- only while compared. A field the viewer filled in and the post left empty
-- shows in its place as not given. The post's type says whether the facts say
-- what the post is or what it is looking for, so nothing in the line marks it.
factsOf :: ViewGame.OkContent -> CardRow -> Maybe Hours -> Array Fact
factsOf game post hours = let
    player = post.type == "player"
    markOf key = Object.lookup key post.marks
    compared key = isJust $ markOf key
    slot key name given = case given of
        Just fact -> [ withMatch (markOf key >>= shownMatch) fact ]
        Nothing | markOf key == Just "missing" -> [ missed $ name <> " not given" ]
        Nothing -> []
    -- Overwatch ranks each role on a ladder of its own, so where a card leads
    -- with more than one ladder each names itself.
    laddersNamed = length (filter (\field -> field.ordered && field.onCard) game.fields) > 1
    gameFact shown field = let
        text = answerText post field
        named = if laddersNamed && field.ordered then text <#> \text' -> field.label <> " " <> text' else text
        in
        if field.ilk == "boolean" && isNothing text && markOf field.key == Just "miss"
        then [ missed $ flagText post.type field false ]
        else slot field.key field.label (if shown field then named <#> plain else Nothing)
    fields = filter (\field -> elem post.type field.appliesTo) game.fields
    microphone
        | post.microphone = [ IconFact
            { icon: Mic
            , label: if player then "Microphone" else "Microphone required"
            , match: markOf "mic" >>= shownMatch
            } ]
        | markOf "mic" == Just "miss" = [ IconFact { icon: MicOff, label: "No microphone", match: Just Miss } ]
        | otherwise = []
    in
    (filter _.onCard fields >>= gameFact (const true))
    <> (if player
        then slot "location" "Location" (post.country <#> plain)
        else slot "location" "Regions" (if null post.regions then Nothing else Just $ plain $ regionsText post.regions))
    <> slot "languages" "Languages"
        (if null post.languages then Nothing else Just $ plain $ joinWith ", " $ languageCode <$> post.languages)
    <> microphone
    <> (if player then [] else slot "ages" "Ages" (agesText post.age_from post.age_to <#> plain))
    <> (filter (not _.onCard) fields >>= gameFact (compared <<< _.key))
    <> slot "hours" "Online hours" (if compared "hours"
        then hours <#> \hours' -> TextFact { text: allDayOr "Online all day" hours', tabular: true, match: Nothing }
        else Nothing)
    <> (if player
        then slot "age" "Age" (if compared "age" then post.age <#> \age -> plain $ "Age " <> show (floor age) else Nothing)
        else [])

detailsOf :: ViewGame.OkContent -> CardRow -> Maybe Hours -> Array Detail
detailsOf game post hours = let
    player = post.type == "player"
    in
    catMaybes
    [ if player then post.age <#> \age -> { label: "Age", value: show $ floor age } else Nothing
    , hours <#> \hours' -> { label: "Usually online", value: allDayOr "All day" hours' }
    ]
    <> (game.fields # filter (\field -> elem post.type field.appliesTo && not field.onCard) # mapMaybe \field ->
        answerText post field <#> \text ->
            { label: field.label
            , value: if field.ilk == "boolean" then (if player then "Yes" else "Needed") else text
            })

renderMatch :: ∀ w i. Maybe Match -> Array (HH.HTML w i)
renderMatch (Just Fit) = [ Icons.fitMark, HH.span [ HS.class_ "visually-hidden" ] [ HH.text "Fits:" ] ]
renderMatch (Just Miss) = [ Icons.missMark, HH.span [ HS.class_ "visually-hidden" ] [ HH.text "Doesn't fit:" ] ]
renderMatch Nothing = []

matchClass :: Maybe Match -> String
matchClass (Just Fit) = "fact fact-fit"
matchClass (Just Miss) = "fact fact-miss"
matchClass Nothing = "fact"

renderFact :: ∀ w i. Fact -> HH.HTML w i
renderFact (TextFact { text, tabular, match }) =
    HH.span [ HS.class_ $ matchClass match ] $ renderMatch match
    <> [ if tabular then HH.span [ HS.class_ "tabular" ] [ HH.text text ] else HH.span_ [ HH.text text ] ]
renderFact (IconFact { icon, label, match }) =
    HH.span [ HS.class_ $ matchClass match ] $ renderMatch match
    <> [ case icon of
            Mic -> Icons.mic
            MicOff -> Icons.micOff
       , HH.span [ HS.class_ "visually-hidden" ] [ HH.text label ]
       ]

typeLabel :: ∀ w i. String -> Array (HH.HTML w i)
typeLabel "group" = [ Icons.users, HH.text "Group" ]
typeLabel "community" = [ Icons.castle, HH.text "Community" ]
typeLabel _ = [ Icons.user, HH.text "Player" ]

-- A group's size: how many it is and how many more it wants, "3 players, wants
-- 2 more", or a range where either will do (brief 5.2). A group that says only
-- how many more it wants opens with that.
slotsText :: CardRow -> Maybe String
slotsText post = let
    size = post.group_size <#> \count -> show count <> " player" <> if count == 1 then "" else "s"
    wanted = case post.group_wanted_from, post.group_wanted_to of
        Just from, Just to | from /= to -> Just $ show from <> "–" <> show to
        from, to -> show <$> (from <|> to)
    wants = wanted <#> \count -> (if isJust size then "wants " else "Wants ") <> count <> " more"
    in
    case catMaybes [ size, wants ] of
    [] -> Nothing
    parts -> Just $ joinWith ", " parts

-- What the contact button says: how the post asked to be reached (brief 5.5).
contactButton :: ∀ w i. CardRow -> Array (HH.HTML w i)
contactButton post = case post.contact_preference of
    "discord" -> [ Icons.discord, HH.text "Join Discord" ]
    "website" -> [ Icons.externalLink, HH.text "Visit site" ]
    "offsite" | elem "discord" post.contacts -> [ Icons.discord, HH.text "Add on Discord" ]
    "offsite" -> [ Icons.gamepad2, HH.text "Add in game" ]
    "either" -> [ Icons.messageCircle, HH.text "Contact" ]
    _ -> [ Icons.messageCircle, HH.text "Message" ]

-- | A post as a card (brief 5). `marked` shows the facts' marks; without it the
-- | card reads as it does with an empty description, marks or not. A preview is the post
-- | screen's: its owner is "you" and it has no actions. The viewer's own post
-- | offers Edit and Renew in place of the contact button, and a post the viewer
-- | has written about opens that conversation (brief 5.6).
card :: ∀ w m. MonadEffect m =>
    { game :: ViewGame.OkContent
    , viewer :: Viewer
    , post :: CardRow
    , marked :: Boolean
    , expanded :: Boolean
    , preview :: Boolean
    , onToggle :: MouseEvent -> m Unit
    , onContact :: m Unit
    , onEdit :: m Unit
    , onRenew :: m Unit
    }
    -> HH.HTML w (m Unit)
card { game, viewer, post, marked, expanded, preview, onToggle, onContact, onEdit, onRenew } = let
    hours = do
        from <- post.online_from
        to <- post.online_to
        inViewerTime viewer post.timezone from to
    facts = factsOf game (if marked then post else post { marks = Object.empty }) hours
    details = detailsOf game post hours
    text = post.summary # joinWith "\n" # trim
    long = CodeUnits.length text > (if post.type == "community" then 360 else 170)
        || length (split (Pattern "\n") text) > 2
    expandable = not null details || not null post.trackers || long
    name = if post.type == "player" then post.owner else fromMaybe (post.owner <> "'s " <> post.type) post.name
    href = "/games/" <> game.handle <> "/posts/" <> show post.id
    classes = joinWith " " $ catMaybes
        [ Just "card"
        , Just $ "card-" <> post.type
        , if post.expired then Just "card-expired" else Nothing
        , if expanded then Just "card-expanded" else Nothing
        ]
    heading = HH.div [ HS.class_ "card-heading" ] $ catMaybes
        -- A draft on the post screen has no page yet, nor an id.
        [ Just if post.id == 0
            then HH.span [ HS.class_ "card-name" ] [ HH.text name ]
            else HH.a [ HS.class_ "card-name", HP.href href, HE.onClick $ navigateWithEvent_ href ] [ HH.text name ]
        , Just $ HH.span [ HS.class_ "card-type" ] $ typeLabel post.type
        , slotsText post <#> \slots -> HH.span [ HS.class_ "card-slots tabular" ] [ HH.text slots ]
        , if post.own then Just $ HH.span [ HS.class_ "card-own" ] [ HH.text "Your post" ] else Nothing
        , Just $ HH.span [ HS.class_ "card-freshness" ] [ HH.text $ "Active " <> ago viewer.now post.updated ]
        ]
    detailRow { label, value } =
        HH.div [ HS.class_ "detail" ] [ HH.span [ HS.class_ "detail-label" ] [ HH.text label ], HH.span_ [ HH.text value ] ]
    trackerRow { title, template, account } =
        HH.div [ HS.class_ "detail" ]
        [ HH.span [ HS.class_ "detail-label" ] [ HH.text "Tracker" ]
        , HH.a
            [ HP.href $ template <> fromMaybe account (encodeURIComponent account)
            , HP.target "_blank"
            , HP.rel "noopener"
            ]
            [ HH.text $ title <> " ", Icons.externalLink ]
        ]
    detailRows = (detailRow <$> details) <> (trackerRow <$> post.trackers)
    ownerLine
        | post.type == "player" = Nothing
        | otherwise = Just $ HH.span [ HS.class_ "card-owner" ]
            [ HH.text $ (if post.type == "community" then "Run by " else "Posted by ")
                <> if preview then "you" else post.owner ]
    messagedLine = post.messaged <#> \time ->
        HH.span [ HS.class_ "card-messaged" ] [ Icons.messageCircle, HH.text $ "You messaged " <> ago viewer.now time ]
    -- A card's contact button is outlined, so a feed of twenty cards doesn't
    -- show twenty filled ones.
    contact content =
        HH.button
        [ HS.class_ "button button-outline button-small card-contact"
        , HP.type_ HP.ButtonButton
        , HE.onClick $ const onContact
        ]
        content
    actions
        | preview = []
        | post.own =
            [ button Outline Small onEdit [ Icons.pencil, HH.text "Edit" ]
            , button Outline Small onRenew [ Icons.refreshCw, HH.text "Renew" ]
            ]
        | isJust post.messaged = [ contact [ Icons.messageCircle, HH.text "Open conversation" ] ]
        | otherwise = [ contact $ contactButton post ]
    toggle =
        HH.button
        [ HS.class_ "button button-text button-small card-details-toggle"
        , HP.type_ HP.ButtonButton
        , HPA.expanded $ show expanded
        , HE.onClick onToggle
        ]
        [ HH.text "Details", Icons.chevronDown ]
    footer = HH.div [ HS.class_ "card-footer" ] $
        [ HH.div [ HS.class_ "card-meta" ] $ catMaybes [ ownerLine, messagedLine ] ]
        <> actions
        <> (if expandable || text /= "" then [ toggle ] else [])
    in
    HH.article [ HS.class_ classes ] $ catMaybes
    [ Just heading
    , if null facts then Nothing
        else Just $ HH.div [ HS.class_ "facts-clip" ] [ HH.div [ HS.class_ "facts" ] $ renderFact <$> facts ]
    , if text == "" then Nothing else Just $ HH.p [ HS.class_ "card-text" ] [ HH.text text ]
    , if null detailRows then Nothing else Just $ HH.div [ HS.class_ "card-details" ] detailRows
    , Just footer
    ]

-- | Which tier of the feed a card's marks put it in: 0 fits, 1 misses one
-- | thing, 2 misses more. A card none of the description applies to goes last.
tierOf :: CardRow -> Int
tierOf post = let
    marks = Object.values post.marks
    misses = marks # filter (notEq "fit") # length
    in
    if null marks then 2 else min misses 2
