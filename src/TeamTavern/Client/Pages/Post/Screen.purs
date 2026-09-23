module TeamTavern.Client.Pages.Post.Screen (postScreen) where

import Prelude

import Async (Async)
import Async as Async
import Control.Alt ((<|>))
import Data.Array (any, elem, find, foldl, snoc)
import Data.Array.NonEmpty as Nea
import Data.Date (Date)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Data.Variant (match, onMatch)
import Effect.Class (liftEffect)
import Effect.Now (now, nowDate)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button, buttonLink)
import TeamTavern.Client.Components.Card (card)
import TeamTavern.Client.Components.Confirm (confirm)
import TeamTavern.Client.Components.Field (formSection)
import TeamTavern.Client.Components.Flow (flowError, flowLead, submitButton)
import TeamTavern.Client.Components.Overlay (Presentation(..), overlay, useOverlay)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Feed.Description (current, loadStored)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Pages.Post.Draft (Draft, clearDraft, emptyDraft, fromContent, fromDescription, gameContacts, loadDraft, saveDraft, toCard, toRequest, withAccount)
import TeamTavern.Client.Pages.Post.Fields (cardFields, contactFields, contactKeys, wordsField)
import TeamTavern.Client.Pages.Post.Register (registerBack)
import TeamTavern.Client.Script.Back (authPath)
import TeamTavern.Client.Script.Discord (authorizeWithDiscord)
import TeamTavern.Client.Script.Expand (toggleCard)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate_, replaceState)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound)
import TeamTavern.Client.Script.Scroll (focusFirstInvalid)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.AccountErrors (somethingWrong)
import TeamTavern.Client.Shared.Contacts (contactLabel)
import TeamTavern.Client.Shared.Fetch (fetchPath, fetchPathBody, fetchSimple)
import TeamTavern.Client.Shared.Me (fetchMe)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Country.ViewCountries (ViewCountries)
import TeamTavern.Routes.Country.ViewCountries as ViewCountries
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Post.CreatePost (CreatePost)
import TeamTavern.Routes.Post.DeletePost (DeletePost)
import TeamTavern.Routes.Post.UpdatePost (UpdatePost)
import TeamTavern.Routes.Post.ViewOwnPost (ViewOwnPost)
import TeamTavern.Routes.Post.ViewOwnPost as ViewOwnPost
import TeamTavern.Routes.Shared.Post (PostError)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)

type Input = { handle :: String, type_ :: String }

data Screen = Loading | Missing | Failed | Ready

-- | `own` is the signed-in player's post of the type, with their account;
-- | `otherPosts` whether they have posts besides it. `conflicting` is whether
-- | the draft meets that post, which the player then either updates with the
-- | draft or keeps as it is (brief 6, Entry points).
type State =
    { screen :: Screen
    , game :: Maybe ViewGame.OkContent
    , countries :: ViewCountries.OkContent
    , own :: Maybe ViewOwnPost.OkContent
    , otherPosts :: Boolean
    , conflicting :: Boolean
    , nickname :: Maybe String
    , draft :: Draft
    , changing :: Array String
    , errors :: Object String
    , formError :: Maybe String
    , sending :: Boolean
    , confirmingDelete :: Boolean
    , previewOpen :: Boolean
    , previewExpanded :: Boolean
    , ownExpanded :: Boolean
    , timezone :: String
    , now :: Maybe { instant :: Instant, iso :: String, date :: Date }
    }

typeTitle :: String -> String
typeTitle = case _ of
    "group" -> "Group"
    "community" -> "Community"
    _ -> "Player"

screenTitle :: String -> String
screenTitle = case _ of
    "group" -> "Tell players about your group"
    "community" -> "Tell players about your community"
    _ -> "Tell groups and players about you"

-- How long a post of the type stays active (brief 9).
days :: String -> Int
days "community" = 90
days _ = 30

-- The checks the server makes that the screen can name before sending
-- (brief 6, step 3).
validate :: ViewGame.OkContent -> String -> Draft -> Object String
validate game type_ draft = Object.fromFoldable $ foldl (\errors (key /\ error) -> maybe errors (snoc errors <<< (key /\ _)) error) []
    [ "name" /\ (if community && trim draft.name == "" then Just "Give your community a name." else Nothing)
    , "text" /\ (if community && trim draft.text == "" then Just "Tell players what your community is about." else Nothing)
    , "discordServer" /\
        (if community && draft.reach == "discord" && trim draft.discordServer == ""
        then Just "Add your invite, or choose another way to join." else Nothing)
    , "website" /\
        (if community && draft.reach == "website" && trim draft.website == ""
        then Just "Add your website, or choose another way to join." else Nothing)
    , "reach" /\
        (if not community && draft.reach == "offsite" && Object.isEmpty (gameContacts game type_ draft)
        then Just $ "Add your " <> accounts <> " below, or choose another way." else Nothing)
    , "hours" /\
        (if isJust draft.online.from /= isJust draft.online.to then Just "Choose both times, or neither." else Nothing)
    ]
    where
    community = type_ == "community"
    accounts = case game.contacts # find (notEq "discord") of
        Just kind | elem "discord" game.contacts -> "Discord or " <> contactLabel kind
        Just kind -> contactLabel kind
        Nothing -> "Discord"

-- What the server turned down, beside the field it is about. A field the
-- screen doesn't ask can't be wrong but for a request it didn't make.
serverError :: PostError -> Maybe { key :: String, error :: String }
serverError = match
    { name: const $ Just { key: "name", error: "Use a name of up to 50 characters." }
    , summary: const $ Just { key: "text", error: "Keep it under 2,000 characters." }
    , hours: const $ Just { key: "hours", error: "Choose both times, or neither." }
    , reach: const $ Just { key: "reach", error: "Add a way to reach you below, or choose another way." }
    , discordServer: const $ Just { key: "discordServer", error: "Check your invite, or choose another way to join." }
    , website: const $ Just { key: "website", error: "Check your website, or choose another way to join." }
    , contact: \{ kind } -> Just { key: kind, error: "Check this account." }
    , field: const Nothing
    }

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { handle, type_ } -> Hooks.do
    state /\ stateId <- Hooks.useState
        ({ screen: Loading
        , game: Nothing
        , countries: { regions: [], countries: [] }
        , own: Nothing
        , otherPosts: false
        , conflicting: false
        , nickname: Nothing
        , draft: emptyDraft type_
        , changing: []
        , errors: Object.empty
        , formError: Nothing
        , sending: false
        , confirmingDelete: false
        , previewOpen: false
        , previewExpanded: false
        , ownExpanded: false
        , timezone: "UTC"
        , now: Nothing
        } :: State)

    let path = "/games/" <> handle <> "/post/" <> type_
        previewRef = H.RefLabel "post-preview"
        set = Hooks.modify_ stateId

        regionOf countries country = countries.countries # find (_.name >>> eq country) <#> _.region

        -- Every change is kept, so the draft outlasts the page.
        changeDraft key change = do
            state' <- Hooks.modify stateId \state' -> state'
                { draft = (change state'.draft) { signedOut = isNothing state'.own }
                , errors =
                    if elem key (maybe [] contactKeys state'.game)
                    then foldl (flip Object.delete) state'.errors (maybe [] contactKeys state'.game)
                    else Object.delete key state'.errors
                }
            liftEffect $ saveDraft handle type_ state'.draft

        startEditing = do
            state' <- Hooks.get stateId
            for_ state'.own \own -> for_ own.post \post -> do
                let draft = withAccount type_ (regionOf state'.countries) own.account (fromContent type_ post.content)
                set _ { draft = draft, changing = [], errors = Object.empty }
                liftEffect $ saveDraft handle type_ draft

        deletePost = do
            set _ { sending = true }
            result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ DeletePost) { handle, type: type_ }
            let deleted = do
                    liftEffect $ clearDraft handle type_
                    set \state' -> state'
                        { own = state'.own <#> _ { post = Nothing }
                        , draft = maybe identity (withAccount type_ (regionOf state'.countries) <<< _.account) state'.own
                            (emptyDraft type_)
                        , confirmingDelete = false
                        , sending = false
                        }
            case result of
                Right response -> response # onMatch
                    { noContent: const deleted, notFound: const deleted }
                    (const $ set _ { sending = false, formError = Just somethingWrong })
                Left _ -> set _ { sending = false, formError = Just somethingWrong }

        -- The player's post as it now is, after another tab published one,
        -- which the draft then meets.
        reloadOwn = do
            result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewOwnPost) { handle, type: type_ }
            case result of
                Right response -> response # onMatch
                    { ok: \own -> set _ { own = Just own, conflicting = true, sending = false } }
                    (const $ set _ { sending = false, formError = Just somethingWrong })
                Left _ -> set _ { sending = false, formError = Just somethingWrong }

        publish = do
            state' <- Hooks.get stateId
            for_ state'.game \game -> do
                let errors = validate game type_ state'.draft
                if not Object.isEmpty errors
                then do
                    set _ { errors = errors, previewOpen = false }
                    liftEffect focusFirstInvalid
                else if isNothing state'.own then do
                    -- Signed out, the player signs up or in and comes back to
                    -- the draft, which waits in local storage, to publish it.
                    liftEffect $ saveDraft handle type_ state'.draft { signedOut = true }
                    navigate_ $ authPath "/signup" $ registerBack path
                else do
                    set _ { sending = true, formError = Nothing, previewOpen = false }
                    let request = toRequest game type_ (fromMaybe state'.timezone state'.draft.timezone) state'.draft
                        editing = state'.draft.editing
                    result <- H.lift $ Async.attempt
                        if editing
                        then fetchPathBody (Proxy :: _ UpdatePost) { handle, type: type_ } request
                        else fetchPathBody (Proxy :: _ CreatePost) { handle, type: type_ } request
                    let failed = set _ { sending = false, formError = Just somethingWrong }
                    case result of
                        Right response -> response # onMatch
                            { noContent: const do
                                liftEffect $ clearDraft handle type_
                                navigate_ $ path <> "/live" <> if editing then "?updated=1" else ""
                            , badRequest: match
                                { post: \errors' -> let
                                    named = Nea.toArray errors' # foldl (\found error -> maybe found (snoc found) (serverError error)) []
                                    in
                                    if any (const true) named
                                    then do
                                        set _
                                            { sending = false
                                            , errors = Object.fromFoldable $ named <#> \{ key, error } -> key /\ error
                                            }
                                        liftEffect focusFirstInvalid
                                    else failed
                                , exists: const reloadOwn
                                }
                            }
                            (const failed)
                        Left _ -> failed

        submit event = do
            liftEffect $ preventDefault event
            publish

        -- Discord brings the player back here signed in, with the draft.
        signUpWithDiscord = do
            state' <- Hooks.get stateId
            liftEffect $ saveDraft handle type_ state'.draft { signedOut = true }
            authorizeWithDiscord path

        updateExisting = do
            state' <- Hooks.modify stateId \state' -> state'
                { draft = state'.draft { editing = true, signedOut = false }, conflicting = false }
            liftEffect $ saveDraft handle type_ state'.draft
            publish

        discardDraft = do
            liftEffect $ clearDraft handle type_
            navigate_ $ "/games/" <> handle

    useOverlay previewRef Modal state.previewOpen (set _ { previewOpen = false })

    Hooks.useLifecycleEffect do
        instant <- liftEffect now
        iso <- liftEffect $ JSDate.now >>= JSDate.toISOString
        date <- liftEffect nowDate
        timezone <- getClientTimezone
        from <- getQueryParam "from"
        set _ { now = Just { instant, iso, date }, timezone = timezone }

        void $ Hooks.fork do
            let failed = set _ { screen = Failed }
            gameResult <- H.lift $ hush <$> Async.attempt (fetchPath (Proxy :: _ ViewGame) { handle })
            countriesResult <- H.lift $ hush <$> Async.attempt (fetchSimple (Proxy :: _ ViewCountries))
            me <- H.lift fetchMe
            ownResult <-
                if isJust me
                then H.lift $ hush <$> Async.attempt (fetchPath (Proxy :: _ ViewOwnPost) { handle, type: type_ })
                else pure Nothing
            let countries = countriesResult >>= onMatch { ok: Just } (const Nothing)
                    # fromMaybe { regions: [], countries: [] }
                own = ownResult >>= onMatch { ok: Just } (const Nothing)
                posts = me # maybe 0 \me' ->
                    foldl (\count game -> count + game.posts) 0 me'.games
                otherPosts = posts > (if isJust (own >>= _.post) then 1 else 0)
            case gameResult of
                Nothing -> failed
                Just response -> response # onMatch
                    { ok: \game -> do
                        setMeta ("Your " <> game.title <> " " <> type_ <> " post | TeamTavern")
                            ("Post on TeamTavern: say who you are and see who fits.")
                        stored <- liftEffect $ loadDraft handle type_
                        described <- liftEffect $ loadStored handle <#> map \stored' -> current stored' { type = type_ }
                        let post = own >>= _.post
                            fromPost = post <#> \post' -> fromContent type_ post'.content
                            -- A draft left editing a post that is gone
                            -- publishes a new one.
                            kept = stored <#> \left -> left { editing = left.editing && isJust post }
                            base = case from of
                                Just "edit" | Just edited <- fromPost -> edited
                                Just "feed" -> maybe identity fromDescription described
                                    (fromPost <|> kept # fromMaybe (emptyDraft type_))
                                _ -> kept # fromMaybe (emptyDraft type_)
                            -- Signed in, a draft written signed out meets the
                            -- player's post, if they have one, and is theirs
                            -- to write on otherwise.
                            conflicting = base.signedOut && isJust post
                            draft = case own of
                                Just own' -> withAccount type_ (regionOf countries) own'.account base
                                    # \draft' -> draft' { signedOut = conflicting }
                                Nothing -> base
                        -- The draft is saved as it now is, so a reload doesn't
                        -- lay the description over it again.
                        when (isJust from) $ replaceState {} path
                        liftEffect $ saveDraft handle type_ draft
                        set _
                            { screen = Ready
                            , game = Just game
                            , countries = countries
                            , nickname = me <#> _.nickname
                            , own = own
                            , otherPosts = otherPosts
                            , conflicting = conflicting
                            , draft = draft
                            }
                        -- Back from the register step, the post goes live.
                        when (from == Just "register" && isJust own && not conflicting) publish
                    , notFound: const do
                        appendRenderReadyNotFound
                        setMeta "Page not found | TeamTavern" ""
                        set _ { screen = Missing }
                    }
                    (const failed)
        pure Nothing

    let signedIn = isJust state.own

        context game =
            HH.div [ HS.class_ "step-context" ] $
            [ HH.img [ HP.src $ "/images/games/" <> handle <> ".webp", HP.alt "" ]
            , HH.span_ [ HH.strong_ [ HH.text game.title ], HH.text $ " · " <> typeTitle type_ <> " post" ]
            ]
            <> if state.draft.editing then []
                else
                [ buttonLink Text Small ("/post/" <> type_) [ HH.text "Change game" ]
                , buttonLink Text Small ("/post?game=" <> handle) [ HH.text "Change type" ]
                ]

        previewCard game now' draft updated expanded onToggle =
            card
            { game
            , viewer: { now: now'.instant, timezone: state.timezone }
            , post: toCard game type_ { nickname: state.nickname, updated, today: now'.date } draft
            , marked: false
            , expanded
            , preview: not signedIn
            , onToggle
            , onContact: pure unit
            , onEdit: pure unit
            , onRenew: pure unit
            }

        togglePreview (event :: MouseEvent) =
            toggleCard event $ set \state' -> state' { previewExpanded = not state'.previewExpanded }

        publishLabel = if state.draft.editing then "Save post" else "Publish post"

        rules =
            if state.draft.editing
            then [ HH.p_ [ HH.text $ "Saving renews your post: it stays active for " <> show (days type_) <> " days from today." ] ]
            else
                [ HH.p_ [ HH.text $ "Your post stays active for " <> show (days type_)
                    <> " days. We'll email you before it expires, and tell you when someone new fits." ] ]
                <> if signedIn then [] else [ HH.p_ [ HH.text "You'll create an account next. Nothing you've written is lost." ] ]

        -- The player's post of this type, as the feed shows it.
        ownCard game now' own post = let
            draft = withAccount type_ (regionOf state.countries) own.account (fromContent type_ post.content)
            in
            card
            { game
            , viewer: { now: now'.instant, timezone: state.timezone }
            , post: (toCard game type_ { nickname: state.nickname, updated: post.updated, today: now'.date } draft)
                { id = post.id }
            , marked: false
            , expanded: state.ownExpanded
            , preview: true
            , onToggle: \event -> toggleCard event $ set \state' -> state' { ownExpanded = not state'.ownExpanded }
            , onContact: pure unit
            , onEdit: pure unit
            , onRenew: pure unit
            }

        existingHeading game = HH.h1_ [ HH.text $ "You already have a " <> game.title <> " " <> type_ <> " post" ]

        -- A player with a post of this type for the game is shown it, with
        -- Edit it and Delete it (brief 6).
        existing game now' own post = let
            name = post.content.name # fromMaybe ("your " <> game.title <> " " <> type_ <> " post")
            in
            HH.div [ HS.class_ "flow" ] $
            [ context game
            , existingHeading game
            , flowLead $ "You can have one " <> type_ <> " post for each game. Edit this one, or delete it to start a new one."
            , ownCard game now' own post
            ]
            <> maybe [] (pure <<< flowError) state.formError
            <> if state.confirmingDelete
                then
                [ confirm
                    { id: "delete-post"
                    , title: "Delete " <> name <> "?"
                    , text:
                        if post.conversations == 0 then "It has no conversations."
                        else show post.conversations <> " "
                            <> (if post.conversations == 1 then "conversation" else "conversations")
                            <> " will be deleted for both of you."
                    , action: [ Icons.trash2, HH.text "Delete post" ]
                    , onConfirm: deletePost
                    , cancel: "Keep it"
                    , onCancel: set _ { confirmingDelete = false }
                    }
                ]
                else
                [ HH.div [ HS.class_ "flow-actions" ]
                    [ button Primary Regular startEditing [ Icons.pencil, HH.text "Edit it" ]
                    , button Destructive Regular (set _ { confirmingDelete = true, formError = Nothing })
                        [ Icons.trash2, HH.text "Delete it" ]
                    ]
                ]

        -- A draft that meets the player's post, once they have signed in or
        -- published from another tab: update the post with it, or keep the
        -- post and discard the draft (brief 6, Entry points).
        conflict game now' own post =
            HH.div [ HS.class_ "flow" ] $
            [ existingHeading game
            , flowLead "Update it with what you just wrote, or keep it as it is and discard what you wrote."
            , HH.p [ HS.class_ "caption" ] [ HH.text "Your post" ]
            , ownCard game now' own post
            , HH.p [ HS.class_ "caption" ] [ HH.text "What you just wrote" ]
            , previewCard game now' state.draft now'.iso state.previewExpanded togglePreview
            ]
            <> maybe [] (pure <<< flowError) state.formError
            <>
            [ HH.div [ HS.class_ "flow-actions" ]
                [ HH.button
                    [ HS.class_ "button button-primary"
                    , HP.type_ HP.ButtonButton
                    , HP.disabled state.sending
                    , HE.onClick $ const updateExisting
                    ]
                    [ HH.text "Update my post" ]
                , button Outline Regular discardDraft [ HH.text "Keep my post as it is" ]
                ]
            ]

        fieldContext game =
            { game
            , type_
            , draft: state.draft
            , account: state.own <#> _.account
            , nickname: state.nickname
            , otherPosts: state.otherPosts
            , changing: state.changing
            , errors: state.errors
            , countries: state.countries.countries <#> _.name
            , regions: state.countries.regions
            , timezone: state.timezone
            , onChange: changeDraft
            , onUnfold: \key -> set \state' -> state' { changing = snoc state'.changing key }
            , onDiscord: signUpWithDiscord
            }

        form game now' =
            [ HH.div [ HS.class_ "flow flow-wide" ]
                [ context game
                , HH.div [ HS.class_ "post-layout" ]
                    [ HH.form [ HS.class_ "form", HP.noValidate true, HE.onSubmit submit ] $
                        [ HH.h1_ [ HH.text if state.draft.editing then "Edit your " <> type_ <> " post" else screenTitle type_ ]
                        , formSection Nothing $ cardFields $ fieldContext game
                        , formSection Nothing [ wordsField $ fieldContext game ]
                        , formSection Nothing $ contactFields $ fieldContext game
                        ]
                        <> maybe [] (pure <<< flowError) state.formError
                        <> [ HH.div [ HS.class_ "publish-footer" ]
                                [ HH.div_ rules, submitButton state.sending publishLabel ]
                           ]
                    , HH.aside [ HS.class_ "preview-column", HPA.label "Preview" ]
                        [ HH.p [ HS.class_ "preview-label" ]
                            [ HH.span_ [ HH.text "Preview" ]
                            , HH.span_ [ HH.text $ "As it shows in the " <> game.title <> " feed" ]
                            ]
                        , previewCard game now' state.draft now'.iso state.previewExpanded togglePreview
                        ]
                    ]
                ]
            -- Below a desktop the preview is opened on demand (brief 6, step 3).
            , HH.div [ HS.class_ "action-bar" ]
                [ button Outline Regular (set _ { previewOpen = true }) [ Icons.eye, HH.text "Preview" ]
                , HH.button
                    [ HS.class_ "button button-primary"
                    , HP.type_ HP.ButtonButton
                    , HP.disabled state.sending
                    , HE.onClick $ const publish
                    ]
                    [ HH.text publishLabel ]
                ]
            , if state.previewOpen
                then overlay
                    { ref: previewRef, presentation: Modal, title: "Preview", onClose: set _ { previewOpen = false } }
                    [ HH.p [ HS.class_ "field-hint" ] [ HH.text $ "As it shows in the " <> game.title <> " feed" ]
                    , previewCard game now' state.draft now'.iso state.previewExpanded togglePreview
                    ]
                    [ button Primary Regular publish [ HH.text publishLabel ] ]
                else HH.text ""
            ]

    Hooks.pure case state.screen, state.game, state.now of
        Missing, _, _ -> placeholder "Page could not be found."
        Failed, _, _ -> placeholder "There has been an error loading the post screen."
        Ready, Just game, Just now' -> case state.own of
            Just own | Just post <- own.post, state.conflicting -> conflict game now' own post
            Just own | Just post <- own.post, not state.draft.editing -> existing game now' own post
            _ -> HH.div_ $ form game now'
        _, _, _ -> HH.div [ HS.class_ "flow flow-wide" ] []

postScreen :: ∀ action slots left.
    Int -> Input -> H.ComponentHTML action (postScreen :: Slot__I Int | slots) (Async left)
postScreen visit input = HH.slot_ (Proxy :: _ "postScreen") visit component input
