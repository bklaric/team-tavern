module TeamTavern.Client.Pages.Account.Details where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Account.EditDetails (editDetails)
import TeamTavern.Client.Pages.Account.EditDetails as EditDetails
import TeamTavern.Client.Pages.Account.Types (Nickname, PlayerStatus(..))
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Player.ViewDetails.SendResponse as ViewDetails

data Input = Input Nickname PlayerStatus

data Action
    = Initialize
    | Receive Input
    | ShowModal EditDetails.Input
    | HandleModalOutput (Modal.Message EditDetails.Output)
    | CopyDiscordTag String

type DiscordTagCopied = Boolean

data State
    = Empty Input
    | Details Nickname PlayerStatus ViewDetails.OkContent DiscordTagCopied

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots = (editDetails :: EditDetails.Slot)

ifNull :: forall a. Array a -> Array a -> Array a
ifNull replacement array = if Array.null array then replacement else array

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Details nickname playerStatus details' discordTagCopied) =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ] $
        [ HH.span [ HP.class_ $ HH.ClassName "card-title-text" ]
            [ HH.text "Player details" ]
        ]
        <>
        case playerStatus of
        SamePlayer -> Array.singleton $
            HH.button
            [ HP.class_ $ HH.ClassName "regular-button"
            , HE.onClick $ const $ Just $ ShowModal $ EditDetails.Input nickname details'
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
            , HH.text "Edit player details"
            ]
        _ -> []
    ]
    <>
    case playerStatus of
    SamePlayer -> [ editDetails (Just <<< HandleModalOutput) ]
    _ -> []
    <>
    [ HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        $ ifNull
            [ HH.p_
                [ HH.text
                    case playerStatus of
                    SamePlayer -> "You haven't entered any details."
                    _ -> "This player hasn't entered any details."
                ]
            ]
        $ Array.catMaybes
        [ details'.age <#> \age ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Is " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show age ]
            , HH.text " years old"
            ]
        , details'.country <#> \country ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Lives in " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ]
            ]
        , if Array.null details'.languages
            then Nothing
            else Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Speaks " ]
                ]
                <>
                (Array.foldr
                    (\language state ->
                        if not state.firstLanguage
                        then state { firstLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ] ] }
                        else if not state.secondLanguage
                        then state { secondLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text " and " ] <> state.languagesSoFar }
                        else state { languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text ", " ] <> state.languagesSoFar }
                    )
                    { firstLanguage: false, secondLanguage: false, languagesSoFar: [] }
                    details'.languages
                    # _.languagesSoFar
                )
        , if details'.hasMicrophone
            then Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text "Has microphone" ]
                , HH.text $ " and is willing to communicate"
                ]
            else Nothing
        , details'.clientWeekdayOnline <#> \{ from, to } ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekdays" ]
            , HH.text " from "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
            , HH.text " to "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
            ]
        , details'.clientWeekendOnline <#> \{ from, to } ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekends" ]
            , HH.text " from "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
            , HH.text " to "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
            ]
        , details'.discordTag <#> \discordTag ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
            [ HH.i [ HP.class_ $ HH.ClassName "fab fa-discord profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text "Discord tag: " ]
            , HH.a
                [ HP.class_ $ HH.ClassName "discord-tag"
                , HE.onClick $ const $ Just $ CopyDiscordTag discordTag ]
                [ HH.text discordTag ]
            ]
            <>
            if discordTagCopied
            then Array.singleton $ HH.span [ HP.class_ $ HH.ClassName "discord-tag-copied" ] [ HH.text "Copied!" ]
            else []
        ]
    ]

loadDetails :: forall left. Nickname -> Async left (Maybe ViewDetails.OkContent)
loadDetails nickname = Async.unify do
    timezone <- H.liftEffect getClientTimezone
    response
        <- Fetch.fetch
           ("/api/players/by-nickname/" <> nickname <> "/details?timezone=" <> timezone)
           (Fetch.credentials := Fetch.Include)
        #  lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty (Input nickname playerStatus) -> do
            details' <- H.lift $ loadDetails nickname
            case details' of
                Just details'' ->
                    H.put $ Details nickname playerStatus details'' false
                Nothing -> pure unit
        _ -> pure unit
handleAction (Receive (Input nickname playerStatus)) = do
    profiles <- H.lift $ loadDetails nickname
    case profiles of
        Just profiles' -> H.put $ Details nickname playerStatus profiles' false
        Nothing -> pure unit
handleAction (ShowModal input) =
    Modal.showWith input (SProxy :: SProxy "editDetails")
handleAction (HandleModalOutput message) = do
    Modal.hide (SProxy :: SProxy "editDetails")
    case message of
        Modal.Inner (EditDetails.DetailsEditted nickname) ->
            handleAction $ Receive $ Input nickname SamePlayer
        _ -> pure unit
handleAction (CopyDiscordTag discordTag) = do
    result <- H.lift $ Async.attempt $ writeTextAsync discordTag
    case result of
        Right _ -> H.modify_
            case _ of
            Details nickname playerStates details' _ ->
                Details nickname playerStates details' true
            other -> other
        _ -> pure unit

component :: forall output left query.
    H.Component HH.HTML query Input output (Async left)
component = mkComponent
    { initialState: Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

details
    :: forall children' name children action left
    .  Cons name (Slot) children' children
    => IsSymbol name
    => Nickname
    -> PlayerStatus
    -> SProxy name
    -> HH.ComponentHTML action children (Async left)
details nickname playerStatus slot =
    HH.slot slot unit component (Input nickname playerStatus) absurd
