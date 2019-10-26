module TeamTavern.Client.Game.Profiles (Query(..), Slot, gameProfiles) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.String as String
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..), defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Game.CreateProfile (createProfile)
import TeamTavern.Client.Game.CreateProfile as CreateProfile
import TeamTavern.Client.Game.FilterProfiles as FilterProfiles
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Cookie as Cookie
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Profile.ViewByGame.SendResponse as ViewByGame
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Init View.OkContent
    | ShowCreateProfileModal View.OkContent PlayerInfo MouseEvent
    | HandleCreateProfileMessage (Modal.Message CreateProfile.Message)

data State
    = Empty View.OkContent
    | Profiles View.OkContent ViewByGame.OkContent (Maybe Cookie.PlayerInfo)

data Query send = ApplyFilters (Array FilterProfiles.Field) send

type Slot = H.Slot Query Void

type ChildSlots =
    ( players :: Anchor.Slot Int
    , createProfile :: CreateProfile.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Profiles game profiles playerInfo') =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h3 [ HP.class_ $ HH.ClassName "card-title" ] $ join
        [ pure $ HH.text "Profiles"
        , case playerInfo' of
            Just playerInfo | not game.hasProfile -> pure $ HH.button
                [ HP.class_ $ ClassName "card-title-button primary-button"
                , HE.onClick $ Just <<< ShowCreateProfileModal game playerInfo
                ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
                , HH.text "Create profile"
                ]
            _ -> []
        ]
    , createProfile $ Just <<< HandleCreateProfileMessage
    ]
    <>
    if Array.null profiles
    then pure $
        HH.div [ HP.class_ $ ClassName "card-section" ]
        [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ]
    else
        (profiles # mapWithIndex \index { nickname, summary, fieldValues } ->
            HH.div [ HP.class_ $ ClassName "card-section" ] $
            [ HH.h3_
                [ navigationAnchorIndexed (SProxy :: SProxy "players") index
                    { path: "/players/" <> nickname, content: HH.text nickname }
                ]
            ]
            <> (Array.catMaybes $ game.fields <#> \field -> let
                fieldValue = fieldValues # find \ { fieldKey } -> field.key == fieldKey
                in
                case { type: field.type, fieldValue } of
                { type: 1, fieldValue: Just { url: Just url' } } -> Just $
                    HH.p_
                    [ HH.strong_ [ HH.text $ field.label <> ": " ]
                    , HH.a [ HP.href url' ] [ HH.text url' ]
                    ]
                { type: 2, fieldValue: Just { optionKey: Just optionKey' } } -> let
                    fieldOption' = field.options >>= find (\{ key } -> key == optionKey')
                    in
                    fieldOption' <#> \{ option } ->
                        HH.p_ [ HH.strong_ [ HH.text $ field.label <> ": " ], HH.text option ]
                { type: 3, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $ HH.p_
                        [ HH.strong_ [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                _ ->  Nothing)
            <> (summary <#> \paragraph -> HH.p_ [ HH.text paragraph ])
        )

loadProfiles :: forall left. View.OkContent -> Array FilterProfiles.Field -> Async left State
loadProfiles game @ { handle } fields = Async.unify do
    let empty = Empty game
    let filterPairs = fields
            <#> (\field -> field.options
                <#> \option -> field.key <> "=" <> option.key)
            # join
            # intercalate "&"
    let filterQuery = if String.null filterPairs then "" else "?" <> filterPairs
    response <- Fetch.fetch_
            ("/api/profiles/by-handle/" <> handle <> filterQuery)
        # lmap (const empty)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const empty)
        _ -> Async.left empty
    playerInfo' <- H.liftEffect getPlayerInfo
    pure $ Profiles game content playerInfo'

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init game) = do
    state <- H.lift $ loadProfiles game []
    H.put state
    pure unit
handleAction (ShowCreateProfileModal game playerInfo event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.showWith { game, playerInfo } (SProxy :: SProxy "createProfile")
handleAction (HandleCreateProfileMessage message) = do
    Modal.hide (SProxy :: SProxy "createProfile")
    case message of
        Modal.Inner (CreateProfile.ProfileCreated handle) ->
            H.liftEffect $ navigate_ $ "/games/" <> trim handle
        _ -> pure unit

handleQuery
    :: forall output send left
    .  Query send
    -> H.HalogenM State Action ChildSlots output (Async left) (Maybe send)
handleQuery (ApplyFilters fields send) = do
    state <- H.get
    case state of
        Empty _ -> pure $ Just send
        Profiles game _ _ -> do
            state' <- H.lift $ loadProfiles game fields
            H.put state'
            pure $ Just send

component
    :: forall output left
    .  View.OkContent
    -> H.Component HH.HTML Query View.OkContent output (Async left)
component game = mkComponent
    { initialState: Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just $ Init game
        , receive = Just <<< Init
        }
    }

gameProfiles
    :: forall query children left
    .  View.OkContent
    -> HH.ComponentHTML
        query (gameProfiles :: Slot Unit | children) (Async left)
gameProfiles game = HH.slot
    (SProxy :: SProxy "gameProfiles") unit (component game) game absurd
