module TeamTavern.Client.Game.Profiles (Slot, gameProfiles) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Foldable (find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..), defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Profile.ViewByGame.SendResponse as ViewByGame

data Action = Init View.OkContent

data State
    = Empty View.OkContent
    | Profiles View.OkContent ViewByGame.OkContent

type Slot = H.Slot (Const Void) Void

type ChildSlots = (players :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Profiles game profiles) = HH.div_ $
    [ HH.h3 [ HP.class_ $ ClassName "card-header"] [ HH.text "Profiles" ] ] <>
    (profiles # mapWithIndex \index { nickname, summary, fieldValues } ->
        HH.div [ HP.class_ $ ClassName "card" ] $
        [ HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "players") index
            { path: "/players/" <> nickname, content: HH.text nickname } ]
        ]
        <> (Array.catMaybes $ game.fields <#> \field -> let
            fieldValue = fieldValues # find \ { fieldId } -> field.id == fieldId
            in
            case { type: field.type, fieldValue } of
            { type: 1, fieldValue: Just { url: Just url' } } -> Just $
                HH.p_
                [ HH.text $ field.label <> ": "
                , HH.a [ HP.href url' ] [ HH.text url' ]
                ]
            { type: 2, fieldValue: Just { optionId: Just optionId' } } -> let
                option' = field.options >>= find (\{ id } -> id == optionId')
                in
                option' <#> \{ option } ->
                    HH.p_ [ HH.text $ field.label <> ": " <> option ]
            { type: 3, fieldValue: Just { optionIds: Just optionIds' } } -> let
                options' = field.options <#> Array.filter \{ id } -> Array.elem id optionIds'
                in
                case options' of
                Just options | not $ Array.null options -> Just $ HH.p_
                    [ HH.text $ field.label <> ": "
                        <> intercalate ", " (options <#> _.option)
                    ]
                _ -> Nothing
            _ ->  Nothing)
        <> (summary <#> \paragraph -> HH.p_ [ HH.text paragraph ])
    )

loadProfiles :: forall left. View.OkContent -> Async left State
loadProfiles game @ { handle } = Async.unify do
    let empty = Empty game
    response <-  Fetch.fetch_ ("/api/profiles?handle=" <> handle)
        # lmap (const empty)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const empty)
        _ -> Async.left empty
    pure $ Profiles game content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init game) = do
    state <- H.lift $ loadProfiles game
    H.put state
    pure unit

component
    :: forall output left query
    .  View.OkContent
    -> H.Component HH.HTML query View.OkContent output (Async left)
component game = mkComponent
    { initialState: Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
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
