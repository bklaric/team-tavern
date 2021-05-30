module TeamTavern.Client.Pages.Competitions where

import Prelude

import Async (Async)
import Data.Array (singleton)
import Data.Const (Const)
import Data.Foldable (foldMap)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (insert)
import TeamTavern.Client.Components.Detail (fieldDetail, urlDetail)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.ViewCompetitions as ViewCompetitions
import TeamTavern.Routes.ViewGame as ViewGame

type Input = { game :: ViewGame.OkContent }

type Competition = Record (ViewCompetitions.OkCompetitionRow (expanded :: Boolean))

data State
    = Empty Input
    | Loaded { game :: ViewGame.OkContent, competitions :: Array Competition }
    | Error

data Action = Initialize | Receive Input | Expand String

type Slot = H.Slot (Const Void) Void Unit

render :: forall slots. State -> HH.HTML slots Action
render (Empty _) = HH.div_ []
render (Loaded { game, competitions: competitions' }) =
    HH.div [ HS.class_ "competitions" ] $
    mapFlipped competitions' \competition ->
        HH.div [ HS.class_ "competition" ]
        [ HH.img [ HS.class_ "competition-banner", HP.src competition.banner ]
        , HH.div [ HS.class_ "competition-text" ]
            [ HH.h2 [ HS.class_ "competition-heading" ] [ HH.text competition.name ]
            , HH.div [ HS.class_ "competition-details" ] $
                foldMap singleton (urlDetail "fas fa-globe" "Website" competition.website)
                <>
                foldMap singleton (urlDetail "fab fa-discord" "Discord server" competition.discordServer)
                <>
                [ fieldDetail "fas fa-globe-europe" "Region"
                    [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text competition.region ] ]
                ]
            , HH.div [ HS.class_ $ "competition-description" <> guard competition.expanded " expanded" ]
                [ HH.span
                    [ HS.class_ "competition-description-expand"
                    , HE.onClick $ const $ Just $ Expand competition.name
                    ]
                    [ HH.text $ if competition.expanded then "Less" else "More" ]
                , HH.div [ HS.class_ "competition-description-text" ] $
                    mapFlipped competition.description \paragraph ->
                        HH.p [ HS.class_ "competition-description-paragraph" ] [ HH.text paragraph ]
                ]
            ]
        ]
render Error =
    HH.p_ [ HH.text "There has been an error loading competitions. Please try again later." ]

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive { game }) = do
    competitions' <- H.lift $ get $ "/api/games/" <> game.handle <> "/competitions"
    H.put case competitions' of
        Nothing -> Error
        Just competitions'' -> Loaded
            { game, competitions: competitions'' <#> insert (SProxy :: SProxy "expanded") false }
handleAction (Expand name) =
    H.modify_ case _ of
        Loaded { game, competitions: competitions' } -> Loaded
            { game
            , competitions: competitions' <#> \competition ->
                if competition.name == name
                then competition { expanded = not competition.expanded }
                else competition
            }
        state -> state

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

competitions :: forall query children left.
    Input -> HH.ComponentHTML query (competitions :: Slot | children) (Async left)
competitions input = HH.slot (SProxy :: SProxy "competitions") unit component input absurd