module TeamTavern.Client.Pages.About (Slot, about) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Snippets.SocialMediaUrls (discordUrl, redditUrl, steamUrl, twitterUrl)

data Action = Init

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( dotaAnchor :: NavigationAnchor.Slot Unit
    , emailAnchor :: NavigationAnchor.Slot Unit
    )
render :: forall monad. MonadEffect monad =>
    HH.ComponentHTML Action ChildSlots monad
render = HH.div_
    [ HH.h1 [ HP.class_ $ HH.ClassName "about-title" ] [ HH.text "About TeamTavern" ]
    , HH.h2 [ HP.class_ $ HH.ClassName "about-heading" ] [ HH.text "What is TeamTavern?" ]
    , HH.p_
        [ HH.text $ "TeamTavern is an online platform for finding esports teammates. "
            <> "It aims to provide players of various online multiplayer games "
            <> "with a common meeting place where they can find each other and form teams. "
            <> "It currently features "
        , navigationAnchor (SProxy :: SProxy "dotaAnchor")
            { path: "/games/dota2", content: HH.text "Dota 2" }
        , HH.text " with more games planned in the future."
        ]
    , HH.h2 [ HP.class_ $ HH.ClassName "about-heading" ] [ HH.text "How does TeamTavern work?" ]
    , HH.p_
        [ HH.text $ "Select a game and browse players looking for a team and teams looking for players. "
            <> "Filter players and teams to find your ideal teammates. "
            <> "If you still can't find suitable teammates, create your own profile and let them find you."
        ]
    , HH.h2 [ HP.class_ $ HH.ClassName "about-heading" ] [ HH.text "How to contact TeamTavern?" ]
    , HH.p_
        [ HH.text "You can contact "
        , navigationAnchor (SProxy :: SProxy "emailAnchor")
            { path: "mailto:admin@teamtavern.net", content: HH.text "admin@teamtavern.net" }
        , HH.text " for all inquiries regarding TeamTavern."
        ]
    , HH.p_
        [ HH.text "Additionally, TeamTavern maintains the following social media presence: " ]
    , HH.ul [ HP.class_ $ HH.ClassName "about-list" ]
        [ HH.li_ [ HH.a [ HP.href discordUrl ] [ HH.text "Discord server" ] ]
        , HH.li_ [ HH.a [ HP.href redditUrl ] [ HH.text "Subreddit" ] ]
        , HH.li_ [ HH.a [ HP.href steamUrl ] [ HH.text "Steam community group" ] ]
        , HH.li_ [ HH.a [ HP.href twitterUrl ] [ HH.text "Twitter account" ] ]
        ]
    , HH.p_
        [ HH.text "Come find out about the latest updates, give feedback, ask questions, find teammates or just say hi!" ]
    ]

handleAction :: forall monad. MonadEffect monad => Action -> monad Unit
handleAction Init = do
    H.liftEffect do
        setMetaTitle "About | TeamTavern"
        setMetaDescription "TeamTavern is an online platform for finding esports teammates."
        setMetaUrl

component :: forall monad output input query. MonadEffect monad =>
    H.Component HH.HTML query input output monad
component = H.mkComponent
    { initialState: const unit
    , render: const render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

about :: forall action monad children. MonadEffect monad =>
    HH.ComponentHTML action (about :: Slot | children) monad
about = HH.slot (SProxy :: SProxy "about") unit component unit absurd