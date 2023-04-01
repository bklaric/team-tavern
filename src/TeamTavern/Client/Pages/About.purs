module TeamTavern.Client.Pages.About (about) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.SocialMediaUrls (discordUrl, redditUrl, steamId, twitterUrl)
import Type.Proxy (Proxy(..))

data Action = Initialize

type ChildSlots = (emailAnchor :: Slot___)

render :: ∀ monad. MonadEffect monad => HH.ComponentHTML Action ChildSlots monad
render = HH.div [ HS.class_ "about" ] $
    [ HH.h1 [ HS.class_ "about-title" ] [ HH.text "About TeamTavern" ]
    , HH.h2 [ HS.class_ "about-heading" ] [ HH.text "What is TeamTavern?" ]
    , HH.p [ HS.class_ "about-text" ]
        [ HH.text $ "TeamTavern is an online platform for finding esports teammates. "
            <> "It aims to provide players of various online multiplayer games "
            <> "with a common meeting place where they can find each other and form teams. "
        ]
    , HH.h2 [ HS.class_ "about-heading" ] [ HH.text "How does TeamTavern work?" ]
    , HH.p [ HS.class_ "about-text" ]
        [ HH.text $ "Select a game and browse players looking for a team and teams looking for players. "
            <> "Filter players and teams to find your ideal teammates. "
            <> "If you still can't find suitable teammates, create your own profile and let them find you."
        ]
    , HH.h2 [ HS.class_ "about-heading" ] [ HH.text "How to contact TeamTavern?" ]
    , HH.p [ HS.class_ "about-text" ]
        [ HH.text "You can contact "
        , navigationAnchor (Proxy :: _ "emailAnchor")
            { path: "mailto:admin@teamtavern.net", content: HH.text "admin@teamtavern.net" }
        , HH.text " for all inquiries regarding TeamTavern."
        ]
    , HH.p [ HS.class_ "about-text" ]
        [ HH.text "Additionally, TeamTavern maintains the following social media presence: " ]
    , HH.ul [ HS.class_ "about-list" ]
        [ HH.li_ [ HH.a [ HP.href discordUrl ] [ HH.text "Discord server" ] ]
        , HH.li_ [ HH.a [ HP.href redditUrl ] [ HH.text "Subreddit" ] ]
        , HH.li_ [ HH.a [ HP.href steamId ] [ HH.text "Steam community group" ] ]
        , HH.li_ [ HH.a [ HP.href twitterUrl ] [ HH.text "Twitter account" ] ]
        ]
    , HH.p [ HS.class_ "about-text" ]
        [ HH.text "Come find out about the latest updates, give feedback, ask questions, find teammates or just say hi!" ]
    , HH.h2 [ HS.class_ "about-heading" ] [ HH.text "Advertising" ]
    , HH.p [ HS.class_ "about-text" ]
        [ HH.text $ "For all advertising inquiries please contact Andrew Church (andrew.church@network-n.com)." ]
    ]

handleAction :: ∀ monad. MonadEffect monad => Action -> monad Unit
handleAction Initialize =
    setMeta "About | TeamTavern" "TeamTavern is an online platform for finding esports teammates."

component :: ∀ monad output input query. MonadEffect monad =>
    H.Component query input output monad
component = H.mkComponent
    { initialState: const unit
    , render: const render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

about :: ∀ action monad children. MonadEffect monad =>
    HH.ComponentHTML action (about :: Slot___ | children) monad
about = HH.slot (Proxy :: _ "about") unit component unit absurd
