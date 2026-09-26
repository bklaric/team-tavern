module TeamTavern.Client.Pages.About (about) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Pages.Document (document)

about :: ∀ w i. HH.HTML w i
about =
    document { title: "About TeamTavern", updated: Nothing }
    [ HH.p_ [ HH.text $ "TeamTavern started in 2018. Public matchmaking had worn us down: teams were "
        <> "disorganised and hardly anyone talked. We wanted to find players at our skill level, but had "
        <> "no idea where to look. Without friends to play with, we were also missing out on co-op games. "
        <> "We figured we weren't the only ones, so we made a place to find people to play online games with."
        ]
    , HH.p_ [ HH.text $ "That's still what it is. Players, groups and communities post what they're "
        <> "looking for in a game: rank, role, region, language, and when they're online. You can browse "
        <> "the posts and message anyone who fits. Or post once, and TeamTavern emails you when someone "
        <> "new fits your post."
        ]
    , HH.p_ [ HH.text $ "Posts stay up for 30 days, and community posts for 90. After that they expire, "
        <> "and you can renew them."
        ]
    , HH.p_ [ HH.text "TeamTavern is run from Croatia. Ads keep it free." ]
    ]
