module TeamTavern.Client.Pages.Contact (contact) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import TeamTavern.Client.Pages.Document (adminEmail, document, link)

contact :: ∀ w m. MonadEffect m => HH.HTML w (m Unit)
contact =
    document { title: "Contact", updated: Nothing }
    [ HH.p_
        [ HH.text "Email ", adminEmail
        , HH.text $ " for questions, problems, or requests about your data. "
            <> "We answer in English or Croatian."
        ]
    , HH.p_ [ HH.text $ "Authorities, the European Commission and the European Board for Digital Services "
        <> "can reach us at the same address, in English or Croatian."
        ]
    , HH.ul_
        [ HH.li_
            [ HH.strong_ [ HH.text "To report a post or a player," ]
            , HH.text $ " use \"Report this post\" on the post or \"Report\" in your conversation. "
                <> "You need to be signed in."
            ]
        , HH.li_
            [ HH.strong_ [ HH.text "To report illegal content without an account," ]
            , HH.text $ " email the address above with a link to the post, why you believe it's illegal, "
                <> "and your name and email address. We'll confirm we got it and tell you what we decided."
            ]
        , HH.li_
            [ HH.strong_ [ HH.text "To delete your account," ]
            , HH.text " go to ", link "/account" "Account", HH.text " and choose Delete account."
            ]
        ]
    ]
