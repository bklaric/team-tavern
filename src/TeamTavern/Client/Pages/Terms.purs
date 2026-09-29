module TeamTavern.Client.Pages.Terms (terms) where

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Pages.Document (adminEmail, document, section)

terms :: ∀ w i. HH.HTML w i
terms =
    document { title: "Terms of use", updated: Just "26 September 2026" }
    [ section "Who we are"
        [ HH.p_
            [ HH.text "TeamTavern (teamtavern.net) is run from Croatia. Contact: "
            , adminEmail, HH.text "."
            ]
        ]
    , section "Age"
        [ HH.p_ [ HH.text "You must be 16 or older to use an account." ] ]
    , section "Your account"
        [ HH.p_ [ HH.text "Keep your sign-in safe. You're responsible for what's posted from your account." ] ]
    , section "What you post"
        [ HH.p_ [ HH.text "Your posts stay yours. You let us show them on the site, and in emails to other players, for as long as they're up. Posts are public: anyone can read them, search engines included." ] ]
    , section "What's not allowed"
        [ HH.ul_
            [ HH.li_ [ HH.text "harassment, threats or hate speech" ]
            , HH.li_ [ HH.text "sexual content" ]
            , HH.li_ [ HH.text "spam or advertising" ]
            , HH.li_ [ HH.text "selling accounts or services" ]
            , HH.li_ [ HH.text "pretending to be someone else" ]
            , HH.li_ [ HH.text "posting someone else's personal information" ]
            , HH.li_ [ HH.text "anything illegal" ]
            ]
        ]
    , section "Moderation"
        [ HH.p_ [ HH.text "Every report is read by hand. We may remove posts or messages, or close accounts, that break these rules, and we'll email you the reason. If you disagree, reply to that email." ] ]
    , section "No guarantees"
        [ HH.p_ [ HH.text "TeamTavern is provided as it is, and it may change or stop. We aren't responsible for other players or for what happens when you play together, as far as the law allows." ] ]
    , section "Changes"
        [ HH.p_ [ HH.text "When these terms change, we update them here, and we email you about significant changes." ] ]
    , section "Law"
        [ HH.p_ [ HH.text "Croatian law applies. If you live in the EU, you keep the consumer rights of your country." ] ]
    ]
