module TeamTavern.Client.Pages.Privacy (privacy) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Pages.Document (adminEmail, document, link, section)

privacy :: ∀ w m. MonadEffect m => HH.HTML w (m Unit)
privacy =
    document { title: "Privacy policy", updated: Just "26 September 2026" }
    [ section "Who's responsible"
        [ HH.p_
            [ HH.text "TeamTavern, run from Croatia, is responsible for your data. Contact: "
            , adminEmail, HH.text "."
            ]
        ]
    , section "What we keep and why"
        [ HH.p_ [ HH.text "To sign up you need a nickname, and either an email and a password or a Discord account. Without them we can't make you an account. Everything else is up to you." ]
        , HH.ul_
            [ item "Your account:" "email, nickname, and password (stored hashed), or your Discord id if you sign in with Discord. We keep these to run your account."
            , item "Your profile and posts:" "what you choose to add, such as birthday, country, languages, timezone, game accounts, online hours and post text. Posts are public. They show your age (never your birthday), country, languages, timezone, online hours and game account links. Your contact details show only to signed-in players. Expired posts stay visible until you delete them."
            , item "Messages:" "kept so that you and the other player can read them."
            , item "Reports and blocks:" "kept to keep the site safe. Each report is also emailed to us."
            , item "Emails you send us:" "kept so that we can answer you."
            , HH.li_
                [ HH.strong_ [ HH.text "Emails:" ]
                , HH.text $ " we email you about new messages, posts that fit yours, and posts about to expire. "
                    <> "You can turn each of these off on "
                , link "/account" "Account", HH.text "."
                ]
            ]
        , HH.p_ [ HH.text "We keep account, profile, post and message data to provide the service you signed up for, and reports, blocks and emails to us in our legitimate interest in keeping the site safe and answering you. Ad cookies are used only with your consent." ]
        ]
    , section "Cookies"
        [ HH.ul_
            [ HH.li_ [ HH.text "One cookie keeps you signed in, for up to a year." ]
            , HH.li_ [ HH.text "Your browser also keeps unfinished post drafts on your device." ]
            , HH.li_ [ HH.text "Neither of these needs consent." ]
            , HH.li_ [ HH.text "Ads come from Venatus Media and its partners. They use cookies for personalised ads only if you agree in the consent dialog, which is Google's and remembers your choice for 390 days." ]
            , HH.li_
                [ HH.text "You can change your choice any time with "
                , HH.strong_ [ HH.text "Cookie settings" ]
                , HH.text " at the bottom of the page."
                ]
            ]
        ]
    , section "Who handles data for us"
        [ HH.ul_
            [ item "Hetzner" "(Germany) hosts the site and database."
            , item "Twilio SendGrid" "(US) sends our emails, including the text of messages you receive."
            , item "Google" "(US) runs the consent dialog and our email, which holds reports, what you send us and our database backups."
            , item "Discord" "(US) is involved only if you sign in with it. We receive your Discord id, username and email."
            ]
        , HH.p_ [ HH.text "Transfers to the US rely on the EU–US Data Privacy Framework or the EU's standard contractual clauses." ]
        , HH.p_ [ HH.text "Ads come from Venatus Media (UK) and its partners. They use data for their own purposes, as far as your consent choice allows, and the consent dialog lists every partner. The EU recognises the UK as protecting data adequately." ]
        ]
    , section "How long we keep it"
        [ HH.p_ [ HH.text "We keep your data until you delete your account. Deleting it on the Account page removes everything from the site, and backups are kept for up to 90 days. Reports and emails you sent us stay in our email for as long as we need them to deal with the report or answer you." ] ]
    , section "Your rights"
        [ HH.p_
            [ HH.text "You can ask to see, correct, delete or export your data. You can also restrict its use, and withdraw consent. Email "
            , adminEmail
            , HH.text "."
            ]
        , HH.p_ [ HH.text "You can object at any time to our keeping reports, blocks and emails, which rests on our legitimate interest. We then stop, unless keeping them outweighs your reasons, such as to keep other players safe." ]
        , HH.p_
            [ HH.text "You can also complain to Croatia's data protection agency ("
            , HH.a [ HP.href "https://azop.hr" ] [ HH.text "AZOP" ]
            , HH.text ") or to the authority where you live."
            ]
        ]
    ]
    where
    item lead text = HH.li_ [ HH.strong_ [ HH.text lead ], HH.text $ " " <> text ]
