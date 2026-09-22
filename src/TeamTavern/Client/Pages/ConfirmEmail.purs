module TeamTavern.Client.Pages.ConfirmEmail (confirmEmail) where

import Prelude

import Async (Async)
import Async as Async
import Control.Bind (bindFlipped)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import JSURI (decodeURIComponent)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), buttonLink)
import TeamTavern.Client.Components.Flow (flow, flowLead)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Shared.AccountErrors (somethingWrong)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Routes.Player.ConfirmEmail (ConfirmEmail)
import Type.Proxy (Proxy(..))

-- The link from the confirmation email, opened signed in or not.
data State = Confirming | Confirmed | Expired | Failed

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState Confirming

    Hooks.useLifecycleEffect do
        nonce <- getQueryParam "nonce" <#> bindFlipped decodeURIComponent
        case nonce of
            Nothing -> Hooks.put stateId Expired
            Just nonce' -> do
                result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ ConfirmEmail) { nonce: nonce' }
                Hooks.put stateId case result of
                    Right response -> response # onMatch
                        { noContent: const Confirmed
                        , notFound: const Expired
                        }
                        (const Failed)
                    Left _ -> Failed
        pure Nothing

    Hooks.pure case state of
        Confirming -> flow [ HH.h1_ [ HH.text "Confirming your email…" ] ]
        Confirmed -> flow
            [ HH.h1_ [ HH.text "Your email is confirmed" ]
            , flowLead "TeamTavern can now write to it when someone new fits your posts and before they expire."
            , buttonLink Primary Regular "/" [ HH.text "Go to TeamTavern" ]
            ]
        Expired -> flow
            [ HH.h1_ [ HH.text "This link doesn't work" ]
            , flowLead "It has been used already, or your account's email has changed since it was sent. Your account page sends a new one."
            ]
        Failed -> flow
            [ HH.h1_ [ HH.text "Confirm email" ]
            , flowLead somethingWrong
            ]

confirmEmail :: ∀ action slots left.
    H.ComponentHTML action (confirmEmail :: Slot___ | slots) (Async left)
confirmEmail = HH.slot_ (Proxy :: _ "confirmEmail") unit component unit
