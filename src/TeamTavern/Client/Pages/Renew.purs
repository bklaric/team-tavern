module TeamTavern.Client.Pages.Renew (renew) where

import Prelude

import Async (Async)
import Async as Async
import Control.Bind (bindFlipped)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import JSURI (decodeURIComponent)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), buttonLink)
import TeamTavern.Client.Components.Flow (flow, flowLead)
import TeamTavern.Client.Pages.Feed.Description (storeDescription)
import TeamTavern.Client.Script.Navigate (navigateReplace_)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Shared.AccountErrors (somethingWrong)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Routes.Post.RenewByNonce (RenewByNonce)
import Type.Proxy (Proxy(..))

-- The Renew link from an email, opened signed in or not. A renewed post opens
-- its game's feed with the post's description, under a note saying so
-- (brief 9); the address is replaced, so Back doesn't come here again.
data State = Renewing | Broken | Failed

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState Renewing

    Hooks.useLifecycleEffect do
        void $ Hooks.fork do
            nonce <- getQueryParam "nonce" <#> bindFlipped decodeURIComponent
            case nonce of
                Nothing -> Hooks.put stateId Broken
                Just nonce' -> do
                    result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ RenewByNonce) { nonce: nonce' }
                    case result of
                        Right response -> response # onMatch
                            { ok: \{ handle, id, type: type_, description } -> do
                                liftEffect $ storeDescription handle type_ description
                                navigateReplace_ $ "/games/" <> handle <> "?renewed=" <> show id
                            , notFound: const $ Hooks.put stateId Broken
                            }
                            (const $ Hooks.put stateId Failed)
                        Left _ -> Hooks.put stateId Failed
        pure Nothing

    Hooks.pure case state of
        Renewing -> flow [ HH.h1_ [ HH.text "Renewing your post…" ] ]
        Broken -> flow
            [ HH.h1_ [ HH.text "This renewal link doesn't work" ]
            , flowLead "The post it renews has been deleted. Your posts are on the home page, signed in."
            , buttonLink Primary Regular "/" [ HH.text "Go to TeamTavern" ]
            ]
        Failed -> flow
            [ HH.h1_ [ HH.text "Renew post" ]
            , flowLead somethingWrong
            ]

renew :: ∀ action slots left.
    H.ComponentHTML action (renew :: Slot___ | slots) (Async left)
renew = HH.slot_ (Proxy :: _ "renew") unit component unit
