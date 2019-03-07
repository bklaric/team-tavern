module TeamTavern.Client.EditProfile (Query, Slot, editProfile) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Profile.Update.SendResponse as Update
import TeamTavern.Profile.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init String String send
    | SummaryInput String send
    | Update Event send

type State' =
    { nickname :: String
    , handle :: String
    , summary :: String
    , summaryError :: Boolean
    , otherError :: Boolean
    }

data State
    = Empty
    | Profile State'
    | NotFound
    | Error

type Slot = H.Slot Query Void

render :: forall left. State -> H.ComponentHTML Query () (Async left)
render Empty = HH.div_ []
render (Profile { summary, summaryError, otherError }) = HH.form
    [ HE.onSubmit $ HE.input Update ]
    [ HH.h2_ [ HH.text $ "Edit profile" ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass summaryError, HP.for "summary" ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HP.class_ $ errorClass summaryError
            , HE.onValueInput $ HE.input SummaryInput
            , HP.value summary
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text
                "The summary cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ summary == ""
        ]
        [ HH.text "Save changes" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]
render NotFound = HH.p_ [ HH.text "Profile could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadProfile :: forall left. String -> String -> Async left State
loadProfile nickname handle = Async.unify do
    response <- Fetch.fetch_
        ("/api/games/" <> handle <> "/profiles/" <> nickname)
        # lmap (const Error)
    { summary } :: View.OkContent <-
        case FetchRes.status response of
        200 -> FetchRes.text response
            >>= JsonAsync.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    pure $ Profile
        { nickname
        , handle
        , summary
        , summaryError: false
        , otherError: false
        }

updateProfile :: forall left. State' -> Async left (Maybe State')
updateProfile state @ { nickname, handle, summary } =
    Async.unify do
    response <- Fetch.fetch
        ("/api/games/" <> handle <> "/profiles/" <> nickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { summary }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidSummary: const $ state { summaryError = true } }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

eval :: forall left. Query ~> H.HalogenM State Query () Void (Async left)
eval (Init nickname' handle' send) = do
    -- playerId <- H.liftEffect getPlayerId
    -- case playerId of
    --     Just id -> do
    --         H.liftEffect $ when (nickname /= nickname') $ navigate_ "/"
    --         state <- H.lift $ loadProfile nickname' handle'
    --         H.put state
    --     Nothing -> H.liftEffect $ navigate_ "/"
    pure send
eval (SummaryInput summary send) = do
    H.modify_ $ case _ of
        Profile state -> Profile $ state { summary = summary }
        state -> state
    pure send
eval (Update event send) = do
    H.liftEffect $ preventDefault event
    state <- H.get
    case state of
        Profile state' -> do
            newState <- H.lift $ updateProfile $ state'
                { summaryError = false
                , otherError   = false
                }
            case newState of
                Nothing -> H.liftEffect $ navigate_ $
                    "/players/" <> state'.nickname
                Just newState' -> H.put $ Profile newState'
        _ -> pure unit
    pure send

component :: forall input left.
    String -> String -> H.Component HH.HTML Query input Void (Async left)
component nickname handle = H.component
    { initialState: const Empty
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action $ Init nickname handle
    , finalizer: Nothing
    }

editProfile
    :: forall query children left
    .  String
    -> String
    -> HH.ComponentHTML query (editProfile :: Slot Unit | children) (Async left)
editProfile nickname handle =
    HH.slot
        (SProxy :: SProxy "editProfile")
        unit
        (component nickname handle)
        unit
        absurd
