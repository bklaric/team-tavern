module TeamTavern.Client.Pages.Home.CallToAction (Input, Slot, callToAction) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Pages.Home.Wizard (wizard)
import TeamTavern.Client.Pages.Home.Wizard as Wizard
import TeamTavern.Client.Pages.Home.Wizard.Shared as WizardShared

type Input = { signedIn :: Boolean, title :: Maybe String }

type State = { signedIn :: Boolean, title :: Maybe String }

data Action = Receive Input | OpenWizard WizardShared.Ilk

type Slot = H.Slot (Const Void) Void Unit

type Slots slots = (wizard :: Wizard.Slot | slots)

titleOrGeneric :: Maybe String -> String
titleOrGeneric = maybe "esports" identity

article :: Maybe String -> String
article Nothing = "an"
article (Just title) = let
    firstLetter = String.take 1 title # String.toLower
    in
    if Array.any (_ == firstLetter) [ "a", "e", "o", "i", "u" ]
    then "an"
    else "a"

render :: forall slots left.
    State -> HH.ComponentHTML Action (Slots slots) (Async left)
render { signedIn, title } =
    HH.div [ HP.class_ $ HH.ClassName "call-to-action" ]
    [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-content" ]
        [ HH.div [HP.class_ $ HH.ClassName "call-to-action-text" ] $
            [ HH.h1 [ HP.class_ $ HH.ClassName "call-to-action-heading" ]
                [ HH.text $ "Find your " <> titleOrGeneric title <> " teammates" ]
            , HH.p [ HP.class_ $ HH.ClassName "call-to-action-paragraph" ]
                [ HH.text $ """TeamTavern is """ <> article title <> """
                """ <> titleOrGeneric title <> """ team finding platform. Create
                your player or team profile and start finding your new teammates."""]
            ]
            <>
            if signedIn
            then []
            else Array.singleton $
                HH.div [ HP.class_ $ HH.ClassName "call-to-action-buttons" ]
                [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-button-group" ]
                    [ HH.button
                        [ HP.class_ $ HH.ClassName "call-to-action-button"
                        , HE.onClick $ const $ Just $ OpenWizard WizardShared.Player
                        ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user call-to-action-icon" ] []
                        , HH.text "I'm a player"
                        ]
                    , HH.p_ [ HH.text "I want to find a team or other players to play with." ]
                    ]
                , HH.div [ HP.class_ $ HH.ClassName "call-to-action-button-group" ]
                    [ HH.button
                        [ HP.class_ $ HH.ClassName "call-to-action-button"
                        , HE.onClick $ const $ Just $ OpenWizard WizardShared.Team
                        ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-users call-to-action-icon" ] []
                        , HH.text "I have a team"
                        ]
                    , HH.p_ [ HH.text "I want to recruit new members or grow my online community." ]
                    ]
                ]
        ]
    -- , wizard { ilk: WizardShared.Player } (const Nothing)
    ]

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action (Slots slots) output (Async left) Unit
handleAction (Receive input) =
    H.put input
handleAction (OpenWizard ilk) =
    pure unit

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

callToAction :: forall query children left.
    Input -> HH.ComponentHTML query (callToAction :: Slot | children) (Async left)
callToAction input =
    HH.slot (SProxy :: SProxy "callToAction") unit component input absurd
