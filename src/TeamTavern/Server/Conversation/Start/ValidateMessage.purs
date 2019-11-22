module TeamTavern.Server.Conversation.Start.ValidateMessage where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label as Label
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Domain.NonEmptyText as NonEmptyText
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.Paragraph as Paragraph

newtype Message = Message (Array Paragraph)

type ValidateMessageError errors = Variant
    ( invalidMessage :: { message :: String, errors :: NonEmptyList NonEmptyTextError}
    | errors )

maxLength :: Int
maxLength = 2000

validateMessage :: forall errors.
    String -> Async (ValidateMessageError errors) Message
validateMessage message = NonEmptyText.create maxLength Message message
    # Async.fromValidated
    # Label.labelMap (SProxy :: SProxy "invalidMessage") { message, errors: _ }

toStringArray :: Message -> Array String
toStringArray (Message paragraphs) = paragraphs <#> Paragraph.toString
