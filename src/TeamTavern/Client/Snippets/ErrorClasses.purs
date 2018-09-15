module TeamTavern.Client.Snippets.ErrorClasses where

import Halogen (ClassName(..))

errorClass :: Boolean -> ClassName
errorClass hasError = ClassName if hasError then "error" else ""

inputErrorClass :: Boolean -> ClassName
inputErrorClass hasError = ClassName
    if hasError then "input-error" else "hidden"

otherErrorClass :: Boolean -> ClassName
otherErrorClass hasError = ClassName
    if hasError then "other-error" else "hidden"
