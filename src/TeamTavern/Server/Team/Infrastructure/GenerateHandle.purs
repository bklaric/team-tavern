module TeamTavern.Server.Team.Infrastructure.GenerateHandle (Handle, generateHandle) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, fromCodePointArray, toCodePointArray)
import TeamTavern.Server.Team.Infrastructure.ValidateName (Name)
import TeamTavern.Server.Team.Infrastructure.ValidateName as ValidateName

newtype Handle = Handle String

aPoint :: CodePoint
aPoint = codePointFromChar 'a'

aCapPoint :: CodePoint
aCapPoint = codePointFromChar 'A'

zPoint :: CodePoint
zPoint = codePointFromChar 'z'

zCapPoint :: CodePoint
zCapPoint = codePointFromChar 'Z'

onePoint :: CodePoint
onePoint = codePointFromChar '1'

ninePoint :: CodePoint
ninePoint = codePointFromChar '9'

dashPoint :: CodePoint
dashPoint = codePointFromChar '-'

underscorePoint :: CodePoint
underscorePoint = codePointFromChar '_'

apostrophePoint :: CodePoint
apostrophePoint = codePointFromChar '\''

isLetter :: CodePoint -> Boolean
isLetter point = (aPoint <= point && point <= zPoint) || (aCapPoint <= point && point <= zCapPoint)

isNumber :: CodePoint -> Boolean
isNumber point = onePoint <= point && point <= ninePoint

generateHandle :: Name -> Handle
generateHandle name =
    ValidateName.toString name
    # toCodePointArray
    <#> (\point ->
        if isLetter point || isNumber point || point == dashPoint || point == underscorePoint
        then Just point
        else if point == apostrophePoint
        then Nothing
        else Just underscorePoint)
    # Array.catMaybes
    # fromCodePointArray
    # Handle
