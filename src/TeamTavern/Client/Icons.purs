-- Lucide icons (ISC licence, lucide.dev) and the Discord mark (simple-icons,
-- CC0), each an inline 24x24 SVG styled by .icon in base.css.
module TeamTavern.Client.Icons
    ( user
    , users
    , castle
    , mic
    , bell
    , mail
    , copy
    , check
    , equalNot
    , fitMark
    , missMark
    , chevronDown
    , chevronUp
    , chevronRight
    , arrowRight
    , clock
    , eye
    , minus
    , trash2
    , info
    , x
    , plus
    , menu
    , externalLink
    , circleAlert
    , pencil
    , refreshCw
    , megaphone
    , partyPopper
    , messageCircle
    , micOff
    , gamepad2
    , ellipsis
    , flag
    , ban
    , arrowLeft
    , search
    , discord
    ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as SE
import Web.HTML.Common (AttrName(..))

-- The class is an attribute, since HP.class_ sets className, which an SVG
-- element doesn't let a script write.
icon :: ∀ w i. Array (HH.HTML w i) -> HH.HTML w i
icon = classedIcon ""

classedIcon :: ∀ w i. String -> Array (HH.HTML w i) -> HH.HTML w i
classedIcon class_ = SE.svg
    [ attr "class" $ if class_ == "" then "icon" else "icon " <> class_
    , attr "viewBox" "0 0 24 24"
    , attr "aria-hidden" "true"
    ]

attr :: ∀ r i. String -> String -> HP.IProp r i
attr name = HP.attr (AttrName name)

user :: ∀ w i. HH.HTML w i
user = icon
    [ SE.path [ attr "d" "M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2" ]
    , SE.circle [ attr "cx" "12", attr "cy" "7", attr "r" "4" ]
    ]

users :: ∀ w i. HH.HTML w i
users = icon
    [ SE.path [ attr "d" "M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2" ]
    , SE.circle [ attr "cx" "9", attr "cy" "7", attr "r" "4" ]
    , SE.path [ attr "d" "M22 21v-2a4 4 0 0 0-3-3.87" ]
    , SE.path [ attr "d" "M16 3.13a4 4 0 0 1 0 7.75" ]
    ]

castle :: ∀ w i. HH.HTML w i
castle = icon
    [ SE.path [ attr "d" "M22 20v-9H2v9a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2Z" ]
    , SE.path [ attr "d" "M18 11V4H6v7" ]
    , SE.path [ attr "d" "M15 22v-4a3 3 0 0 0-3-3a3 3 0 0 0-3 3v4" ]
    , SE.path [ attr "d" "M22 11V9" ]
    , SE.path [ attr "d" "M2 11V9" ]
    , SE.path [ attr "d" "M6 4V2" ]
    , SE.path [ attr "d" "M18 4V2" ]
    , SE.path [ attr "d" "M10 4V2" ]
    , SE.path [ attr "d" "M14 4V2" ]
    ]

mic :: ∀ w i. HH.HTML w i
mic = icon
    [ SE.path [ attr "d" "M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z" ]
    , SE.path [ attr "d" "M19 10v2a7 7 0 0 1-14 0v-2" ]
    , SE.line [ attr "x1" "12", attr "x2" "12", attr "y1" "19", attr "y2" "22" ]
    ]

bell :: ∀ w i. HH.HTML w i
bell = icon
    [ SE.path [ attr "d" "M10.268 21a2 2 0 0 0 3.464 0" ]
    , SE.path [ attr "d" "M3.262 15.326A1 1 0 0 0 4 17h16a1 1 0 0 0 .74-1.673C19.41 13.956 18 12.499 18 8A6 6 0 0 0 6 8c0 4.499-1.411 5.956-2.738 7.326" ]
    ]

mail :: ∀ w i. HH.HTML w i
mail = icon
    [ SE.rect [ attr "width" "20", attr "height" "16", attr "x" "2", attr "y" "4", attr "rx" "2" ]
    , SE.path [ attr "d" "m22 7-8.97 5.7a1.94 1.94 0 0 1-2.06 0L2 7" ]
    ]

copy :: ∀ w i. HH.HTML w i
copy = icon
    [ SE.rect [ attr "width" "14", attr "height" "14", attr "x" "8", attr "y" "8", attr "rx" "2", attr "ry" "2" ]
    , SE.path [ attr "d" "M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2" ]
    ]

check :: ∀ w i. HH.HTML w i
check = icon checkShapes

checkShapes :: ∀ w i. Array (HH.HTML w i)
checkShapes =
    [ SE.path [ attr "d" "M20 6 9 17l-5-5" ]
    ]

equalNot :: ∀ w i. HH.HTML w i
equalNot = icon equalNotShapes

equalNotShapes :: ∀ w i. Array (HH.HTML w i)
equalNotShapes =
    [ SE.line [ attr "x1" "5", attr "x2" "19", attr "y1" "9", attr "y2" "9" ]
    , SE.line [ attr "x1" "5", attr "x2" "19", attr "y1" "15", attr "y2" "15" ]
    , SE.line [ attr "x1" "19", attr "x2" "5", attr "y1" "5", attr "y2" "19" ]
    ]

-- The marks a card's fact carries: check where it fits, ≠ where it doesn't.
fitMark :: ∀ w i. HH.HTML w i
fitMark = classedIcon "fact-mark" checkShapes

missMark :: ∀ w i. HH.HTML w i
missMark = classedIcon "fact-mark" equalNotShapes

chevronDown :: ∀ w i. HH.HTML w i
chevronDown = icon
    [ SE.path [ attr "d" "m6 9 6 6 6-6" ]
    ]

chevronUp :: ∀ w i. HH.HTML w i
chevronUp = icon
    [ SE.path [ attr "d" "m18 15-6-6-6 6" ]
    ]

chevronRight :: ∀ w i. HH.HTML w i
chevronRight = icon
    [ SE.path [ attr "d" "m9 18 6-6-6-6" ]
    ]

arrowRight :: ∀ w i. HH.HTML w i
arrowRight = icon
    [ SE.path [ attr "d" "M5 12h14" ]
    , SE.path [ attr "d" "m12 5 7 7-7 7" ]
    ]

clock :: ∀ w i. HH.HTML w i
clock = icon
    [ SE.circle [ attr "cx" "12", attr "cy" "12", attr "r" "10" ]
    , SE.polyline [ attr "points" "12 6 12 12 16 14" ]
    ]

eye :: ∀ w i. HH.HTML w i
eye = icon
    [ SE.path [ attr "d" "M2.062 12.348a1 1 0 0 1 0-.696 10.75 10.75 0 0 1 19.876 0 1 1 0 0 1 0 .696 10.75 10.75 0 0 1-19.876 0" ]
    , SE.circle [ attr "cx" "12", attr "cy" "12", attr "r" "3" ]
    ]

minus :: ∀ w i. HH.HTML w i
minus = icon
    [ SE.path [ attr "d" "M5 12h14" ]
    ]

trash2 :: ∀ w i. HH.HTML w i
trash2 = icon
    [ SE.path [ attr "d" "M3 6h18" ]
    , SE.path [ attr "d" "M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6" ]
    , SE.path [ attr "d" "M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2" ]
    , SE.line [ attr "x1" "10", attr "x2" "10", attr "y1" "11", attr "y2" "17" ]
    , SE.line [ attr "x1" "14", attr "x2" "14", attr "y1" "11", attr "y2" "17" ]
    ]

info :: ∀ w i. HH.HTML w i
info = icon
    [ SE.circle [ attr "cx" "12", attr "cy" "12", attr "r" "10" ]
    , SE.path [ attr "d" "M12 16v-4" ]
    , SE.path [ attr "d" "M12 8h.01" ]
    ]

x :: ∀ w i. HH.HTML w i
x = icon
    [ SE.path [ attr "d" "M18 6 6 18" ]
    , SE.path [ attr "d" "m6 6 12 12" ]
    ]

plus :: ∀ w i. HH.HTML w i
plus = icon
    [ SE.path [ attr "d" "M5 12h14" ]
    , SE.path [ attr "d" "M12 5v14" ]
    ]

menu :: ∀ w i. HH.HTML w i
menu = icon
    [ SE.line [ attr "x1" "4", attr "x2" "20", attr "y1" "12", attr "y2" "12" ]
    , SE.line [ attr "x1" "4", attr "x2" "20", attr "y1" "6", attr "y2" "6" ]
    , SE.line [ attr "x1" "4", attr "x2" "20", attr "y1" "18", attr "y2" "18" ]
    ]

externalLink :: ∀ w i. HH.HTML w i
externalLink = icon
    [ SE.path [ attr "d" "M15 3h6v6" ]
    , SE.path [ attr "d" "M10 14 21 3" ]
    , SE.path [ attr "d" "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6" ]
    ]

circleAlert :: ∀ w i. HH.HTML w i
circleAlert = icon
    [ SE.circle [ attr "cx" "12", attr "cy" "12", attr "r" "10" ]
    , SE.line [ attr "x1" "12", attr "x2" "12", attr "y1" "8", attr "y2" "12" ]
    , SE.line [ attr "x1" "12", attr "x2" "12.01", attr "y1" "16", attr "y2" "16" ]
    ]

pencil :: ∀ w i. HH.HTML w i
pencil = icon
    [ SE.path [ attr "d" "M21.174 6.812a1 1 0 0 0-3.986-3.987L3.842 16.174a2 2 0 0 0-.5.83l-1.321 4.352a.5.5 0 0 0 .623.622l4.353-1.32a2 2 0 0 0 .83-.497z" ]
    , SE.path [ attr "d" "m15 5 4 4" ]
    ]

refreshCw :: ∀ w i. HH.HTML w i
refreshCw = icon
    [ SE.path [ attr "d" "M3 12a9 9 0 0 1 9-9 9.75 9.75 0 0 1 6.74 2.74L21 8" ]
    , SE.path [ attr "d" "M21 3v5h-5" ]
    , SE.path [ attr "d" "M21 12a9 9 0 0 1-9 9 9.75 9.75 0 0 1-6.74-2.74L3 16" ]
    , SE.path [ attr "d" "M8 16H3v5" ]
    ]

megaphone :: ∀ w i. HH.HTML w i
megaphone = icon
    [ SE.path [ attr "d" "M11 6a13 13 0 0 0 8.4-2.8A1 1 0 0 1 21 4v12a1 1 0 0 1-1.6.8A13 13 0 0 0 11 14H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2z" ]
    , SE.path [ attr "d" "M6 14a12 12 0 0 0 2.4 7.2 2 2 0 0 0 3.2-2.4A8 8 0 0 1 10 14" ]
    , SE.path [ attr "d" "M8 6v8" ]
    ]

partyPopper :: ∀ w i. HH.HTML w i
partyPopper = icon
    [ SE.path [ attr "d" "M5.8 11.3 2 22l10.7-3.79" ]
    , SE.path [ attr "d" "M4 3h.01" ]
    , SE.path [ attr "d" "M22 8h.01" ]
    , SE.path [ attr "d" "M15 2h.01" ]
    , SE.path [ attr "d" "M22 20h.01" ]
    , SE.path [ attr "d" "m22 2-2.24.75a2.9 2.9 0 0 0-1.96 3.12c.1.86-.57 1.63-1.45 1.63h-.38c-.86 0-1.6.6-1.76 1.44L14 10" ]
    , SE.path [ attr "d" "m22 13-.82-.33c-.86-.34-1.82.2-1.98 1.11c-.11.7-.72 1.22-1.43 1.22H17" ]
    , SE.path [ attr "d" "m11 2 .33.82c.34.86-.2 1.82-1.11 1.98C9.52 4.9 9 5.52 9 6.23V7" ]
    , SE.path [ attr "d" "M11 13c1.93 1.93 2.83 4.17 2 5-.83.83-3.07-.07-5-2-1.93-1.93-2.83-4.17-2-5 .83-.83 3.07.07 5 2Z" ]
    ]

messageCircle :: ∀ w i. HH.HTML w i
messageCircle = icon
    [ SE.path [ attr "d" "M7.9 20A9 9 0 1 0 4 16.1L2 22Z" ]
    ]

micOff :: ∀ w i. HH.HTML w i
micOff = icon
    [ SE.line [ attr "x1" "2", attr "x2" "22", attr "y1" "2", attr "y2" "22" ]
    , SE.path [ attr "d" "M18.89 13.23A7.12 7.12 0 0 0 19 12v-2" ]
    , SE.path [ attr "d" "M5 10v2a7 7 0 0 0 12 5" ]
    , SE.path [ attr "d" "M15 9.34V5a3 3 0 0 0-5.68-1.33" ]
    , SE.path [ attr "d" "M9 9v3a3 3 0 0 0 5.12 2.12" ]
    , SE.line [ attr "x1" "12", attr "x2" "12", attr "y1" "19", attr "y2" "22" ]
    ]

gamepad2 :: ∀ w i. HH.HTML w i
gamepad2 = icon
    [ SE.line [ attr "x1" "6", attr "x2" "10", attr "y1" "11", attr "y2" "11" ]
    , SE.line [ attr "x1" "8", attr "x2" "8", attr "y1" "9", attr "y2" "13" ]
    , SE.line [ attr "x1" "15", attr "x2" "15.01", attr "y1" "12", attr "y2" "12" ]
    , SE.line [ attr "x1" "18", attr "x2" "18.01", attr "y1" "10", attr "y2" "10" ]
    , SE.path [ attr "d" "M17.32 5H6.68a4 4 0 0 0-3.978 3.59c-.006.052-.01.101-.017.152C2.604 9.416 2 14.456 2 16a3 3 0 0 0 3 3c1 0 1.5-.5 2-1l1.414-1.414A2 2 0 0 1 9.828 16h4.344a2 2 0 0 1 1.414.586L17 18c.5.5 1 1 2 1a3 3 0 0 0 3-3c0-1.545-.604-6.584-.685-7.258-.007-.05-.011-.1-.017-.151A4 4 0 0 0 17.32 5z" ]
    ]

ellipsis :: ∀ w i. HH.HTML w i
ellipsis = icon
    [ SE.circle [ attr "cx" "12", attr "cy" "12", attr "r" "1" ]
    , SE.circle [ attr "cx" "19", attr "cy" "12", attr "r" "1" ]
    , SE.circle [ attr "cx" "5", attr "cy" "12", attr "r" "1" ]
    ]

flag :: ∀ w i. HH.HTML w i
flag = icon
    [ SE.path [ attr "d" "M4 22V4a1 1 0 0 1 .4-.8A6 6 0 0 1 8 2c3 0 5 2 7.333 2q2 0 3.067-.8A1 1 0 0 1 20 4v10a1 1 0 0 1-.4.8A6 6 0 0 1 16 16c-3 0-5-2-8-2a6 6 0 0 0-4 1.528" ]
    ]

ban :: ∀ w i. HH.HTML w i
ban = icon
    [ SE.circle [ attr "cx" "12", attr "cy" "12", attr "r" "10" ]
    , SE.path [ attr "d" "m4.9 4.9 14.2 14.2" ]
    ]

arrowLeft :: ∀ w i. HH.HTML w i
arrowLeft = icon
    [ SE.path [ attr "d" "m12 19-7-7 7-7" ]
    , SE.path [ attr "d" "M19 12H5" ]
    ]

search :: ∀ w i. HH.HTML w i
search = icon
    [ SE.circle [ attr "cx" "11", attr "cy" "11", attr "r" "8" ]
    , SE.path [ attr "d" "m21 21-4.3-4.3" ]
    ]

discord :: ∀ w i. HH.HTML w i
discord = icon
    [ SE.path [ attr "fill" "currentColor", attr "stroke" "none", attr "d" "M20.317 4.3698a19.7913 19.7913 0 00-4.8851-1.5152.0741.0741 0 00-.0785.0371c-.211.3753-.4447.8648-.6083 1.2495-1.8447-.2762-3.68-.2762-5.4868 0-.1636-.3933-.4058-.8742-.6177-1.2495a.077.077 0 00-.0785-.037 19.7363 19.7363 0 00-4.8852 1.515.0699.0699 0 00-.0321.0277C.5334 9.0458-.319 13.5799.0992 18.0578a.0824.0824 0 00.0312.0561c2.0528 1.5076 4.0413 2.4228 5.9929 3.0294a.0777.0777 0 00.0842-.0276c.4616-.6304.8731-1.2952 1.226-1.9942a.076.076 0 00-.0416-.1057c-.6528-.2476-1.2743-.5495-1.8722-.8923a.077.077 0 01-.0076-.1277c.1258-.0943.2517-.1923.3718-.2914a.0743.0743 0 01.0776-.0105c3.9278 1.7933 8.18 1.7933 12.0614 0a.0739.0739 0 01.0785.0095c.1202.099.246.1981.3728.2924a.077.077 0 01-.0066.1276 12.2986 12.2986 0 01-1.873.8914.0766.0766 0 00-.0407.1067c.3604.698.7719 1.3628 1.225 1.9932a.076.076 0 00.0842.0286c1.961-.6067 3.9495-1.5219 6.0023-3.0294a.077.077 0 00.0313-.0552c.5004-5.177-.8382-9.6739-3.5485-13.6604a.061.061 0 00-.0312-.0286zM8.02 15.3312c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9555-2.4189 2.157-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.9555 2.4189-2.1569 2.4189zm7.9748 0c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9554-2.4189 2.1569-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.946 2.4189-2.1568 2.4189Z" ]
    ]
