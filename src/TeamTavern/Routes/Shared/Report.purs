module TeamTavern.Routes.Shared.Report where

-- | `reason` is one of `reasons`' values, and `detail` may be blank. `block`
-- | blocks the reported player as well.
type Report = { reason :: String, detail :: String, block :: Boolean }

-- | Whether the viewer had reported the player about the post before, when
-- | nothing new is stored or mailed, though a block asked for is made.
type ReportOutcome = { alreadyReported :: Boolean }

-- | What a report can be about (brief 10), as the report form offers it.
reasons :: Array { value :: String, label :: String }
reasons =
    [ { value: "spam", label: "Spam or advertising" }
    , { value: "harassment", label: "Harassment, hate or threats" }
    , { value: "selling", label: "Selling accounts, boosting or cheats" }
    , { value: "other", label: "Something else" }
    ]
