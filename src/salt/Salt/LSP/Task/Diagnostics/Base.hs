
module Salt.LSP.Task.Diagnostics.Base where
import Salt.Data.Location

-- | Severity of a diagnostic.
data Severity
        = SeverityError
        | SeverityWarning
        | SeverityInformation
        | SeverityHint
        deriving (Eq, Show)


-- | Get the numeric code of a severity level.
codeOfSeverity :: Severity -> Int
codeOfSeverity sv
 = case sv of
        SeverityError           -> 1
        SeverityWarning         -> 2
        SeverityInformation     -> 3
        SeverityHint            -> 4


-- | Munge a range to work with VSCode
--   The ranges that Inchworm attaches to tokens are from the first character
--   to the last character in the token. VSCode wants from first character
--   to just after where to put the red wiggle.
mungeRangeForVSCode :: Range Location -> Range Location
mungeRangeForVSCode range'@(Range locStart locEnd)
 | Location lStart cStart <- locStart
 , Location lEnd   cEnd   <- locEnd
 , lStart == lEnd, cEnd - cStart > 1
 = Range locStart (Location lEnd (cEnd + 1))

 | otherwise = range'


