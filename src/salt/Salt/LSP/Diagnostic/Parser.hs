
module Salt.LSP.Diagnostic.Parser where
import Salt.Core.Codec.Text.Parser
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import qualified Text.Parsec.Error      as Parsec

import Data.Maybe
import Data.List


-- | A parser diagnostic to send to the client.
data ParserDiagnostic
        = ParserDiagnostic
        { parserDiagnosticRange         :: Range Location
        , parserDiagnosticMessage       :: String }
        deriving Show


-- | Build a diagnostic from a parse error.
diagnosticOfParseError :: [At Token] -> ParseError -> ParserDiagnostic
diagnosticOfParseError _toks (ParseError rangeTokHere mRangeTokPrev msgs)
 = ParserDiagnostic rangeReport sMsg
 where

        -- Range and token that caused the parse error.
        rangeHere       = fst rangeTokHere
        mTokHere        = snd rangeTokHere

        -- Range of the previous token, if we have it.
        mRangePrev      = fmap fst mRangeTokPrev 

        -- Whether the token that caused the problem starts a declaration.
        tokHereIsDecl   = maybe False isDeclStartToken mTokHere

        sMsg     
         = case catMaybes [mIncompleteDecl, mMessage, mUnexpected, mSysUnexpect, mExpect] 
            of  []      -> "Parse error."
                parts   -> intercalate "\n" parts

        mIncompleteDecl 
         | isNothing mRangeIncompleteDecl = Nothing
         | otherwise = Just "Incomplete declaration."

        mUnexpected     
         = listToMaybe  [ "Unexpected " ++ s ++ "." 
                                | Parsec.UnExpect s <- msgs 
                                , isNothing mRangeIncompleteDecl ]
        mSysUnexpect    
         = listToMaybe  [ "Unexpected " ++ s ++ "." 
                                | Parsec.SysUnExpect s <- msgs 
                                , isNothing mRangeIncompleteDecl ]
        mExpect         
         = listToMaybe  [ "Expecting " ++ s  ++ "." | Parsec.Expect s <- msgs ]

        mMessage        
         = listToMaybe  [ s | Parsec.Message s <- msgs ]

        -- If we have detected an incomplete declaration,
        -- then produce the new range to highlight, instead of the token that 
        -- caused the parse error.
        mRangeIncompleteDecl
         | not tokHereIsDecl
         = Nothing

         | Just rangePrev <- mRangePrev
         , Range _ (Location nLine nCol) <- rangePrev
         = let  lHighFirst = Location nLine nCol
                lHighEnd   = Location nLine (nCol + 1)
           in   Just $ Range lHighFirst lHighEnd

         | otherwise   
         = Nothing

        rangeReport
         = mungeRangeForVSCode
         $ fromMaybe rangeHere mRangeIncompleteDecl


-- | Determine if the token that caused the error is one that starts a new
--   declaration. If so the user has probably started typing a declaration, 
--   and is not expecting this to actually parse yet.
isDeclStartToken :: Token -> Bool
isDeclStartToken tok
 = case tok of
        KTerm   -> True
        KType   -> True
        KTest   -> True
        _       -> False


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


-- | Find the source range of the declaration start token just before the
--   given location.
findDeclStartLocation :: [At Token] -> Location -> Maybe (Range Location)
findDeclStartLocation toks loc
 = loop $ reverse toks
 where  
        loop [] = Nothing
        loop (At range@(Range lStart _lEnd) k : toksRest)
         | lStart <= loc 
         , isDeclStartToken k
         = Just range

         | otherwise     
         = loop toksRest


