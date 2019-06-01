
module Salt.Core.Codec.Text.Parser where
import Salt.Core.Codec.Text.Parser.Module
import Salt.Core.Codec.Text.Parser.Term
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Exp
import Salt.Core.Codec.Text.Token               as Token
import Salt.Core.Codec.Text.Token               (Token)
import Salt.Data.Pretty

import Data.Function
import Data.Maybe
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Error              as P


---------------------------------------------------------------------------------------- Parsing --
-- | Parse a salt source file from tokens.
parseModule :: [At Token] -> Either [ParseError] (Module RL)
parseModule toks
 = let
        -- Get the range of the last token in the file, if there is one.
        rangeLast
         = case reverse toks of
                []                -> Range (Location 0 0) (Location 0 0)
                Token.At r _ : _  -> r

        -- Drop comments before we try to parse the tokens.
        toks'   = [ Token.At l k
                  | Token.At l k <- toks
                  , k & \case Token.KMetaComment _ -> False
                              _                    -> True]
                ++ [Token.At rangeLast Token.KMetaEnd]

        -- Parser context.
        --  We use this to tie the mutually recursive knot through the parser,
        --  so that we don't need mutually recrusive modules.
        ctx    = Context
                { contextParseTerm      = pTerm
                , contextParseTermApp   = pTermApp
                , contextParseTermArg   = pTermArg  }

        -- Parse the module in the prefix,
        -- also returning any remaining unparsable tokens.
        eResult
         = P.runParser
                (do result <- pModule ctx
                    rest   <- P.getInput
                    return (result, rest))
               (State
                { statePrev     = At (Range (Location 0 0) (Location 0 0)) KMetaStart
                , stateOffside  = []
                , stateInjected = []})
                "sourceName"
                toks'

   in   case eResult of
         Left err
          -> Left [errorOfParseError toks' err]

         Right (xModule, [])
          -> Right xModule

         Right (_, _xRest)
          -> Left [ParseError
                        (rangeLast, Nothing)
                        Nothing
                        [P.Message "parse error at end of input"]]


----------------------------------------------------------------------------- Error Construction --
-- | Parse error.
data ParseError
        = ParseError
        { -- | The token we hit the caused the error.
          errorHere     :: (Range Location, Maybe Token)

          -- | The token previous to the one that caused the error, if there is one.
        , errorPrev     :: Maybe (Range Location, Token)

          -- | Parser messages we got from parsec.
        , errorMessages :: [P.Message] }


-- | Extract error information from a Parsec error message.
errorOfParseError :: [At Token] -> P.ParseError -> ParseError
errorOfParseError toks err
 = let  sp      = P.errorPos err

        -- Parsec only produces the single character source location
        -- that is the first char of the token it couldn't parse.
        -- Given this info we retrive the full range of the token from the
        -- original list of tokens.
        nLine   = P.sourceLine sp
        nCol    = P.sourceColumn sp
        lErr    = Location nLine nCol
        rErr    = fromMaybe (Range lErr lErr, Nothing)
                $ findThisTokenRange toks lErr

        -- Find the range of the token just before the one that caused the error.
        rPrev   = findPrevTokenRange err toks

        msgs    = P.errorMessages err
   in   ParseError rErr rPrev msgs


-- | Find the full source range of the token that starts at this location.
--   Parsec only gives us the starting location, but tokens themselves
--   are tagged with full ranges.
findThisTokenRange
        :: [At Token] -> Location
        -> Maybe (Range Location, Maybe Token)
findThisTokenRange [] _ = Nothing
findThisTokenRange (Token.At range@(Token.Range lStart _lEnd) k : ks) lStart'
 | lStart == lStart'    = Just (range, Just k)
 | otherwise            = findThisTokenRange ks lStart'


-- | Find the token just before the one that caused the parse error,
--   if there is one.
findPrevTokenRange
        :: P.ParseError -> [At Token]
        -> Maybe (Range Location, Token)
findPrevTokenRange err (Token.At range0 t0 : k1@(Token.At range1 _) : ks)
 | Token.Range (Location l c) _ <- range1
 , sp <- P.errorPos err
 , l' <- P.sourceLine sp
 , c' <- P.sourceColumn sp
 , l == l', c == c'
 = Just (range0, t0)

 | otherwise
 = findPrevTokenRange err (k1 : ks)

findPrevTokenRange _err _ = Nothing


-------------------------------------------------------------------------------- Pretty Printing --
-- | Pretty print a parse error suitable for display in the console.
ppParseError :: FilePath -> ParseError -> Doc
ppParseError path (ParseError (range, _mTok) _mLocPrev msgs)
 = vcat ( string path
                %  string ":"
                %% ppRange range
        : text "  parse error"
                % fromMaybe empty mSysUnexpect
                % fromMaybe empty mUnexpected

        : catMaybes [ mMessage, mExpect])

 where
        mSysUnexpect
         = listToMaybe  [ text ", " % text "unexpected " % string s
                        | P.SysUnExpect s <- msgs ]

        -- These come from points where we have called 'unexpected'
        -- explicitly in the parser.
        mUnexpected
         = listToMaybe  [ text ", " % string s
                        | P.UnExpect   s <- msgs ]

        -- These come from points where we have called 'fail'
        -- explicitly in the parser.
        mMessage
         = listToMaybe  [ text "  " % string s
                        | P.Message s <- msgs]

        -- These come from where we have use <?> in the parser.
        mExpect
         = listToMaybe  [ text "  expecting " % string s
                        | P.Expect s <- msgs ]


-- | Pretty print a range for display in the console.
ppRange :: Range Location -> Doc
ppRange (Range lFirst lFinal)
 = ppLocation lFirst % text "-" % ppLocation lFinal


-- | Pretty print a location for display in the console.
ppLocation :: Location -> Doc
ppLocation (Location nLine nCol)
 = int (nLine + 1) % text ":" % int (nCol + 1)


-- | Pretty print a parse error message for display in the console.
ppMessage :: P.Message -> Doc
ppMessage msg
 = case msg of
        P.SysUnExpect str -> text "unexpected" %% string (show str)
        P.UnExpect    str -> text "unexpected" %% string (show str)
        P.Expect      str -> text "expected"   %% string (show str)
        P.Message     str -> text "message"    %% string (show str)

