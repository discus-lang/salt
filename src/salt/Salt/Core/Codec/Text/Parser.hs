
module Salt.Core.Codec.Text.Parser where
import Salt.Core.Codec.Text.Parser.Module
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Exp
import Salt.Core.Codec.Text.Token               as Token
import Salt.Data.Pretty

import Data.Function
import Data.Maybe
-- import Data.List
import qualified Text.Parsec                    as P
-- import qualified Text.Parsec.Pos                as P
import qualified Text.Parsec.Error              as P


---------------------------------------------------------------------------------------------------
-- | Parse error.
data ParseError 
        = ParseError
        { -- | Location of the token we hit the caused the error.
          errorNextRange :: Range Location

          -- | Location of the token previous to the one that caused the error,
          --   if there is one.
        , errorPrevRange :: Maybe (Range Location)

          -- | Parser messages we got from parsec.
        , errorMessages  :: [P.Message] }


-- | Parse a salt source file from tokens.
parseModule :: [At Token] -> Either [ParseError] (Module Location)
parseModule toks
 = let  toks'   = [ Token.At l k
                  | Token.At l k <- toks
                  , k & \case Token.KComment _ -> False
                              _                -> True]
                ++ [Token.At (Range (Location 0 0) (Location 0 0)) Token.KEnd]
                -- TODO: fix location of end token

        eResult
         = P.parse
                (do result <- pModule
                    rest <- P.getInput
                    return (result, rest))
                "sourceName" toks'
        
   in   case eResult of
         Left err
          -> Left [errorOfParseError toks' err]

         Right (xModule, []) 
          -> Right xModule

         -- TODO: real location
         Right (_, _xRest)       
          -> Left [ParseError 
                        (Range (Location 0 0) (Location 0 0))
                        Nothing
                        [P.Message "parse error at end of input"]]


-- | Extract error information from a Parsec error message.
errorOfParseError :: [At Token] -> P.ParseError -> ParseError
errorOfParseError toks err
 = let  sp      = P.errorPos err
        nLine   = P.sourceLine sp
        nCol    = P.sourceColumn sp
        msgs    = P.errorMessages err
   in   ParseError 
          (Range (Location nLine nCol) (Location nLine nCol))
          (findPrevTokenRange err toks)
          msgs


-- | Find the token just before the one that caused the parse error,
--   if there is one.
findPrevTokenRange :: P.ParseError -> [At Token] -> Maybe (Range Location)
findPrevTokenRange err (Token.At range0 _ : k1@(Token.At range1 _) : ks) 
 | Token.Range (Location l c) _ <- range1
 , sp <- P.errorPos err
 , l' <- P.sourceLine sp
 , c' <- P.sourceColumn sp
 , l == l', c == c'
 = Just range0

 | otherwise
 = findPrevTokenRange err (k1 : ks)

findPrevTokenRange _err _ = Nothing


---------------------------------------------------------------------------------------------------
ppParseError :: FilePath -> ParseError -> Doc
ppParseError path (ParseError range _mLocPrev msgs)
 = vcat ( string path 
                % string ":" 
                %% ppRange range
        : text "  parse error" 
                % fromMaybe empty mSysUnexpect
                % fromMaybe empty mUnexpected

        : catMaybes [ mExpect, mMessage])
 
 where  mUnexpected  
         = listToMaybe  [ text ", " % string s 
                        | P.UnExpect   s <- msgs ]

        mSysUnexpect 
         = listToMaybe  [ text ", " % text "unexpected " % string s 
                        | P.SysUnExpect s <- msgs ]

        mExpect      
         = listToMaybe  [ text "  expecting " % string s 
                        | P.Expect s <- msgs ]

        mMessage
         = listToMaybe  [ text "  " % string s 
                        | P.Message s <- msgs]

ppRange :: Range Location -> Doc
ppRange (Range lFirst lFinal)
 = ppLocation lFirst % text "-" % ppLocation lFinal

ppLocation :: Location -> Doc
ppLocation (Location nLine nCol)
 = int (nLine + 1) % text ":" % int (nCol + 1)

ppMessage :: P.Message -> Doc
ppMessage msg
 = case msg of
        P.SysUnExpect str -> text "unexpected" %% string (show str)
        P.UnExpect    str -> text "unexpected" %% string (show str)
        P.Expect      str -> text "expected"   %% string (show str)
        P.Message     str -> text "message"    %% string (show str)

