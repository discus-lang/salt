
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
        { errorStart    :: Location
        , errorEnd      :: Location
        , errorMessages :: [P.Message] }


-- | Parse a salt source file from tokens.
parseModule :: [At Token] -> Either [ParseError] (Module Location)
parseModule toks
 = let  toks'   = [ Token.At l k
                  | Token.At l k <- toks
                  , k & \case Token.KComment _ -> False
                              _                -> True]
                ++ [Token.At (Location 0 0) Token.KEnd]

        eResult
         = P.parse
                (do result <- pModule
                    rest <- P.getInput
                    return (result, rest))
                "sourceName" toks'
        
   in   case eResult of
         Left err        
          -> Left [errorOfParseError err]

         Right (xModule, []) 
          -> Right xModule

         Right (_, _xRest)       
          -> Left [ParseError (Location 0 0) (Location 0 0) 
                        [P.Message "parse error at end of input"]]


-- | Extract error information from a Parsec error message.
errorOfParseError :: P.ParseError -> ParseError
errorOfParseError err
 = let  sp      = P.errorPos err
        nLine   = P.sourceLine sp
        nCol    = P.sourceColumn sp
        msgs    = P.errorMessages err
   in   ParseError (Location nLine nCol) (Location nLine nCol) msgs


---------------------------------------------------------------------------------------------------

--ppLocation locStart % text "-" % ppLocation locEnd
--  %% vcat (map ppMessage) msg

ppParseError :: FilePath -> ParseError -> Doc
ppParseError path (ParseError locStart locEnd msgs)
 = vcat ( string path % string ":" %% ppLocation locStart %% text "-" %% ppLocation locEnd
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


ppLocation :: Location -> Doc
ppLocation (Location nLine nCol)
 = int nLine % text ":" % int nCol

ppMessage :: P.Message -> Doc
ppMessage msg
 = case msg of
        P.SysUnExpect str -> text "unexpected" %% string (show str)
        P.UnExpect    str -> text "unexpected" %% string (show str)
        P.Expect      str -> text "expected"   %% string (show str)
        P.Message     str -> text "message"    %% string (show str)

