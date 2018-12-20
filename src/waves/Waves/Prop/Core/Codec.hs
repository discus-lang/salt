
-- | Convenience functions for testing round-trip between pretty and parse
module Waves.Prop.Core.Codec where

import qualified Salt.Core.Codec.Text.Lexer       as Lexer
import qualified Salt.Core.Codec.Text.Parser      as Parser
import qualified Salt.Core.Codec.Text.Parser.Base as Parser.Base
import           Salt.Core.Codec.Text.Pretty ()
import qualified Salt.Core.Codec.Text.Token       as Token
import qualified Salt.Core.Transform.MapAnnot as MapAnnot

import qualified Salt.Data.Pretty as Pretty

import qualified Text.Lexer.Inchworm.Char       as IW

import System.IO.Unsafe (unsafePerformIO)

-- | We hope this holds:
-- > dataOfText pTerm . textOfDataPlain = Right
-- > dataOfText pTerm . textOfDataIndent = Right
dataOfText :: MapAnnot.MapAnnot c => Parser.Base.Parser (c a) -> String -> Either (RoundtripError (c ())) (c ())
dataOfText p text = do
  toks <- scanner text
  parser (MapAnnot.stripAnnot <$> p) toks

textOfData :: Pretty.Pretty () a => a -> String
textOfData a = Pretty.render $ Pretty.ppr () a


-- | The kinds of errors that can occur when we try to lex & parse the result of pretty-printing:
data RoundtripError v
  = ErrorNoParse String
  | ErrorLexLeftover TokensNoEq String
  | ErrorParseLeftover v String
  deriving (Eq, Show)

newtype TokensNoEq = Tokens [Token.At Token.Token]
 deriving Show
-- Eq instance for (Either (RoundTripError _) term) is required for round-tripping test on (term), but the (term) is the only important bit
instance Eq TokensNoEq where
 _ == _ = True

scanner :: String -> Either (RoundtripError a) [Token.At Token.Token]
scanner text =
 let fp = "<test>"
     -- Is there a pure lexer?
     (toks,_,strRest) = unsafePerformIO $ IW.scanStringIO text (Lexer.scanner fp)
  in case strRest of
      [] -> return toks
      _  -> Left $ ErrorLexLeftover (Tokens toks) strRest

parser :: Parser.Base.Parser a -> [Token.At Token.Token] -> Either (RoundtripError a) a
parser p toks = case Parser.parse p "<test>" toks of
  Right (v, [])     -> return v
  Right (v,tokRest) -> Left $ ErrorParseLeftover v (show tokRest)
  Left pe           -> Left $ ErrorNoParse (show pe)

