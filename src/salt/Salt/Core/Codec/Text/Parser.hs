
module Salt.Core.Codec.Text.Parser where
import Salt.Core.Codec.Text.Parser.Decl
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp
import qualified Text.Parsec    as P


-- | Run a parser on a list of located tokens.
parse   :: Parser a -> FilePath -> [At Token]
        -> Either P.ParseError (a, [At Token])
parse p f ts
 = P.parse
        (do result <- p
            rest   <- P.getInput
            return (result, rest))
        f ts


-- | Parser for a module.
pModule :: Parser (Module Location)
pModule
 = do   decls   <- P.many pDecl
        pTok KEnd
        return  $ Module decls


