
module Salt.Core.Codec.Text.Parser.Module where
import Salt.Core.Codec.Text.Parser.Decl
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp
import qualified Text.Parsec            as P


-- | Parser for a module.
pModule :: Parser (Module RL)
pModule
 = do   decls   <- P.many pDecl
        pTok KMetaEnd
        return  $ Module decls

