
module Salt.Core.Codec.Text.Parser.Params where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


-- | Parser for some term parameters.
pTermParams :: Parser (TermParams RL)
pTermParams
 = pMPAnn $ P.choice
 [ do   -- '@' '[' (Var ':' Type)+ ']'
        pTok KAt
        bts     <- pSquared $ flip P.sepEndBy1 (pTok KComma)
                $  do   b <- pBind  <?> "a binder for the parameter"
                        pTok KColon <?> "a ':' to give the kind of the parameter"
                        t <- pType  <?> "the kind of the parameter"
                        return (b, t)
        return  $ MPTypes bts

 , do   -- '[' (Var ':' Type)* ']'
        bts     <- pSquared $ flip P.sepEndBy  (pTok KComma)
                $  do   b <- pBind  <?> "a binder for the parameter"
                        pTok KColon <?> "a ':' to give the type of the parameter"
                        t <- pType  <?> "the type of the parameter"
                        return (b, t)
        return  $ MPTerms bts
 ]


