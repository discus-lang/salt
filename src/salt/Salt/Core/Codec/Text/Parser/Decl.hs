
module Salt.Core.Codec.Text.Parser.Decl where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Term
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                              ((<?>))
import qualified Text.Parsec                    as P


---------------------------------------------------------------------------------------------------
-- | Parser for a top-level declaration.
pDecl :: Parser (Decl Location)
pDecl
 = P.choice
 [ do   -- 'test' Name? 'kind'   ('.' Var)? Type
        -- 'test' Name? 'type'   ('.' Var)? Term
        -- 'test' Name? 'eval'   ('.' Var)? Term
        -- 'test' Name? 'assert' ('.' Var)? Term
        loc <- getLocation
        pTok KTest

        nMode   <- P.choice
                [ do    n <- pVar
                        (guard $ elem n ["kind", "type", "eval", "assert"])
                        return n

                , do    -- 'type' is both a test specifier and a keyword,
                        -- so we need to match for it explicitly.
                        pTok KType
                        return "type" ]
                <?> "test mode specifier"

        mName <- P.optionMaybe $ do pTok KDot; pVar

        P.choice
         [ do   guard $ nMode == "kind"
                tType   <- pType
                return  $ DTest $ DeclTestKind
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestType  = tType }

         , do   guard $ nMode == "type"
                mTerm   <- pTerm
                return  $ DTest $ DeclTestType
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestTerm  = mTerm }

         , do   guard $ nMode == "eval"
                mBody   <- pTerm
                return  $ DTest $ DeclTestEval
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestBody  = mBody }

         , do   guard $ nMode == "assert"
                mBody   <- pTerm
                return  $ DTest $ DeclTestAssert
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestBody  = mBody }
         ]
         <?> "a test declaration"

 , do   -- 'term' Var TermParams* (':' Type)? '=' Term
        loc <- getLocation
        pTok KTerm
        n       <- pVar
        mps     <- P.many pTermParams
        pTok KColon
        ts      <- pTypesResult
        pTok KEquals
        mBody   <- pTerm
        return  $  DTerm $ DeclTerm
                { declAnnot       = loc
                , declName        = n
                , declParams      = mps
                , declTypesResult = ts
                , declBody        = mBody }
 ]
 <?> "a declaration"

