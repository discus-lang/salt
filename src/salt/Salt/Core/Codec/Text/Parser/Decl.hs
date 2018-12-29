
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
 [ do   -- 'type' Var TypeParams* ':' Type '=' Type
        loc <- getLocation
        pTok KType
        nType   <- pVar
        tps     <- P.many pTypeParams
        pTok KColon
        kResult <- pType
        pTok KEquals
        tBody   <- pType
        return  $ DType $ DeclType
                { declAnnot       = loc
                , declName        = nType
                , declParams      = tps
                , declKindResult  = kResult
                , declBody        = tBody }


 , do   -- 'term' Var TermParams* (':' Type)? '=' Term
        loc <- getLocation
        pTok KTerm
        nTerm   <- pVar
        mps     <- P.many pTermParams
        pTok KColon
        tsResult <- pTypesResult
        pTok KEquals
        mBody   <- pTerm
        return  $  DTerm $ DeclTerm
                { declAnnot       = loc
                , declName        = nTerm
                , declParams      = mps
                , declTypesResult = tsResult
                , declBody        = mBody }


 , do   -- 'test' 'kind'   (Name '=')? Type
        -- 'test' 'type'   (Name '=')? Term
        -- 'test' 'eval'   (Name '=')? Term
        -- 'test' 'exec'   (Name '=')? Term
        -- 'test' 'assert' (Name '=')? Term
        loc <- getLocation
        pTok KTest

        nMode   <- P.choice
                [ do    n <- pVar
                        unless (elem n  [ "kind",      "type"
                                        , "eval'type", "eval'term", "eval"
                                        , "exec",      "assert"])
                         $ P.unexpected "test mode"

                        return n

                , do    -- 'type' is both a test specifier and a keyword,
                        -- so we need to match for it explicitly.
                        pTok KType
                        return "type" ]
                <?> "test mode specifier"

        mName   <- P.choice
                [  P.try $ do n <- pVar; pTok KEquals; return $ Just n
                ,  return Nothing ]

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

         , do   guard $ (nMode == "eval'type")
                tBody   <- pType
                return  $ DTest $ DeclTestEvalType
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestType  = tBody }

         , do   guard $ (nMode == "eval'term") || (nMode == "eval")
                mBody   <- pTerm
                return  $ DTest $ DeclTestEvalTerm
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestTerm  = mBody }

         , do   guard $ nMode == "exec"
                mBody   <- pTerm
                return  $ DTest $ DeclTestExec
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
 ]
 <?> "a declaration"

