
module Salt.Core.Codec.Text.Parser.Decl where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Term
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                              ((<?>))
import Data.Maybe
import qualified Text.Parsec                    as P


-- | Parser for a top-level declaration.
pDecl :: Parser (Decl Location)
pDecl
 = P.choice
 [ do   -- 'type' Var TypeParams* ':' Type '=' Type
        loc <- getLocation
        pTok KType
        nType <- pVar           <?> "a name for the type"
        tps   <- P.many
                (pTypeParams    <?> "some parameters, or a ':' to give the result kind")
        pTok KColon             <?> "more parameters, or a ':' to give the result kind"
        kResult <- pType        <?> "the result kind"
        pTok KEquals            <?> "a '=' to start the body"
        tBody   <- pType        <?> "the body"
        return  $ DType $ DeclType
                { declAnnot       = loc
                , declName        = nType
                , declParams      = tps
                , declKindResult  = kResult
                , declBody        = tBody }


 , do   -- 'term' Var TermParams* (':' Type)? '=' Term
        loc <- getLocation
        pTok KTerm
        nTerm   <- pVar          <?> "a name for the term"
        mps     <- P.many
                (pTermParams     <?> "some parameters, or a result type annotation")
        pTok KColon              <?> "more parameters, or a ':' to start the result type"
        tsResult <- pTypesResult <?> "some result types"
        pTok KEquals             <?> "a '=' to start the body"
        mBody   <- pTerm         <?> "the body"
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

        -- Lookahead to get the test mode.
        --  If we don't recognize it then we want the error to be on this token,
        --  not the one after.
        nMode
         <- P.lookAhead
                (P.choice [ pVar, do pTok KType; return "type"])
         <?> "the test mode"

        -- Check this is a valid test mode.
        unless (elem nMode
                [ "kind",      "type"
                , "eval'type", "eval'term", "eval"
                , "exec",      "assert"])
         $ P.unexpected "test mode"

        -- 'type' is both a test specifier and a keyword,
        -- so we need to match for it explicitly.
        P.choice
         [ do   pVar
         , do   pTok KType; return "type" ]

        -- See if this is a named test, or a bare type/term.
        --  We know it's a named test if we can see the '=' after the name.
        mName   <- P.choice
                [  P.try $ do n <- pVar; pTok KEquals; return $ Just n
                ,  return Nothing ]

        -- What we parse next depends on the test mode.
        P.choice
         [ do   guard $ nMode == "kind"
                tType   <-  pType
                        <?> if isJust mName
                                then "the type to take the kind of"
                                else "a test name, or the type to take the kind of"
                return  $ DTest $ DeclTestKind
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestType  = tType }

         , do   guard $ nMode == "type"
                mTerm   <- pTerm
                        <?> if isJust mName
                                then "the term to take the type of"
                                else "a test name, or the term to take the type of"
                return  $ DTest $ DeclTestType
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestTerm  = mTerm }

         , do   guard $ (nMode == "eval'type")
                tBody   <- pType
                        <?> if isJust mName
                                then "the type to evaluate"
                                else "a test name, or the type to evaluate"
                return  $ DTest $ DeclTestEvalType
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestType  = tBody }

         , do   guard $ (nMode == "eval'term") || (nMode == "eval")
                mBody   <- pTerm
                        <?> if isJust mName
                                then "the term to evaluate"
                                else "a test name, or the term to evaluate"
                return  $ DTest $ DeclTestEvalTerm
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestTerm  = mBody }

         , do   guard $ nMode == "exec"
                mBody   <- pTerm
                        <?> if isJust mName
                                then "the term to execute"
                                else "a test name, or the term to execute"
                return  $ DTest $ DeclTestExec
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestBody  = mBody }

         , do   guard $ nMode == "assert"
                mBody   <- pTerm
                        <?> if isJust mName
                                then "the term you hope is true"
                                else "a test name, or the term you hope is true"
                return  $ DTest $ DeclTestAssert
                        { declAnnot     = loc
                        , declTestName  = mName
                        , declTestBody  = mBody }
         ]
 ]
