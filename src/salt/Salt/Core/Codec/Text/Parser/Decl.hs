
module Salt.Core.Codec.Text.Parser.Decl where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Term
import Salt.Core.Codec.Text.Parser.Params
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                              ((<?>))
import Data.Maybe
import qualified Text.Parsec                    as P


------------------------------------------------------------------------------------------- Decl --
-- | Parser for a top-level declaration.
pDecl :: Context -> Parser (Decl RL)
pDecl ctx
 = P.choice
 [ pDeclType, pDeclTerm ctx, pDeclTest ctx, pDeclEmit ctx ]


------------------------------------------------------------------------------------------- Type --
-- | Parser for a type declaration.
pDeclType :: Parser (Decl RL)
pDeclType
 = do   -- 'type' Var TypeParams* ':' Type '=' Type
        pTok KType
        (rName, nName)
          <- pRanged (pVar <?> "a name for the type")
        tps     <- P.many  (pTypeParams
                                <?> "some parameters, or a ':' to give the result kind")
        pTok KColon             <?> "more parameters, or a ':' to give the result kind"
        kResult <- pType        <?> "the result kind"
        pTok KEquals            <?> "a '=' to start the body"
        tBody   <- pType        <?> "the body"
        return  $ DType $ DeclType
                { declAnnot       = rName
                , declName        = nName
                , declParams      = tps
                , declKindResult  = kResult
                , declBody        = tBody }


------------------------------------------------------------------------------------------- Term --
-- | Parser for a term declaration.
pDeclTerm :: Context -> Parser (Decl RL)
pDeclTerm ctx
 = P.choice
 [ do   -- 'term' Var TermParams* ':' Type '=' Term
        pTok KTerm
        (rName, nName)
         <- pRanged (pVar       <?> "a name for the term")
        mps     <- P.many (pTermParams
                                <?> "some parameters, or a result type annotation")
        pTok KColon             <?> "more parameters, or a ':' to start the result type"
        tsRes   <- pTypesResult <?> "some result types"
        pTok KEquals            <?> "a '=' to start the body"
        mBody   <- pTerm ctx    <?> "the body"
        return  $  DTerm $ DeclTerm
                { declAnnot       = rName
                , declTermMode    = DeclTermModePlain
                , declName        = nName
                , declParams      = mps
                , declTypesResult = tsRes
                , declBody        = mBody }

 , do   -- 'proc' Var TermParams* ':' Type '=' Proc
        pTok KProc
        (rName, nName)
         <- pRanged (pVar       <?> "a name for the proc")
        mps     <- P.many (pTermParams
                                <?> "some parameters, or a result type annotation")
        pTok KColon             <?> "more parameters, or a ':' to start the result type"
        tsRes   <- pTypesResult <?> "some result types"
        pTok KEquals            <?> "a '=' to start the body"
        mBody   <- pTerm ctx
                                <?> "the body"
        return  $  DTerm $ DeclTerm
                { declAnnot       = rName
                , declTermMode    = DeclTermModeProc
                , declName        = nName
                , declParams      = mps
                , declTypesResult = tsRes
                , declBody        = mBody }
 ]


------------------------------------------------------------------------------------------- Test --
-- | Parser for a test declaration.
pDeclTest :: Context -> Parser (Decl RL)
pDeclTest ctx
 = do   -- 'watch'? 'test' 'kind'   (Name '=')? Type
        -- 'watch'? 'test' 'type'   (Name '=')? Term
        -- 'watch'? 'test' 'eval'   (Name '=')? Term
        -- 'watch'? 'test' 'exec'   (Name '=')? Term
        -- 'watch'? 'test' 'assert' (Name '=')? Term
        bWatch
         <- P.choice
         [  do  pTok KTest;  return False

         ,  do  pTok KWatch
                P.choice [ pTok KTest, return ()]
                return True
         ]

        -- Lookahead to get the test mode.
        --  If we don't recognize it then we want the error to be on this token,
        --  not the one after.
        nMode
         <- P.lookAhead (P.choice [ pVar, do pTok KType; return "type"])
         <?> "the test mode"

        -- Check this is a valid test mode.
        unless (elem nMode
                [ "kind",      "type"
                , "eval'type", "eval'term", "eval"
                , "exec",      "assert"])
         $ P.unexpected "test mode"

        -- Don't allow exec tests to be watched in the IDE as we don't have
        -- anywhere to direct console output to, and the order of side effects
        -- is indetermiante.
        when (bWatch && nMode == "exec")
         $ fail "exec tests cannot be watched"

        -- 'type' is both a test specifier and a keyword,
        -- so we need to match for it explicitly.
        P.choice
         [ do   pVar
         , do   pTok KType; return "type" ]

        -- See if this is a named test, or a bare type/term.
        --  We know it's a named test if we can see the '=' after the name.
        mRangeName
                <- P.choice
                [  P.try $ do rn <- pRanged pVar; pTok KEquals; return $ Just rn
                ,  return Nothing ]

        let mName = fmap snd mRangeName

        -- What we parse next depends on the test mode.
        lStart  <- locHere
        P.choice
         [ do   guard $ nMode == "kind"
                tType   <-  pType
                        <?> if isJust mName
                                then "the type to take the kind of"
                                else "a test name, or the type to take the kind of"
                lEnd    <- locPrev
                let a   = fromMaybe (Range lStart lEnd) $ fmap fst mRangeName
                return  $ DTest $ DeclTestKind
                        { declAnnot     = a
                        , declTestWatch = bWatch
                        , declTestName  = mName
                        , declTestType  = tType }

         , do   guard $ nMode == "type"
                mTerm   <- pTerm ctx
                        <?> if isJust mName
                                then "the term to take the type of"
                                else "a test name, or the term to take the type of"
                lEnd    <- locPrev
                let a   = fromMaybe (Range lStart lEnd) $ fmap fst mRangeName
                return  $ DTest $ DeclTestType
                        { declAnnot     = a
                        , declTestWatch = bWatch
                        , declTestName  = mName
                        , declTestTerm  = mTerm }

         , do   guard $ (nMode == "eval'type")
                tBody   <- pType
                        <?> if isJust mName
                                then "the type to evaluate"
                                else "a test name, or the type to evaluate"
                lEnd    <- locPrev
                let a   = fromMaybe (Range lStart lEnd) $ fmap fst mRangeName
                return  $ DTest $ DeclTestEvalType
                        { declAnnot     = a
                        , declTestWatch = bWatch
                        , declTestName  = mName
                        , declTestType  = tBody }

         , do   guard $ (nMode == "eval'term") || (nMode == "eval")
                mBody   <- pTerm ctx
                        <?> if isJust mName
                                then "the term to evaluate"
                                else "a test name, or the term to evaluate"
                lEnd    <- locPrev
                let a   = fromMaybe (Range lStart lEnd) $ fmap fst mRangeName
                return  $ DTest $ DeclTestEvalTerm
                        { declAnnot     = a
                        , declTestWatch = bWatch
                        , declTestName  = mName
                        , declTestTerm  = mBody }

         , do   guard $ nMode == "exec"
                mBody   <- pTerm ctx
                        <?> if isJust mName
                                then "the term to execute"
                                else "a test name, or the term to execute"
                lEnd    <- locPrev
                let a   = fromMaybe (Range lStart lEnd) $ fmap fst mRangeName
                return  $ DTest $ DeclTestExec
                        { declAnnot     = a
                        , declTestWatch = bWatch
                        , declTestName  = mName
                        , declTestBody  = mBody }

         , do   guard $ nMode == "assert"
                mBody   <- pTerm ctx
                        <?> if isJust mName
                                then "the term you hope is true"
                                else "a test name, or the term you hope is true"
                lEnd    <- locPrev
                let a   = fromMaybe (Range lStart lEnd) $ fmap fst mRangeName
                return  $ DTest $ DeclTestAssert
                        { declAnnot     = a
                        , declTestWatch = bWatch
                        , declTestName  = mName
                        , declTestBody  = mBody }
         ]


------------------------------------------------------------------------------------------- Emit --
-- | Parser for an emit declaration.
pDeclEmit :: Context -> Parser (Decl RL)
pDeclEmit ctx
 = do   -- 'emit' Var? '=' Term
        pTok KEmit

        -- See if this is a named emission, or a bare type/term.
        --  We know it's a named test if we can see the '=' after the name.
        mRangeName
                <- P.choice
                [  P.try $ do rn <- pRanged pVar; pTok KEquals; return $ Just rn
                ,  return Nothing ]

        let mName = fmap snd mRangeName

        -- What we parse next depends on the test mode.
        lStart  <- locHere
        mBody   <- pTerm ctx    <?> "the body"
        lEnd    <- locPrev
        let a   = fromMaybe (Range lStart lEnd) $ fmap fst mRangeName
        return  $ DEmit $ DeclEmit
                { declAnnot     = a
                , declEmitName  = mName
                , declEmitBody  = mBody }
