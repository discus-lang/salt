
-- TODO: check we still have expected text at intermediate points.
module Salt.Core.Codec.Text.Parser.Proc
        (pProc)
where
import Salt.Core.Codec.Text.Parser.Params
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P

---------------------------------------------------------------------------------------- Context --
pTerm    ctx = contextParseTerm    ctx ctx
pTermApp ctx = contextParseTermApp ctx ctx
pTermArg ctx = contextParseTermArg ctx ctx


------------------------------------------------------------------------------------------- Proc --
-- | Parser for a procedure.
pProc :: Context -> Parser (Term RL)
pProc ctx
 = pMAnn $ P.choice
 [ do   -- ( PROC )
        pTok KRBra
        mProc <- pProc ctx
        pTok KRKet
        return mProc

 , do   -- ProcStmt ((';' Proc) | ε)
        mkProc <- pProcStmt ctx
        P.choice
         [ do   pTok KSemi
                mRest   <- pProc ctx
                return  $ mkProc mRest

         , do   return  $ mkProc $ MProcYield (MTerms []) ]

 , do   -- ProcFinal
        pProcFinal ctx
 ]


-------------------------------------------------------------------------------------- ProcFinal --
-- | Parse a final procedure,
--   which is a procedure that has no tail.
pProcFinal :: Context -> Parser (Term RL)
pProcFinal ctx
 = P.choice
 [ do   -- ( PROC )
        pTok KRBra
        mProc <- pProc ctx
        pTok KRKet
        return mProc

 , do   -- 'yield' Exp
        pTok KYield
        mExp <- pTerm ctx
        return $ MProcYield mExp

 , do   -- 'end'
        pTok KEnd
        return $ MProcYield (MTerms [])

 , do   -- 'call' Prm TermArgs*
        -- 'call' Con TermArgs*
        pTok KCall
        mApp <- pTermApp ctx
        let Just (mFun, mgssArg) = takeMAps mApp
        return $ MProcCall mFun mgssArg

 , do   -- 'launch' Types of ...
        pTok KLaunch
        tsRet   <- pTypes
        pTok KOf
        mRest   <- pProc ctx
        return  $ MProcLaunch tsRet mRest

 , do   -- 'return' Exp
        pTok KReturn
        mBody <- pTerm ctx
        return $ MProcReturn mBody

        -- 'break'
 , do   pTok KBreak
        return MProcBreak

 , do   -- 'continue'
        pTok KContinue
        return MProcContinue

 , do   -- 'leave'
        pTok KLeave
        return MProcLeave

 , do   -- ProcDo
        pProcDo ctx

 , do   -- Prm TermArgs*
        -- Con TermArgs*
        -- TODO: check we have at least one arg.
        mApp <- pTermApp ctx
        (mFun, mgssArg)
         <- case takeMAps mApp of
                Just (mFun, mgssArg) -> return (mFun, mgssArg)
                _ -> P.parserZero

        return $ MProcCall mFun mgssArg
 ]


--------------------------------------------------------------------------------------- ProcStmt --
-- | Parse a procedure which has a tail.
--   We produce a function that takes the tail and builds the
--   overall procedure.
pProcStmt :: Context -> Parser (Term RL -> Term RL)
pProcStmt ctx
 = P.choice
 [ do   -- 'seq' '[' Var,* ']' '=' Proc ...
        pTok KSeq
        (mps, mBind) <- pProcBind ctx
        return  $ \mRest -> MProcSeq mps mBind mRest

 , do   -- 'cell' Var ':' Type '←' Term ...
        pTok KCell
        nCell   <- pVar
        pTok KColon
        tCell   <- pType
        pLeft
        mBind   <- pTerm ctx
        return  $ \mRest -> MProcCell nCell tCell mBind mRest

 , do   -- 'update' 'Var' '←' Term ...
        pTok KUpdate
        nCell   <- pVar
        pLeft
        mValue  <- pTerm ctx
        return  $ \mRest -> MProcUpdate nCell mValue mRest

 , do   -- 'when' TermArg Proc ...
        pTok KWhen
        mCond   <- pTermArg ctx    <?> "a term for the condition"
        mThen   <- pProc    ctx    <?> "the body of the 'then' branch"
        return  $ \mRest -> MProcWhens [mCond] [mThen] mRest

 , do   --  'whens' '{' (Term '→' Proc);+ '}' ...
        pTok KWhens
        pTokBlock KCBra KSemi KCKet
         <?> "a '{' to start the list of branches"

        (msCond, msThen)
         <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
         $  do  mCond <- pTerm ctx <?> "a term for a condition."
                pRight             <?> "a completed term, or '→' to start the body"

                mThen <- pProc ctx <?> "the body of the branch"
                return (mCond, mThen)
        pTok KCKet
        return $ \mRest -> MProcWhens msCond msThen mRest

 , do   -- 'match' Term 'of' '{' (Lbl [(Var ':' Type)*] '→' Stmt);* '}' ...
        pTok KMatch
        mScrut <- pTerm ctx     <?> "a term for the scrutinee"
        pTok KWith              <?> "a completed term, or 'of' to start the alternatives"

        pTokBlock KCBra KSemi KCKet
         <?> "a '{' to start the list of alternatives"

        msAlts
         <- flip P.sepEndBy1 (pTok KSemi)
         $  do  lAlt    <- pLbl <?> " a variant label"

                (rPat, btsPat)
                  <- pRanged (pSquared
                        $  flip P.sepBy (pTok KComma)
                        $  do   b <- pBind  <?> "a binder for a variant field"
                                pTok KColon <?> "a ':' to specify the type of the field"
                                t <- pType  <?> "the type of the field"
                                return (b, t))
                        <?> "binders for the payload of the variant"

                pRight          <?> "a '→' to start the body"

                mBody <- pProc ctx
                 <?> "the body of the alternative"

                return $ MVarAlt lAlt (MPAnn rPat $ MPTerms btsPat) mBody

        pTok KCKet              <?> "a completed term, or '}' to end the alternatives"
        return $ \mRest -> MProcMatch mScrut msAlts mRest

 , do   -- 'loop' Proc ...
        pTok KLoop
        mBody   <- pProc ctx    <?> "the body of the loop"
        return  $ \mRest -> MProcLoop mBody mRest

  , do  -- 'while' TermArg (ProcStmt | ProcFinal) ...
        pTok KWhile
        mPred   <- pTermArg ctx <?> "the 'while' predicate"
        mBody   <- pProc ctx    <?> "the body of the loop"
        return  $ \mRest -> MProcWhile mPred mBody mRest

  , do  -- 'let' Binds '=' Term ...
        pTok KLet
        mps     <- pProcBinds
        pTok KEquals
        mBind   <- pTerm ctx
        return  $ \mRest -> MProcSeq mps (MProcYield mBind) mRest

  , do  -- 'enter' Name 'with' '{' ProcTermBind+ '}' ...
        pTok KEnter
        mEnter  <- pTermApp ctx
        pTok KWith      <?> "'with' to start the list of bindings"
        pTokBlock KCBra KSemi KCKet
         <?> "a '{' to start the list of bindings"
        bms     <- flip P.sepEndBy1 (pTok KSemi)
                $  do   b       <- pBind
                        mps     <- P.many pTermParams
                        pTok KColon
                        ts      <- pTypesResult
                        pTok KEquals
                        m       <- pProc ctx
                        return  $  MBind b mps ts m
        pTok KCKet
        return  $ \mRest -> MProcEnter mEnter bms mRest

  , P.try $ do
        -- Binds '=' Proc ...
        --   Seq without an initial keyword.
        --   We know this is a seq when we get to the '='
        mps     <- pProcBinds
        pTok KEquals
        mBind   <- pProcFinal ctx
        return  $ \mRest -> MProcSeq mps mBind mRest

 , P.try $ do
        -- Var '←' Term ...
        --   Update form without an initial keyword.
        --   We know this is an update when we get to the '←'.
        nCell   <- pVar
        pLeft
        mBind   <- pTerm ctx
        return  $ \mRest -> MProcUpdate nCell mBind mRest
 ]


----------------------------------------------------------------------------------------- ProcDo --
-- | Parser for a 'do' construct.
pProcDo :: Context -> Parser (Term RL)
pProcDo ctx
 = do   -- 'do' '{' ProcStmt; ... ProcFinal }
        pTok KDo
        pTokBlock KCBra KSemi KCKet
        mBody <- pProcDoStmts ctx
        pTok KCKet
        return mBody


-- | Parser for the statements in the body of a 'do' construct.
pProcDoStmts :: Context -> Parser (Term RL)
pProcDoStmts ctx
 = P.choice
 [ do   mkStmt  <- pProcStmt ctx
        P.choice
         [ do   pTok KSemi
                mRest <- pProcDoStmts ctx
                return $ mkStmt mRest

         , do   return $ mkStmt $ MProcYield (MTerms []) ]

 , do   mFinal  <- pProcFinal ctx
        P.choice
         [ do   pTok KSemi
                mRest <- pProcDoStmts ctx
                return $ MProcSeq (MPTerms []) mFinal mRest
         , do   return $ mFinal ]
 ]


--------------------------------------------------------------------------------------- ProcBind --
-- | Parser for a procedure binding.
pProcBind :: Context -> Parser (TermParams RL, Term RL)
pProcBind ctx
 = P.choice
 [ P.try $ do
        -- Binds '=' Proc
        mps   <- pProcBinds
        pTok KEquals       <?> "a type annotation, or '=' to start the binding"
        mBind <- pProc ctx <?> "a procedure for the binding"
        return (mps, mBind)

 , do   -- Proc
        mBind <- pProc ctx
        return (MPTerms [], mBind)
 ]

pProcBinds :: Parser (TermParams RL)
pProcBinds
 = do
        (rBinds, bts)
         <- pRanged (P.choice
                [ do    pSquared
                         $ flip P.sepBy (pTok KComma)
                         $ do   b  <- pBind <?> "a binder"
                                P.choice
                                 [ do   pTok KColon
                                        t <- pType <?> "a type for the binder"
                                        return (b, t)
                                 , do   return (b, THole) ]
                                 <?> "a binder"

                , do    b       <- pBind
                        P.choice
                         [ do   pTok KColon
                                t <- pType <?> "a type for the binder"
                                return [(b, t)]

                         , do   return [(b, THole)]]
                ]
                <?> "some binders")

        return $ MPAnn rBinds (MPTerms bts)


