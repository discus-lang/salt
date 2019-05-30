
-- TODO: check we still have expected text at intermediate points.
module Salt.Core.Codec.Text.Parser.Proc where
import Salt.Core.Codec.Text.Parser.Params
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


------------------------------------------------------------------------------------------- Proc --
-- | Parser for a procedure.
pProc :: Parser (Term RL) -> Parser (Term RL) -> Parser (Term RL)
pProc pTerm pTermApp
 = P.choice
 [ do   -- ProcFinal (';' Proc) | ε
        mkProc <- pProcStmt pTerm pTermApp
        P.choice
         [ do   pTok KSemi
                mRest   <- pProc pTerm pTermApp
                return  $ mkProc mRest

         , do   return  $ mkProc $ MProcYield (MTerms []) ]

 , do   -- ProcFinal
        pProcFinal pTerm pTermApp
 ]


-------------------------------------------------------------------------------------- ProcFinal --
-- | Parse a final procedure,
--   which is a procedure that has no tail.
pProcFinal :: Parser (Term RL) -> Parser (Term RL)
           -> Parser (Term RL)
pProcFinal pTerm pTermApp
 = P.choice
 [ do   -- ( PROC )
        pTok KRBra
        mProc <- pProc pTerm pTermApp
        pTok KRKet
        return mProc

 , do   -- 'yield' Exp
        pTok KYield
        mExp <- pTerm
        return $ MProcYield mExp

 , do   -- 'end'
        pTok KEnd
        return $ MProcYield (MTerms [])

 , do   -- 'call' Prm TermArgs*
        -- 'call' Con TermArgs*
        pTok KCall
        mApp <- pTermApp
        let Just (mFun, mgssArg) = takeMAps mApp
        return $ MProcCall mFun mgssArg

 , do   -- 'launch' Types of ...
        pTok KLaunch
        tsRet   <- pTypes
        pTok KOf
        mRest   <- pProc pTerm pTermApp
        return  $ MProcLaunch tsRet mRest

 , do   -- 'return' Exp
        pTok KReturn
        mBody <- pTerm
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
        pProcDo pTerm pTermApp

 , do   -- Prm TermArgs*
        -- Con TermArgs*
        -- TODO: check we have at least one arg.
        mApp <- pTermApp
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
pProcStmt :: Parser (Term RL) -> Parser (Term RL)
          -> Parser (Term RL -> Term RL)
pProcStmt pTerm pTermApp
 = P.choice
 [ do   -- 'seq' '[' Var,* ']' '=' Proc ...
        pTok KSeq
        (mps, mBind) <- pProcBind pTerm pTermApp
        return  $ \mRest -> MProcSeq mps mBind mRest

 , do   -- 'cell' Var ':' Type '←' Term ...
        pTok KCell
        nCell   <- pVar
        pTok KColon
        tCell   <- pType
        pLeft
        mBind   <- pTerm
        return  $ \mRest -> MProcCell nCell tCell mBind mRest

 , do   -- 'update' 'Var' '←' Term ...
        pTok KUpdate
        nCell   <- pVar
        pLeft
        mValue  <- pTerm
        return  $ \mRest -> MProcUpdate nCell mValue mRest

 , do   -- 'when' Term (ProcStmt | ProcFinal)
        pTok KWhen
        mCond   <- pTerm        <?> "a term for the condition"
        P.choice
         [ do   mkThen <- pProcStmt pTerm pTermApp
                 <?> "the body of the 'then' branch"
                let mThen = mkThen (MProcYield (MTerms []))
                return $ \mRest -> MProcWhen [mCond] [mThen] mRest

         , do   mThen   <- pProcFinal pTerm pTermApp
                 <?> "the body of the 'then' branch"
                return $ \mRest -> MProcWhen [mCond] [mThen] mRest
         ]

 , do   --  'whens' '{' (Term '→' Proc);+ '}' ...
        pTok KWhens
        pTokBlock KCBra KSemi KCKet
         <?> "a '{' to start the list of branches"

        (msCond, msThen)
         <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
         $  do  mCond <- pTerm  <?> "a term for a condition."
                pRight          <?> "a completed term, or '→' to start the body"

                mThen <- pProc pTerm pTermApp
                                <?> "the body of the branch"
                return (mCond, mThen)
        pTok KCKet
        return $ \mRest -> MProcWhen msCond msThen mRest


 , do   -- 'match' Term 'of' '{' (Lbl [(Var ':' Type)*] '→' Stmt);* '}' ...
        pTok KMatch
        mScrut <- pTerm         <?> "a term for the scrutinee"
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

                mBody <- pProc pTerm pTermApp
                 <?> "the body of the alternative"

                return $ MVarAlt lAlt (MPAnn rPat $ MPTerms btsPat) mBody

        pTok KCKet              <?> "a completed term, or '}' to end the alternatives"
        return $ \mRest -> MProcMatch mScrut msAlts mRest

 , do   -- 'loop' Proc (ProcStmt | ProcFinal) ...
        pTok KLoop
        P.choice
         [ do   mkThen <- pProcStmt pTerm pTermApp
                 <?> "the body of the 'then' branch"
                let mThen = mkThen (MProcYield (MTerms []))
                return $ \mRest -> MProcLoop mThen mRest

         , do   mThen   <- pProcFinal pTerm pTermApp
                 <?> "the body of the 'then' branch"
                return $ \mRest -> MProcLoop mThen mRest
         ]

  , do  -- 'let' Binds '=' Term ...
        pTok KLet
        mps     <- pProcBinds
        pTok KEquals
        mBind   <- pTerm
        return  $ \mRest -> MProcSeq mps (MProcYield mBind) mRest

  , do  -- 'enter' Name 'with' '{' ProcTermBind+ '}' ...
        pTok KEnter
        mEnter  <- pTermApp
        pTok KWith      <?> "'with' to start the list of bindings"
        pTokBlock KCBra KSemi KCKet
         <?> "a '{' to start the list of bindings"
        bms     <- flip P.sepEndBy1 (pTok KSemi)
                $  do   b       <- pBind
                        mps     <- P.many pTermParams
                        pTok KColon
                        ts      <- pTypes
                        pTok KEquals
                        m       <- pProc pTerm pTermApp
                        return  $  MBind b mps ts m
        pTok KCKet
        return  $ \mRest -> MProcEnter mEnter bms mRest

  , P.try $ do
        -- Binds '=' Proc ...
        --   Seq without an initial keyword.
        --   We know this is a seq when we get to the '='
        mps     <- pProcBinds
        pTok KEquals
        mBind   <- pProcFinal pTerm pTermApp
        return  $ \mRest -> MProcSeq mps mBind mRest

 , P.try $ do
        -- Var '←' Term ...
        --   Update form without an initial keyword.
        --   We know this is an update when we get to the '←'.
        nCell   <- pVar
        pLeft
        mBind   <- pTerm
        return  $ \mRest -> MProcUpdate nCell mBind mRest
 ]


----------------------------------------------------------------------------------------- ProcDo --
-- | Parser for a 'do' construct.
pProcDo    :: Parser (Term RL) -> Parser (Term RL)
           -> Parser (Term RL)
pProcDo pTerm pTermApp
 = do   -- 'do' '{' ProcStmt; ... ProcFinal }
        pTok KDo
        pTokBlock KCBra KSemi KCKet
        mBody <- pProcDoStmts pTerm pTermApp
        pTok KCKet
        return mBody


-- | Parser for the statements in the body of a 'do' construct.
pProcDoStmts :: Parser (Term RL) -> Parser (Term RL)
           -> Parser (Term RL)
pProcDoStmts pTerm pTermApp
 = P.choice
 [ do   mkStmt  <- pProcStmt pTerm pTermApp
        P.choice
         [ do   pTok KSemi
                mRest <- pProcDoStmts pTerm pTermApp
                return $ mkStmt mRest
         , do   return $ mkStmt $ MProcYield (MTerms []) ]

 , do   mFinal  <- pProcFinal pTerm pTermApp
        P.choice
         [ do   pTok KSemi
                mRest <- pProcDoStmts pTerm pTermApp
                return $ MProcSeq (MPTerms []) mFinal mRest
         , do   return $ mFinal ]
 ]


--------------------------------------------------------------------------------------- ProcBind --
-- | Parser for a procedure binding.
pProcBind  :: Parser (Term RL) -> Parser (Term RL)
           -> Parser (TermParams RL, Term RL)
pProcBind pTerm pTermApp
 = P.choice
 [ P.try $ do
        -- Binds '=' Proc
        mps   <- pProcBinds
        pTok KEquals      <?> "a type annotation, or '=' to start the binding"
        mBind <- pProc pTerm pTermApp
                          <?> "a procedure for the binding"
        return (mps, mBind)

 , do   -- Proc
        mBind <- pProc pTerm pTermApp
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


