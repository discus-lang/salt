
-- TODO: check we still have expected text at intermediate points.
module Salt.Core.Codec.Text.Parser.TermProc where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


--------------------------------------------------------------------------------------- TermProc --
-- | Parser for a procedure.
pTermProc :: Parser (Term RL) -> Parser (Term RL) -> Parser (Term RL)
pTermProc pTerm pTermApp
 = P.choice
 [ do   pProcFinal pTerm pTermApp


 , do   mkProc <- pProcStmt pTerm pTermApp
        P.choice
         [ do   pTok KSemi
                mRest   <- pTermProc pTerm pTermApp
                return  $ mkProc mRest

         , do   return  $ mkProc $ MProcYield (MTerms []) ]
 ]


-------------------------------------------------------------------------------------- ProcFinal --
-- | Parse a final statement for a procedure,
--   which is a procedure that has no tail.
pProcFinal :: Parser (Term RL) -> Parser (Term RL)
           -> Parser (Term RL)
pProcFinal pTerm pTermApp
 = P.choice
 [ do   -- ( PROC )
        pTok KRBra
        mProc <- pTermProc pTerm pTermApp
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

 , do   -- 'return' Exp
        pTok KReturn
        mBody <- pTerm
        return $ MProcReturn mBody


 ]


--------------------------------------------------------------------------------------- ProcStmt --
-- | Parse a statement which expects a tail.
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

 , do   --  'when' '{' (Term '→' TermProc);+ '}' ...
        --  'when' Term 'then' TermProc 'done' ...
        pTok KWhen
        P.choice
         [ do   -- ... '{' (Term '→' TermProc);+ '}' ...
                pTok KCBra          <?> "a '{' to start the list of branches"
                (msCond, msThen)
                 <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
                 $  do  mCond <- pTerm  <?> "a term for a condition."
                        pRight          <?> "a completed term, or '→' to start the body"

                        mThen <- pTermProc pTerm pTermApp
                         <?> "the body of the branch"

                        return (mCond, mThen)
                pTok KCKet
                return $ \mRest -> MProcWhen msCond msThen mRest

          , do  -- ... Term 'then' TermStmt 'done' ...
                mCond   <- pTerm        <?> "a term for the condition"
                pTok KThen              <?> "a completed procedure, or 'then' to start the body"

                mThen   <- pTermProc pTerm pTermApp
                 <?> "the body of the 'then' branch"

                pTok KDone
                return $ \mRest -> MProcWhen [mCond] [mThen] mRest
          ]

 , do   -- 'match' Term 'of' '{' (Lbl [(Var ':' Type)*] '→' Stmt);* '}' ...
        pTok KMatch
        mScrut <- pTerm         <?> "a term for the scrutinee"
        pTok KWith              <?> "a completed term, or 'of' to start the alternatives"
        pTok KCBra              <?> "a '{' to start the list of alternatives"
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

                mBody <- pTermProc pTerm pTermApp
                 <?> "the body of the alternative"

                return $ MVarAlt lAlt (MPAnn rPat $ MPTerms btsPat) mBody

        pTok KCKet              <?> "a completed term, or '}' to end the alternatives"
        return $ \mRest -> MProcMatch mScrut msAlts mRest

 , do   -- 'loop' Proc 'done' ...
        pTok KLoop
        mBody   <- pTermProc pTerm pTermApp
        pTok KDone
        return $ \mRest -> MProcLoop mBody mRest
 ]


--------------------------------------------------------------------------------------- ProcBind --
pProcBind  :: Parser (Term RL) -> Parser (Term RL)
           -> Parser (TermParams RL, Term RL)

pProcBind pTerm pTermApp
 = P.choice
 [ P.try $ do
        -- Binds '=' Proc
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

        pTok KEquals      <?> "a type annotation, or '=' to start the binding"
        mBind <- pTermProc pTerm pTermApp
                          <?> "a procedure for the binding"

        return (MPAnn rBinds $ MPTerms bts, mBind)

 , do   -- Proc
        mBind <- pTermProc pTerm pTermApp
        return (MPTerms [], mBind)
 ]


--------------------------------------------------------------------------------------- TermStmt --
{-
pTermStmt :: Parser (Term RL) -> Parser (Term RL) -> Parser (Term RL)
pTermStmt pTerm pTermApp
 = P.choice
 [ do   -- 'break'
        pTok KBreak
        return MStmtBreak

 , do   -- 'continue'
        pTok KContinue
        return MStmtContinue


 , do   -- 'proc' Types 'of' Proc
        pTok KProc
        tsReturn <- pTypes
        pTok KOf
        mBody    <- pTermProc pTerm pTermApp
        return $ MStmtProc tsReturn mBody

 , do   -- Proc
        mBody   <- pTermProc pTerm pTermApp
        return  $ MStmtNest mBody

 , do   -- TermApp
        mBody    <- pTermApp
        return  $ MStmtCall mBody

 ]
-}
