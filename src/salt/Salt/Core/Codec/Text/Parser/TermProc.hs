
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
 [ do   -- 'yield' Exp
        pTok KYield
        mExp <- pTerm
        return $ MProcYield mExp

 , do   -- 'call' Prm TermArgs*
        -- 'call' Con TermArgs*
        pTok KCall
        mApp <- pTermApp
        let Just (mFun, mgssArg) = takeMAps mApp
        return $ MProcCall mFun mgssArg

 , do   -- 'seq' '[' Var,* ']' '=' Proc ('end' | ';' Proc)
        pTok KSeq
        (mps, mBind) <- pProcBind pTerm pTermApp

        P.choice
         [ do   pTok KEnd
                return $ MProcSeq mps mBind (MProcYield (MTerms []))

         , do   pTok KSemi
                P.choice
                 [ do   pTok KEnd
                        return $ MProcSeq mps mBind (MProcYield (MTerms []))

                 , do   mRest        <- pTermProc pTerm pTermApp
                        return $ MProcSeq mps mBind mRest
                 ]
         ]

 , do   -- 'cell' Var ':' Type '←' Term ';' Proc
        pTok KCell
        nCell   <- pVar
        pTok KColon
        tCell   <- pType
        pLeft
        mBind   <- pTerm
        pTok KSemi
        mRest   <- pTermProc pTerm pTermApp
        return  $ MProcCell nCell tCell mBind mRest

 , do   -- 'update' 'Var' '←' Term ';' Proc
        pTok KUpdate
        nCell   <- pVar
        pLeft
        mValue  <- pTerm
        pTok KSemi
        mRest   <- pTermProc pTerm pTermApp
        return  $ MProcUpdate nCell mValue mRest

 , do   -- 'return'
        pTok KReturn
        mBody <- pTerm
        return $ MProcReturn mBody

 , do   --  'when' '{' (Term '→' TermProc);+ '}' (';' TermProc)?
        --  'when' Term 'then' TermProc 'done'   (';' TermProc)?
        pTok KWhen
        P.choice
         [ do   -- ... '{' (Term '→' TermProc);+ '}' (';' TermProc)?
                pTok KCBra          <?> "a '{' to start the list of branches"
                (msCond, msThen)
                 <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
                 $  do  mCond <- pTerm      <?> "a term for a condition."
                        pRight              <?> "a completed term, or '→' to start the body"

                        mThen <- pTermProc pTerm pTermApp
                         <?> "the body of the branch"

                        return (mCond, mThen)
                pTok KCKet

                P.choice
                 [ do   -- ... ';' TermProc
                        pTok KSemi
                        mRest   <- pTermProc pTerm pTermApp
                        return  $ MProcWhen msCond msThen mRest

                 , do   -- ...
                        return $ MProcWhen msCond msThen (MProcYield (MTerms []))
                 ]

          , do  -- ... Term 'then' TermStmt 'done' (';' TermProc)?
                mCond   <- pTerm        <?> "a term for the condition"
                pTok KThen              <?> "a completed procedure, or 'then' to start the body"

                mThen   <- pTermProc pTerm pTermApp
                 <?> "the body of the 'then' branch"

                pTok KDone

                P.choice
                 [ do   -- ... ';' TermProc
                        pTok KSemi              <?> "a ';' to continue the procedure"
                        mRest   <- pTermProc pTerm pTermApp
                        return $ MProcWhen [mCond] [mThen] mRest

                 , do   -- ...
                        return $ MProcWhen [mCond] [mThen] (MProcYield (MTerms []))
                 ]
          ]

 , do   -- 'match' Term 'of' '{' (Lbl [(Var ':' Type)*] '→' Stmt);* '}' (';' TermProc?)
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

        P.choice
         [ do   -- ... ';' TermProc
                pTok KSemi
                mRest <- pTermProc pTerm pTermApp
                return $ MProcMatch mScrut msAlts mRest

         , do   -- ...
                return $ MProcMatch mScrut msAlts (MProcYield (MTerms []))
         ]

 , do   -- 'loop' Proc 'done' (';' TermProc)?
        pTok KLoop
        mBody   <- pTermProc pTerm pTermApp
        pTok KDone

        P.choice
         [ do   -- ... ';' TermProc
                pTok KSemi
                mRest   <- pTermProc pTerm pTermApp
                return $ MProcLoop mBody mRest

         , do   -- ...
                return $ MProcLoop mBody (MProcYield (MTerms []))
         ]

 , do   -- ( PROC )
        pTok KRBra
        mProc <- pTermProc pTerm pTermApp
        pTok KRKet
        return mProc
 ]


--------------------------------------------------------------------------------------- ProcBind --
pProcBind
        :: Parser (Term RL) -> Parser (Term RL)
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
