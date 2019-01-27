
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
 [ do   -- 'let' '[' Var,* ']' '=' Term Proc
        pTok KLet
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
        mBind <- pTerm    <?> "a term for the binding"
        mRest <- pTermProc pTerm pTermApp
        return  $ MProcLet (MPAnn rBinds (MPTerms bts)) mBind mRest


 , do   -- 'seq' Stmt Proc
        pTok KSeq
        mStmt   <- pTermStmt pTerm pTermApp
        mRest   <- pTermProc pTerm pTermApp
        return  $ MProcSeq mStmt mRest


 , do   -- 'cel' Var ':' Type '←' Term Proc
        pTok KCel
        nCell   <- pVar
        pTok KColon
        tCell   <- pType
        pLeft
        mBind   <- pTerm
        mRest   <- pTermProc pTerm pTermApp
        return  $ MProcCel nCell tCell mBind mRest


 , do   -- 'end'
        -- 'end' 'with' Term
        pTok KEnd
        P.choice
         [ do   pTok KWith
                mResult <- pTerm
                return  $  MProcEndWith mResult

         , do   return  $  MProcEnd ]
 ]


--------------------------------------------------------------------------------------- TermStmt --
pTermStmt :: Parser (Term RL) -> Parser (Term RL) -> Parser (Term RL)
pTermStmt pTerm pTermApp
 = P.choice
 [ do   --  'if' '{' (Term '→' TermStmt);+ '}'
        --  'if' Term 'then' TermStmt
        pTok KIf
        P.choice
         [ do   -- ... '{' (Term '→' TermStmt);+ '}'
                pTok KCBra          <?> "a '{' to start the list of branches"
                (msCond, msThen)
                 <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
                 $  do  mCond <- pTerm      <?> "a term for a condition."
                        pRight              <?> "a completed term, or '→' to start the body"

                        mThen <- pTermStmt pTerm pTermApp
                         <?> "the body of the branch"

                        return (mCond, mThen)
                pTok KCKet

                return  $ MStmtIf msCond msThen

          , do  -- ... Term 'then' TermStmt
                mCond   <- pTerm        <?> "a term for the condition"
                pTok KThen              <?> "a completed procedure, or 'then' to start the body"

                mThen   <- pTermStmt pTerm pTermApp
                 <?> "the body of the 'then' branch"

                return $ MStmtIf [mCond] [mThen]
         ]

 , do   -- 'case' Term 'of' '{' (Lbl [(Var ':' Type)*] '→' Stmt)* '}'
        pTok KCase
        mScrut <- pTerm         <?> "a term for the scrutinee"
        pTok KOf                <?> "a completed term, or 'of' to start the alternatives"
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

                mBody <- pTermStmt pTerm pTermApp
                 <?> "the body of the alternative"

                return $ MVarAlt lAlt (MPAnn rPat $ MPTerms btsPat) mBody

        pTok KCKet              <?> "a completed term, or '}' to end the alternatives"
        return $ MStmtCase mScrut msAlts

 , do   -- 'loop' Stmt
        pTok KLoop
        mBody   <- pTermStmt pTerm pTermApp
        return  $ MStmtLoop mBody

 , do   -- 'Var' '←' Term
        P.lookAhead $ do
                pVar; pLeft

        nCell   <- pVar
        pLeft
        mValue  <- pTerm
        return  $  MStmtUpdate nCell mValue

 , do   -- 'break'
        pTok KBreak
        return MStmtBreak

 , do   -- 'continue'
        pTok KContinue
        return MStmtContinue

 , do   -- 'return'
        pTok KReturn
        mBody <- pTerm
        return $ MStmtReturn mBody

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
