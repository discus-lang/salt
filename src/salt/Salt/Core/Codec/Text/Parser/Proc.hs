
-- TODO: check we still have expected text at intermediate points.
module Salt.Core.Codec.Text.Parser.Proc
        (pProcStmt, pProcDo, pProcDoStmt)
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


--------------------------------------------------------------------------------------- ProcStmt --
-- | Parse a procedure which has a tail.
--   We produce a function that takes the tail and builds the
--   overall procedure.
pProcStmt :: Context -> Parser (Term RL -> Term RL)
pProcStmt ctx
 = P.choice
 [ do   -- 'seq' Term Proc ...
        pTok KSeq
        mBind <- pTerm ctx
        return  $ \mRest -> MLet (MPTerms []) mBind mRest

 , do   -- 'cell' Var ':' Type '←' Term ...
        pTok KCell
        nCell   <- pVar
        pTok KColon
        tCell   <- pType
        pLeft
        mBind   <- pTerm ctx
        return  $ \mRest -> MCell nCell tCell mBind mRest

 , do   -- 'update' 'Var' '←' Term ...
        pTok KUpdate
        nCell   <- pVar
        pLeft
        mValue  <- pTerm ctx
        return  $ \mRest -> MUpdate nCell mValue mRest

 , do   -- 'when' TermArg Proc ...
        pTok KWhen
        mCond   <- pTermArg ctx    <?> "a term for the condition"
        mThen   <- pTerm    ctx    <?> "the body of the 'then' branch"
        return  $ \mRest -> MWhens [mCond] [mThen] mRest

 , do   --  'whens' '{' (Term '→' Proc);+ '}' ...
        pTok KWhens
        pTokBlock KCBra KSemi KCKet
         <?> "a '{' to start the list of branches"

        (msCond, msThen)
         <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
         $  do  mCond <- pTerm ctx <?> "a term for a condition."
                pRight             <?> "a completed term, or '→' to start the body"

                mThen <- pTerm ctx <?> "the body of the branch"
                return (mCond, mThen)
        pTok KCKet
        return $ \mRest -> MWhens msCond msThen mRest

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

                mBody <- pTerm ctx
                 <?> "the body of the alternative"

                return $ MVarAlt lAlt (MPAnn rPat $ MPTerms btsPat) mBody

        pTok KCKet              <?> "a completed term, or '}' to end the alternatives"
        return $ \mRest -> MMatch mScrut msAlts mRest

 , do   -- 'loop' Proc ...
        pTok KLoop
        mBody   <- pTerm ctx    <?> "the body of the loop"
        return  $ \mRest -> MLoop mBody mRest

  , do  -- 'while' TermArg (ProcStmt | ProcFinal) ...
        pTok KWhile
        mPred   <- pTermArg ctx <?> "the 'while' predicate"
        mBody   <- pTerm ctx    <?> "the body of the loop"
        return  $ \mRest -> MWhile mPred mBody mRest

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
                        m       <- pTerm ctx
                        return  $  MBind b mps ts m
        pTok KCKet
        return  $ \mRest -> MEnter mEnter bms mRest

 , P.try $ do
        -- Var '←' Term ...
        --   Update form without an initial keyword.
        --   We know this is an update when we get to the '←'.
        nCell   <- pVar
        pLeft
        mBind   <- pTerm ctx
        return  $ \mRest -> MUpdate nCell mBind mRest
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

         , do   return $ mkStmt $ MTerms [] ]

 , do   mFinal  <- pTerm ctx
        P.choice
         [ do   pTok KSemi
                mRest <- pProcDoStmts ctx
                return $ MSeq (MPTerms []) mFinal mRest
         , do   return $ mFinal ]
 ]

-- | Parser for a do-statement.
pProcDoStmt :: Context -> Parser (TermParams RL, Term RL)
pProcDoStmt ctx
 = P.choice
 [ P.try $ do
        -- '[' (Var : Type),* ']' = Term
        (rBinds, bs)
         <- pRanged $ pSquared $ flip P.sepEndBy (pTok KComma)
                        (pBind <?> "a binder")
        pTok KEquals    <?> "an '=' to start the body"
        mBody   <- pTerm ctx   <?> "the body of the statement"
        return  ( MPAnn rBinds $ MPTerms $ zip bs $ repeat THole
                , mBody)

 , do   -- Var '=' Term
        -- We need the lookahead here because plain terms
        -- in the next choice can also start with variable name.
        P.try $ P.lookAhead $ do
                pBind; pTok KEquals

        (rBind, nBind)
         <- pRanged $ (pBind  <?> "a binder")
        pTok KEquals        <?> "a '=' to start the body"
        mBody <- pTerm ctx  <?> "the body of the statement"
        return  (MPAnn rBind $ MPTerms [(nBind, THole)], mBody)

 , do   -- Term
        mBody   <- pTerm ctx
        return  (MPTerms [], mBody)
 ]


