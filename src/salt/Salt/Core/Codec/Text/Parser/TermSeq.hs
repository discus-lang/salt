
module Salt.Core.Codec.Text.Parser.TermSeq where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


-- | Parser for a term sequence.
pTermSeq :: Parser (Term RL) -> Parser (Term RL) -> Parser (Term RL)
pTermSeq pTerm pTermApp
 = P.choice
 [ do   -- 'let' '[' Var,* ']' '=' Term TermSeqRest
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
        pTok KSemi        <?> "a completed term, or ';' to start the body"
        mBody <- pTermSeq pTerm pTermApp
        return  $ MLet (MPAnn rBinds (MPTerms bts)) mBind mBody


 , do   -- proc sequence forms, with a continuing chain
        -- 'if' '{' (Term '→' Term);+ '}' TermSeqRest
        -- 'if' Term 'then' Term TermSeqRest

        -- term forms
        -- 'if' '{' (Term '→' Term);* '}' 'else' Term
        -- 'if' '{' (Term '→' Term);* 'else' '→' Term '}'
        -- 'if' Term 'then' Term 'else' Term

        pTok KIf
        P.choice
         [ do   pTok KCBra          <?> "a '{' to start the list of branches"
                (msCond, msThen)
                 <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
                 $  do  mCond <- pTerm
                         <?> "a term for a condition, or 'else' for the final branch"
                        pRight            <?> "a completed term, or '→' to start the body"
                        mThen <- pTerm    <?> "the body of the branch"
                        return (mCond, mThen)

                P.choice
                 [ do   -- ... 'else' Term '}'
                        pTok KElse
                        mElse <- pTerm    <?> "the body of the branch"
                        P.optional (pTok KSemi)
                        pTok KCKet
                        return $ MIf msCond msThen mElse

                 , do   -- ... '}' else Term
                        pTok KCKet
                        pTok KElse
                        mElse  <- pTerm     <?> "the body of the branch"

                        return $ MIf msCond msThen mElse

                 , do   -- .. TermSeqRest
                        mRest   <- P.choice [ pTermSeqRest pTerm pTermApp
                                            , pTermSeq     pTerm pTermApp]

                        return $ MProcIf msCond msThen mRest
                 ]

          , do  mCond   <- pTerm    <?> "a term for the condition"
                pTok KThen          <?> "a completed term, or 'then' to start the body"
                mThen   <- pTerm    <?> "the body of the 'then' branch"

                P.choice
                 [ do   -- ... 'else' Term
                        pTok KElse
                        mElse <- pTerm  <?> "the body of the 'else' branch"
                        return $ MIf [mCond] [mThen] mElse

                 , do   -- ... TermSeqRest
                        mRest   <- P.choice [ pTermSeqRest pTerm pTermApp
                                            , pTermSeq     pTerm pTermApp]
                        return $ MProcIf [mCond] [mThen] mRest
                 ]
         ]

 , do   -- 'case' Term 'of' '{' (Lbl Var ':' Type '→' Term)* '}'
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

                pRight           <?> "a '→' to start the body"
                mBody <- pTerm   <?> "the body of the alternative"
                return $ MVarAlt lAlt (MPAnn rPat $ MPTerms btsPat) mBody
        pTok KCKet               <?> "a completed term, or '}' to end the alternatives"

        mRest   <- P.choice [ pTermSeqRest pTerm pTermApp
                            , pTermSeq     pTerm pTermApp]

        return  $ MProcCase mScrut msAlts mRest


 , do   -- 'loop' Term ';' TermSeq
        pTok KLoop
        mBody   <- pTerm

        mRest   <- P.choice [ pTermSeqRest pTerm pTermApp
                            , pTermSeq     pTerm pTermApp ]

        return  $ MProcLoop mBody mRest


 , do   -- 'cell' Var ':' Type '←' Term ';' TermSeq
        pTok KCell
        nCell   <- pVar
        pTok KColon
        tCell   <- pType
        pLeft
        mBind   <- pTerm
        mRest   <- pTermSeqRest pTerm pTermApp
        return  $ MProcCell nCell tCell mBind mRest


 , do   -- 'Var' '←' Term ';' TermSeq
        P.lookAhead $ do
                pVar; pLeft

        nCell   <- pVar
        pLeft
        mValue  <- pTerm
        mRest   <- pTermSeqRest pTerm pTermApp
        return  $  MProcAssign nCell mValue mRest


 , do   -- TermApp (';' TermSeq)
        mApp    <- pTermApp
        P.choice
         [ do   pTok KSemi
                P.choice
                 [ do   P.lookAhead $ pTok KEnd
                        return mApp

                 , do   mRest <- pTermSeq pTerm pTermApp
                        return $ MLet (MPTerms []) mApp mRest ]

         , do   P.lookAhead $ pTok KEnd
                return mApp
         ]

  , do  -- Term
        mTerm   <- pTerm
        return mTerm
 ]


-- | Parser for the rest of a term sequence.
--
--   Formally this should be  (';' TermSeq), but we're flexible in the case that
--   the separating ';' is omitted just before a sequence end. In this case we
--   treat it as if the sequence was ended with an empty term vector.
--
pTermSeqRest :: Parser (Term RL) -> Parser (Term RL) -> Parser (Term RL)
pTermSeqRest pTerm pTermApp
 = P.choice
 [ do   pTok KSemi
        P.choice
         [ do   P.lookAhead $ pTok KEnd
                return $ MTerms []
         , do   pTermSeq pTerm pTermApp]

 , do   P.lookAhead $ pTok KEnd
        return $ MTerms []
 ]

