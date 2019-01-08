
module Salt.Core.Codec.Text.Parser.Term where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Prim.Values
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


------------------------------------------------------------------------------------------- Term --
-- | Parse a term, and wrap the result in an source location annotation.
pTerm  :: Parser (Term RL)
pTerm
 = pMAnn
 $ do   m <- pTermBody
        P.choice
         [ do   pTok KColon
                TGTypes ts <- pTypesHead
                 <?> "a type to ascribe"
                return  $ MThe ts m

         , do   P.lookAhead $ pTok KDot
                nsLabel <- P.many (do pTok KDot; pLbl <?> "a field label")
                return  $ foldl (flip MProject) m nsLabel

         , do   return  $ m ]


pTermBody  :: Parser (Term RL)
pTermBody
 = pMAnn $ P.choice
 [ do   -- 'the' Type 'of' '`' Lbl TermArg
        -- 'the' Type 'of' Term
        pTok KThe
        TGTypes ts <- pTypesHead
         <?> "a type to ascribe"

        -- For the variant case we can only attach a single type to the term
        -- AST. If we're going to get more than one then look ahead to see
        -- the problem so we can report the 'of' as the location of the error,
        -- rather than deeper in the variant syntax.
        bIsVariant
         <- P.lookAhead $ do
                pTok KOf <?> "'of' to start the body"
                P.choice [ do pTok KBacktick; return True
                         , return False ]

        when bIsVariant
         $ case ts of
                [_] -> return ()
                _   -> P.unexpected "type vector in variant annotation"

        pTok KOf <?> "'of' to start the body"
        P.choice
         [ do   pTok KBacktick
                t <- case ts of
                        [t] -> return t
                        _   -> fail "type vector in variant annotation"
                l <- pLbl <?> "a variant label"
                m <- P.choice
                        [ do    fmap MTerms
                                $ pSquared $ flip P.sepBy (pTok KComma)
                                        (pTerm <?> "an argument in the vector")
                        , do    m <- pTermArg
                                return m
                        ]
                  <?> "a argument term or vector for the variant body"
                return $ MVariant l m t

         , do   m <- pTerm  <?> "a body term"
                return $ MThe ts m
         ]
         <?> "a body term"


 , do   -- 'box' Term
        pTok KBox
        m  <- pTerm <?> "a term to box"
        return  $ MBox m


 , do   -- 'run' Term
        pTok KRun
        m  <- pTerm <?> "a term to run"
        return  $ MRun m


 , do   -- 'λ' TermParams+ '->' Term
        pFun
        mps  <- P.many1 $ (pTermParams
             <?> "some parameters, or a '→' to start the body")
        pRight           <?> "more paramaters, or a '→' to start the body"
        mBody <- pTerm   <?> "a body for the function"
        return  $ foldr MAbs mBody mps


 , do   -- 'let' '[' Var,* ']' '=' Term ';' Term
        pTok KLet
        bts     <- P.choice
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
                <?> "some binders"

        pTok KEquals    <?> "a type annotation, or '=' to start the binding"
        mBind <- pTerm  <?> "a term for the binding"
        pTok KSemi      <?> "a completed term, or ';' to start the body"
        mBody <- pTerm  <?> "a body term"
        return  $ MLet bts mBind mBody


  , do  -- 'do' '{' Stmt;* '}'
        --   This is sugar for let expressions that does not require
        --   all bindings to have a name, as we execute some for their
        --   actions only.
        pTok KDo

        -- Handle the KBra/KKet explicitly so we can detect missing final
        -- term before we get to the end of the block of statements.
        pTok KCBra
         <?> "a '{' to start the do-block"

        binds   <- (flip P.sepEndBy (pTok KSemi)
                        $ (pTermStmt <?> "another binding, or a result value"))
                <?> "some statements"

        m <- case reverse binds of
                ([], mBody) : bmsRest
                  -> let (bsBind, msBind) = unzip $ reverse bmsRest
                     in  return $ foldr (\(bs, m) m' -> MLet (zip bs $ repeat THole) m m')
                                        mBody (zip bsBind msBind)

                [] -> fail "Empty do block."
                _  -> fail "do block must have a result value."

        pTok KCKet      <?> "some statements, or a '}' to end the do-block"
        return m


 , do   -- 'if' Term 'then' Term 'else' Term
        -- 'if' '{' (Term '→' Term);* 'otherwise' '→' Term '}'
        pTok KIf
        P.choice
         [ do   pTok KCBra          <?> "a '{' to start the list of branches"
                (msCond, msThen)
                 <- fmap unzip $ P.many
                 $  do  mCond <- pTerm
                         <?> "a term for a condition, or 'otherwise' for the final branch"
                        pRight            <?> "a completed term, or '→' to start the body"
                        mThen <- pTerm    <?> "the body of the branch"
                        pTok KSemi        <?> "a completed term, or ';' to end the branch"
                        return (mCond, mThen)

                pTok KOtherwise     <?> "an 'otherwise' to start the default branch"
                pRight              <?> "a '→' to start the body"
                mDefault <- pTerm   <?> "the body of the branch"

                -- Allow a stray ';' after the otherwise,
                -- else it will be too annoying to swap branch order around.
                P.choice [ do pTok KSemi; return ()
                         , return ()]

                pTok KCKet          <?> "a completed term, or '}' to end the list of branches"
                return $ MIf msCond msThen mDefault

          , do  mCond   <- pTerm    <?> "a term for the condition"
                pTok KThen          <?> "a completed term, or 'then' to start the body"
                mThen   <- pTerm    <?> "the body of the 'then' branch"
                pTok KElse          <?> "a completed term, or 'else' to start the body"
                mElse   <- pTerm    <?> "the body of the 'else' branch"
                return  $ MIf [mCond] [mThen] mElse
         ]


 , do   -- 'case' Term 'of' '{' (Lbl Var ':' Type '→' Term)* '}'
        pTok KCase
        mScrut <- pTerm         <?> "a term for the scrutinee"
        pTok KOf                <?> "a completed term, or 'of' to start the alternatives"
        pTok KCBra              <?> "a '{' to start the list of alternatives"
        msAlts
         <- flip P.sepEndBy1 (pTok KSemi)
         $  do  lAlt    <- pLbl <?> " a variant label"

                btsPat  <- (pSquared
                        $  flip P.sepBy (pTok KComma)
                        $  do   b <- pBind  <?> "a binder for a variant field"
                                pTok KColon <?> "a ':' to specify the type of the field"
                                t <- pType  <?> "the type of the field"
                                return (b, t))
                        <?> "binders for the payload of the variant"

                pRight           <?> "a '→' to start the body"
                mBody <- pTerm   <?> "the body of the alternative"
                return $ MVarAlt lAlt btsPat mBody
        pTok KCKet               <?> "a completed term, or '}' to end the alternatives"
        return  $ MVarCase mScrut msAlts


 , do   -- Con TypeArg* TermArg*
        (rCon, nCon)  <- pRanged pCon
        pTermAppArgsSat (MAnn rCon $ MCon nCon)
         <?> "arguments for the constructor application"

 , do   -- Prm TermArgs*
        (rPrm, nPrm) <- pRanged pPrm
        case takePrimValueOfName nPrm of
         Just vPrm -> pTermAppArgsSat (MAnn rPrm (MVal vPrm))
                      <?> "arguments for the primitive application"

         Nothing   -> pTermAppArgsSat (MAnn rPrm (MPrm nPrm))
                      <?> "arguments for the primitive application"

 , do   -- TermArg TermArgs*
        mFun    <- pTermArgProj
        pTermAppArgs mFun
         <?> "arguments for the application"
 ]

--------------------------------------------------------------------------------------- App/Args --
-- | Parse arguments to the given function,
--   returning the constructed application.
pTermAppArgs :: Term RL -> Parser (Term RL)
pTermAppArgs mFun
 = pMAnn $ P.choice
 [ do   gsArgs  <- P.many1 (pTermArgs <?> "some arguments")
        return $ foldl MApp mFun gsArgs

 , do   return mFun]


-- | Parse arguments to the given function
--   returning a saturated primitive application.
pTermAppArgsSat :: Term RL -> Parser (Term RL)
pTermAppArgsSat mFun
 = pMAnn $ P.choice
 [ do   gsArgs  <- P.many1 (pTermArgs <?> "some arguments")
        return  $ MAps mFun gsArgs

 , do   return mFun ]


-- | Parse some term arguments.
pTermArgs :: Parser (TermArgs RL)
pTermArgs
 = pMGAnn $ P.choice
 [ do   -- '@' '[' Type;+ ']'
        -- '@' TypeArg
        pTok KAt
        P.choice
         [ do   ts <- pSquared
                    $ flip P.sepBy1 (pTok KComma)
                        (pType  <?> "a type for the argument")
                return $ MGTypes ts

         , do   t <- pTypeArg
                return $ MGTypes [t]
         ]
         <?> "a type argument or vector"

 , do   -- TermProj
        -- NOTE: This needs to come before the following case because
        --       collection terms like [list #Nat| 1, 2, 3]
        --       also start with open brackets.
        m       <- pTermArgProj
        return  $ MGTerm m

 , do   -- '[' Term;+ ']'
        ms <- pSquared
           $  flip P.sepBy (pTok KComma)
                (pTerm <?> "an argument")
        return  $ MGTerms ms
 ]


-- | Parser for a type argument.
pTermArgType :: Parser (Type RL)
pTermArgType
 = do   -- '@' Type
        pTok KAt
        t <- pTypeArg <?> "an argument type"
        return t


-- | Parser for a term argument or record projection.
pTermArgProj :: Parser (Term RL)
pTermArgProj
 = pMAnn
 $ do   mTerm   <- pTermArg
        nsLabel <- P.many
                $  do   pTok KDot
                        pLbl <?> "a field label"
        return  $  foldl (flip MProject) mTerm nsLabel


-- | Parser for a term argument.
pTermArg :: Parser (Term RL)
pTermArg
 = pMAnn $ P.choice
 [ do   -- Var
        -- Var ^ Nat
        n <- pVar
        P.choice
         [ do   pTok KHat
                b <- pNat <?> "the number of bump levels for the variable"
                return $ MVar $ BoundWith n b
         , do   return $ MVar $ BoundWith n 0 ]

 , do   -- Con
        pCon    >>= return . MCon

 , do   -- Syn
        pSym    >>= return . MSymbol

 , do   -- Nat
        pNat    >>= return . MNat

 , do   -- Text
        pText   >>= return . MText

        -- #Name
        -- This matches primitive values.
        -- Primitive operators that should be applied to arguments are
        -- handled by the general term parser.
 , do   (rPrm, nPrm) <- pRanged pPrm
        case takePrimValueOfName nPrm of
         Just v  -> return $ MAnn rPrm $ MVal v
         Nothing -> P.unexpected "primitive value"


 , do   -- '[list Type |' Term,* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pType; pTok KBar
                guard (n == Name "list")

        pTok KSBra; pVar; t <- pType; pTok KBar
        msElem  <- flip P.sepEndBy (pTok KComma)
                        (pTerm <?> "an element of the list")
        pTok KSKet <?> "a ']' to end the list"
        return  $ MList t msElem


 , do   -- '[set|' Term,* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pType; pTok KBar
                guard (n == Name "set")

        pTok KSBra; pVar; t <- pType; pTok KBar
        msElem  <- flip P.sepEndBy (pTok KComma)
                        (pTerm <?> "an element of the set")
        pTok KSKet <?> "a ']' to end the set"
        return  $ MSet t msElem


 , do   -- '[map|' (Term ':=' Term),* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTypeArg; pTypeArg; pTok KBar
                guard (n == Name "map")

        pTok KSBra; pVar
        tk <- pTypeArg; tv <- pTypeArg
        pTok KBar
        mmsElem <- flip P.sepEndBy (pTok KComma)
                $  do   m1 <- pTerm       <?> "a term for the key"
                        pTok KColonEquals <?> "a complete term, or ':=' to give the value"
                        m2 <- pTerm       <?> "a term for the value"
                        return (m1, m2)
        pTok KSKet <?> "a ']' to end the map"
        let (mks, mvs) = unzip mmsElem
        return  $ MMap tk tv mks mvs


 , do   -- '[record|' (Lbl '=' Term),* ']'
        -- '⟨' (Lbl '=' Term)* '⟩'
        pTermRecord


 , do   -- '[' Term,* ']'
        ms <- pSquared $ flip P.sepBy (pTok KComma)
                (pTerm <?> "a term")
        return  $ MTerms ms

 , do   -- '(' Term ')'
        pTok KRBra
        t <- pTerm <?> "a term"
        pTok KRKet
        return t
 ]


----------------------------------------------------------------------------------------- Params --
-- | Parser for some term parameters.
pTermParams :: Parser (TermParams RL)
pTermParams
 = pMPAnn $ P.choice
 [ do   -- '@' '[' (Var ':' Type)+ ']'
        pTok KAt
        bts     <- pSquared $ flip P.sepEndBy1 (pTok KComma)
                $  do   b <- pBind  <?> "a binder for the parameter"
                        pTok KColon <?> "a ':' to give the kind of the parameter"
                        t <- pType  <?> "the kind of the parameter"
                        return (b, t)
        return  $ MPTypes bts

 , do   -- '[' (Var ':' Type)* ']'
        bts     <- pSquared $ flip P.sepEndBy  (pTok KComma)
                $  do   b <- pBind  <?> "a binder for the parameter"
                        pTok KColon <?> "a ':' to give the type of the parameter"
                        t <- pType  <?> "the type of the parameter"
                        return (b, t)
        return  $ MPTerms bts
 ]


-- | Parser for a term binding.
pTermBind :: Parser (Bind, Term RL)
pTermBind
 = do   -- Var '=' Term
        nBind   <- pBind  <?> "a binder"
        pTok KEquals      <?> "a '=' to start the binding"
        mBody   <- pTerm  <?> "the bound term"
        return  (nBind, mBody)


------------------------------------------------------------------------------------------- Stmt --
-- | Parser for a statement.
pTermStmt :: Parser ([Bind], Term RL)
pTermStmt
 = P.choice
 [ do   -- '[' (Var : Type),* ']' = Term
        bs      <- pSquared $ flip P.sepEndBy (pTok KComma)
                        (pBind <?> "a binder")
        pTok KEquals     <?> "an '=' to start the body"
        mBody   <- pTerm <?> "the body of the statement"
        return  (bs, mBody)

 , do   -- Var '=' Term
        -- We need the lookahead here because plain terms
        -- in the next choice can also start with variable name.
        P.try $ P.lookAhead $ do
                pBind; pTok KEquals

        nBind <- pBind  <?> "a binder"
        pTok KEquals    <?> "a '=' to start the body"
        mBody <- pTerm  <?> "the body of the statement"
        return  ([nBind], mBody)

 , do   -- Term
        mBody   <- pTerm
        return  ([], mBody)
 ]


----------------------------------------------------------------------------------------- Record --
-- | Parser for a record.
pTermRecord :: Parser (Term RL)
pTermRecord
 = pMAnn $ P.choice
 [ do   -- '∏' '[' (Lbl '=' Term),* ']'
        pTok KSymProd
        lms     <- (pSquared $ flip P.sepEndBy (pTok KComma)
                $  do   l   <- pLbl  <?> "a label for the record field"
                        pTok KEquals <?> "a '=' to start the field"
                        m <- pTerm   <?> "a term for the field"
                        return (l, m))
                <?> "a vector of record fields"
        let (ls, ms) = unzip lms
        return $ MRecord ls ms


        -- '[record' (Lbl '=' Term),* ']'
  , do  P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "record")

        lms <- (pSquared $ do
                 pVar; pTok KBar
                 flip P.sepEndBy (pTok KComma)
                  $ (do l <- pLbl    <?> "a label for the record field"
                        pTok KEquals <?> "a '=' to start the field"
                        m <- pTerm   <?> "a term for the field"
                        return (l, m)))
            <?> "a vector of record fields"
        let (ls, ms) = unzip lms
        return $ MRecord ls ms


 , do   -- '[' (Lbl '=' Term)+ ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; pVar; pTok KEquals

        lms <- pSquared $ do
                 flip P.sepEndBy1 (pTok KComma)
                  $ do  l   <- pLbl  <?> "a label for the record field"
                        pTok KEquals <?> "a '=' to start the field"
                        m <- pTerm   <?> "a term for the field"
                        return (l, m)
        let (ls, ms) = unzip lms
        return $ MRecord ls ms
 ]


------------------------------------------------------------------------------------------ Value --
-- | Parser for a single value.
pValue :: Parser (Value Location)
pValue
 = P.choice
 [ do   pPrmOf "unit"  >> return VUnit
 , do   pPrmOf "true"  >> return VTrue
 , do   pPrmOf "false" >> return VFalse
 , do   pSym    >>= return . VSymbol
 , do   pNat    >>= return . VNat
 , do   pInt    >>= return . VInt
 , do   pText   >>= return . VText
 , do   pTermValueRecord ]


-- | Parser for a list of values, or a single value.
pValues :: Parser [Value Location]
pValues
 = P.choice
 [ do   -- '[' Value,* ']'
        pSquared $ flip P.sepEndBy (pTok KComma)
                 $ (pValue <?> "a value")

 , do   -- Value
        v <- pValue
        return [v]
 ]


-- | Parser for a record value.
pTermValueRecord :: Parser (Value Location)
pTermValueRecord
 = do   -- '[' (Lbl '=' Value)* ']'
        pTok KSBra
        lvs     <- flip P.sepEndBy1 (pTok KComma)
                $  do   l   <- pLbl    <?> "a field label"
                        pTok KEquals   <?> "a '=' to start the field"
                        vs  <- pValues <?> "some values"
                        return (l, vs)
        pTok KSKet
        return $ VRecord lvs


------------------------------------------------------------------------------------- Annotation --
pMAnn :: Parser (Term RL) -> Parser (Term RL)
pMAnn p
 = do   (r, m) <- pRanged p
        return $ MAnn r m


-- | Parse some type parameters wrapped in source range annotations.
pMPAnn :: Parser (TermParams RL) -> Parser (TermParams RL)
pMPAnn p
 = do   (r, tgs) <- pRanged p
        return $ MPAnn r tgs


-- | Parse some type arguments wrapped in source range annotations.
pMGAnn :: Parser (TermArgs RL) -> Parser (TermArgs RL)
pMGAnn p
 = do   (r, tgs) <- pRanged p
        return $ MGAnn r tgs

