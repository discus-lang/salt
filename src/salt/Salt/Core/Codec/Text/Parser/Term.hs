
module Salt.Core.Codec.Text.Parser.Term where
import Salt.Core.Codec.Text.Parser.Params
import Salt.Core.Codec.Text.Parser.Proc
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Prim.Values
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


------------------------------------------------------------------------------------------ Terms --
-- | Parse a term vector.
pTerms :: Context -> Parser [Term RL]
pTerms ctx
 = P.choice
 [ do   pSquared $ flip P.sepEndBy1 (pTok KComma)
                 $ pTerm ctx
 , do   m <- pTerm ctx
        return [m]
 ]


------------------------------------------------------------------------------------------- Term --
-- | Parse a term, and wrap the result in a source location annotation.
pTerm  :: Context -> Parser (Term RL)
pTerm ctx
 = pMAnn
 $ do   m <- pTermBody ctx
        P.choice
         [ do   pTok KColon
                TGTypes ts <- pTypesHead
                 <?> "a type to ascribe"
                return  $ MThe ts m

         , do   P.lookAhead $ pTok KDot
                nsLabel <- P.many (do pTok KDot; pLbl <?> "a field label")
                return  $ foldl (flip MProject) m nsLabel

         , do   return  $ m ]


pTermBody :: Context -> Parser (Term RL)
pTermBody ctx
 = pMAnn $ P.choice
 [ do   -- 'λ' TermParams+ '->' Term
        pFun
        mps  <- P.many1 $ (pTermParams
             <?> "some parameters, or a '→' to start the body")
        pRight                  <?> "more paramaters, or a '→' to start the body"
        mBody <- pTerm ctx      <?> "a body for the function"
        return  $ foldr MAbs mBody mps

 , do   -- 'let' '[' Var,* ']' '=' Term 'in' Term
        pTok KLet
        (rBinds, bts) <- pTermBinds
        pTok KEquals            <?> "a type annotation, or '=' to start the binding"
        mBind <- pTerm ctx      <?> "a term for the binding"
        P.choice
         [ do   pTok KIn        <?> "a completed term, or 'in' to start the body"
                mBody <- pTerm ctx      <?> "a body term"
                return  $ MLet (MPAnn rBinds $ MPTerms bts) mBind mBody

         , do   pTok KSemi
                mBody <- pTerm ctx      <?> "a body term"
                return  $ MLet (MPAnn rBinds $ MPTerms bts) mBind mBody ]


  , do  -- 'private' Var 'with' '{' Cap;* '}' 'in' Term
        pTok KPrivate
        rName <- pVar           <?> "region name to bind"
        let rBound = BoundWith rName 0
        pTok KWith              <?> "'with' to start capability list"
        btsW <- pCaps rBound    <?> "capability list"
        pTok KIn                <?> "'in' to start the body"
        mBody <- pTerm ctx      <?> "a body term"
        let rBinding = (BindName rName, TRegion)
        return $ MPrivate [rBinding] btsW mBody


  , do  -- 'extend' Var 'using' Var 'with' '{' Cap;* '}' 'in' Term
        pTok KExtend
        rName1 <- pVar          <?> "existing region name to extend"
        pTok KUsing             <?> "'using' to start second region"
        rName2 <- pVar          <?> "new region name to introduce"
        pTok KWith              <?> "'with' to start capability list"
        let rBound2 = BoundWith rName2 0
        btsW <- pCaps rBound2   <?> "capability list"
        pTok KIn                <?> "'in' to start the body"
        mBody <- pTerm ctx      <?> "a body term"
        let rBound1 = TVar $ BoundWith rName1 0
        let rBinding2 = (BindName rName2, TRegion)
        return $ MExtend rBound1 [rBinding2] btsW mBody

  , do -- 'pack' Term 'with' '[' Type,* ']' 'as' Type
       pTok KPack
       term <- pTerm ctx   <?> "pack term"
       pTok KWith
       abstractedTypes <- pTypeVector <?> "vector of types to existentially erase"
       pTok KAs
       ascription <- pType <?> "pack type ascription"
       return $ MPack term abstractedTypes ascription

  , do -- 'unpack' Term 'as' Var 'with' '[' Var,* ']' 'in' Term
       pTok KUnpack
       mPacked <- pTerm ctx  <?> "unpack term"
       pTok KAs
       termName <- pVar      <?> "unpack term variable to bind"
       pTok KWith
       typeNames <- pSquared (flip P.sepEndBy (pTok KComma) pVar) <?> "vector of type variables to bind"
       pTok KIn
       mBody <- pTerm ctx    <?> "unpack body"
       let rTypeBindings = map (\n -> (BindName n, TType)) typeNames
       let rTermBinding = (BindName termName, TData)
       return $ MUnpack mPacked rTermBinding rTypeBindings mBody

  , do  -- 'rec' '{' (Bind TermParams* ':' Types '=' Term);+ '}' 'in' Term
        pTok KRec
        bms     <- pBraced $ flip P.sepEndBy1 (pTok KSemi)
                $ do    b       <- pBind
                        mps     <- P.many pTermParams
                        pTok KColon
                        ts      <- pTypes
                        pTok KEquals
                        m       <- pTerm ctx
                        return $ MBind b mps ts m
        pTok KIn
        mBody   <- pTerm ctx
        return  $ MRec bms mBody


  , do  -- 'the' Type 'of' '`' Lbl TermArg
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
                                        (pTerm ctx <?> "an argument in the vector")
                        , do    m <- pTermArg ctx
                                return m
                        ]
                  <?> "a argument term or vector for the variant body"
                return $ MVariant l m t

         , do   m <- pTerm ctx <?> "a body term"
                return $ MThe ts m
         ]
         <?> "a body term"


 , do   -- 'box' Term
        pTok KBox
        m  <- pTerm ctx <?> "a term to box"
        return  $ MBox m


 , do   -- 'run' Term
        pTok KRun
        m  <- pTerm ctx <?> "a term to run"
        return  $ MRun m


  , do  -- 'do' '{' Stmt;* '}'
        --   This is sugar for let expressions that does not require
        --   all bindings to have a name, as we execute some for their
        --   actions only.
        pTok KDo
        pTokBlock KCBra KSemi KCKet
         <?> "a '{' to start the do-block"

        binds   <- (flip P.sepEndBy (pTok KSemi)
                        $ (pProcDoStmt ctx <?> "another binding, or a result value"))
                <?> "some statements"

        m <- case reverse binds of
                (mps', mBody) : bmsRest
                 | Just [] <- takeMPTerms mps'
                  -> let (bsBind, msBind) = unzip $ reverse bmsRest
                     in  return $ foldr (\(mps, m) m' -> MLet mps m m')
                                        mBody (zip bsBind msBind)

                [] -> fail "Empty do block."
                _  -> fail "do block must have a result value."

        pTok KCKet      <?> "some statements, or a '}' to end the do-block"
        return m


 , do   -- 'if' Term 'then' Term 'else' Term
        pTok KIf
        mCond   <- pTerm ctx            <?> "a term for the condition"
        pTok KThen                      <?> "a completed term, or 'then' to start the body"
        mThen   <- pTerm ctx            <?> "the body of the 'then' branch"
        pTok KElse                      <?> "a completed term, or 'else' to start the body"
        mElse   <- pTerm ctx            <?> "the body of the 'else' branch"
        return  $ MIf [mCond] [mThen] mElse


 , do   -- 'ifs' '{' (Term '→' Term);* '}' else Term
        -- 'ifs' '{' (Term '→' Term);* 'else' '→' Term '}'
        pTok KIfs
        pTokBlock KCBra KSemi KCKet     <?> "a '{' to start the list of alternatives"
        (msCond, msThen)
         <- fmap unzip $ flip P.sepEndBy (pTok KSemi)
         $  do  mCond <- pTerm ctx
                 <?> "a term for a condition, or 'else' for the final branch"
                pRight                  <?> "a completed term, or '→' to start the body"
                mThen <- pTerm ctx      <?> "the body of the branch"
                return (mCond, mThen)

        P.choice
         [ do   -- ... 'else' → Term '}'
                pTok KElse
                pRight
                mElse <- pTerm ctx      <?> "the body of the branch"
                P.optional (pTok KSemi)
                pTok KCKet
                return $ MIf msCond msThen mElse

         , do   -- ... '}' else Term
                pTok KCKet
                pTok KElse
                mElse  <- pTerm ctx     <?> "the body of the branch"
                return $ MIf msCond msThen mElse
         ]


 , do   -- 'case' Term 'of' '{' (Lbl Var ':' Type '→' Term)* '}' ('else' Term)?
        pTok KCase
        mScrut <- pTerm ctx             <?> "a term for the scrutinee"
        pTok KOf                        <?> "a completed term, or 'of' to start the alternatives"
        pTokBlock KCBra KSemi KCKet     <?> "a '{' to start the list of alternatives"
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

                pRight             <?> "a '→' to start the body"
                mBody <- pTerm ctx <?> "the body of the alternative"
                return $ MVarAlt lAlt (MPAnn rPat $ MPTerms btsPat) mBody
        pTok KCKet               <?> "a completed term, or '}' to end the alternatives"

        P.choice
         [ do   pTok KElse
                mElse   <- pTerm ctx <?> "a term for the default alternative"
                return  $ MVarCase mScrut msAlts [mElse]

         , do   return  $ MVarCase mScrut msAlts [] ]

 , do   -- 'end'
        pTok KEnd
        return $ MTerms []

 , do   -- 'launch' Types of ...
        pTok KLaunch
        tsRet   <- pTypes
        pTok KOf
        mRest   <- pTerm ctx
        return  $ MLaunch tsRet mRest

 , do   -- 'return' Exp
        pTok KReturn
        mBody <- pTerm ctx
        return $ MReturn mBody

 , do   -- 'break'
        pTok KBreak
        return MBreak

 , do   -- 'continue'
        pTok KContinue
        return MContinue

 , do   -- 'leave'
        pTok KLeave
        return MLeave

 , do   -- 'do '{' ProcStmt; ... Term '}'
        pProcDo ctx

 , do   -- ProcStmt ((';' Proc) | ε)
        mkProc <- pProcStmt ctx
        P.choice
         [ do   pTok KSemi
                mRest   <- pTerm ctx
                return  $ mkProc mRest

         , do   return  $ mkProc $ MTerms [] ]


 , do   -- Con TypeArg* TermArg*
        -- Prm TermArgs*
        -- TermArg TermArgs*
        pTermApp ctx
 ]


--------------------------------------------------------------------------------------- App/Args --
-- | Parse an application expression.
pTermApp :: Context -> Parser (Term RL)
pTermApp ctx
 = P.choice
 [ do   -- Con TermArgs*
        (rCon, nCon)  <- pRanged pCon
        pTermAppArgsSat ctx (MAnn rCon $ MCon nCon)
         <?> "arguments for the constructor application"

 , do   -- Prm TermArgs*
        (rPrm, nPrm) <- pRanged pPrm
        case takePrimValueOfName nPrm of
         Just vPrm -> pTermAppArgsSat ctx (MAnn rPrm (MVal vPrm))
                      <?> "arguments for the primitive application"

         Nothing   -> pTermAppArgsSat ctx (MAnn rPrm (MPrm nPrm))
                      <?> "arguments for the primitive application"

 , do   -- TermArg TermArgs*
        mFun <- pTermArgProj ctx
        pTermAppArgsSat ctx mFun
         <?> "arguments for the application"
 ]


-- | Parse arguments to the given function,
--   returning the constructed application.
pTermAppArgs :: Context -> Term RL -> Parser (Term RL)
pTermAppArgs ctx mFun
 = pMAnn $ P.choice
 [ do   gsArgs  <- P.many1 (pTermArgs ctx <?> "some arguments")
        return $ foldl MApp mFun gsArgs

 , do   return mFun]


-- | Parse arguments to the given function
--   returning a saturated primitive application.
pTermAppArgsSat :: Context -> Term RL -> Parser (Term RL)
pTermAppArgsSat ctx mFun
 = pMAnn $ P.choice
 [ do   gsArgs  <- P.many1 (pTermArgs ctx <?> "some arguments")
        return  $ MAps mFun gsArgs

 , do   return mFun ]


-- | Parse some term arguments.
pTermArgs :: Context -> Parser (TermArgs RL)
pTermArgs ctx
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
        m <- pTermArgProj ctx
        return  $ MGTerm m

 , do   -- '[' Term;+ ']'
        ms <- pSquared
           $  flip P.sepBy (pTok KComma)
                (pTerm ctx <?> "an argument")
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
pTermArgProj :: Context -> Parser (Term RL)
pTermArgProj ctx
 = pMAnn
 $ do   mTerm   <- pTermArg ctx
        nsLabel <- P.many
                $  do   pTok KDot
                        pLbl <?> "a field label"
        return  $  foldl (flip MProject) mTerm nsLabel


-- | Parser for a term argument.
pTermArg :: Context -> Parser (Term RL)
pTermArg ctx
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

 , do   -- Int
        pInt    >>= return . MInt

 , do   -- Word
        pWord    >>= return . MWord

 , do   -- Int8
        pInt8     >>= return . MInt8
 , do   -- Int16
        pInt16    >>= return . MInt16
 , do   -- Int32
        pInt32    >>= return . MInt32
 , do   -- Int64
        pInt64    >>= return . MInt64

 , do   -- Word8
        pWord8     >>= return . MWord8
 , do   -- Word16
        pWord16    >>= return . MWord16
 , do   -- Word32
        pWord32    >>= return . MWord32
 , do   -- Word64
        pWord64    >>= return . MWord64

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
                        (pTerm ctx <?> "an element of the list")
        pTok KSKet <?> "a ']' to end the list"
        return  $ MList t msElem


 , do   -- '[set|' Term,* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pType; pTok KBar
                guard (n == Name "set")

        pTok KSBra; pVar; t <- pType; pTok KBar
        msElem  <- flip P.sepEndBy (pTok KComma)
                        (pTerm ctx <?> "an element of the set")
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
                $  do   m1 <- pTerm ctx   <?> "a term for the key"
                        pTok KColonEquals <?> "a complete term, or ':=' to give the value"
                        m2 <- pTerm ctx   <?> "a term for the value"
                        return (m1, m2)
        pTok KSKet <?> "a ']' to end the map"
        let (mks, mvs) = unzip mmsElem
        return  $ MMap tk tv mks mvs


 , do   -- '[record|' (Lbl '=' Term),* ']'
        -- '⟨' (Lbl '=' Term)* '⟩'
        pTermRecord ctx


 , do   -- '[' Term,* ']'
        ms <- pSquared $ flip P.sepBy (pTok KComma)
                (pTerm ctx <?> "a term")
        return  $ MTerms ms

 , do   -- '(' Term ')'
        pTok KRBra
        t <- pTerm ctx <?> "a term"
        pTok KRKet
        return t
 ]


----------------------------------------------------------------------------------------- Binder --
pTermBinds :: Parser (RL, [(Bind, Type RL)])
pTermBinds
 = do   (rBinds, bts)
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

        return (rBinds, bts)


-- | Parser for a term binding.
pTermBind :: Context -> Parser (Bind, Term RL)
pTermBind ctx
 = do   -- Var '=' Term
        nBind   <- pBind        <?> "a binder"
        pTok KEquals            <?> "a '=' to start the binding"
        mBody   <- pTerm ctx    <?> "the bound term"
        return  (nBind, mBody)


----------------------------------------------------------------------------------------- Record --
-- | Parser for a record.
pTermRecord :: Context -> Parser (Term RL)
pTermRecord ctx
 = pMAnn $ P.choice
 [ do   -- '∏' '[' (Lbl '=' Term),* ']'
        pTok KSymProd
        lms     <- (pSquared $ flip P.sepEndBy (pTok KComma)
                $  do   l   <- pLbl    <?> "a label for the record field"
                        pTok KEquals   <?> "a '=' to start the field"
                        m <- pTerm ctx <?> "a term for the field"
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
                  $ (do l <- pLbl      <?> "a label for the record field"
                        pTok KEquals   <?> "a '=' to start the field"
                        m <- pTerm ctx <?> "a term for the field"
                        return (l, m)))
            <?> "a vector of record fields"
        let (ls, ms) = unzip lms
        return $ MRecord ls ms


 , do   -- '[' (Lbl '=' Term)+ ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; pVar; pTok KEquals

        lms <- pSquared $ do
                 flip P.sepEndBy1 (pTok KComma)
                  $ do  l   <- pLbl    <?> "a label for the record field"
                        pTok KEquals   <?> "a '=' to start the field"
                        m <- pTerm ctx <?> "a term for the field"
                        return (l, m)
        let (ls, ms) = unzip lms
        return $ MRecord ls ms
 ]

------------------------------------------------------------------------------------------ Capabilities --
-- | Parser for a bracketed list of capabilities
pCaps :: Bound -> Parser [(Bind, Type a)]
pCaps rBound
 = pBraced $ flip P.sepEndBy (pTok KSemi) $ P.choice
     [ do   pTok KAlloc
            return (BindNone, TAlloc (TVar rBound))

     , do   pTok KRead
            return (BindNone, TRead (TVar rBound))

     , do   pTok KWrite
            return (BindNone, TWrite (TVar rBound))
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
 , do   pInt8    >>= return . VInt8
 , do   pInt16    >>= return . VInt16
 , do   pInt32    >>= return . VInt32
 , do   pInt64    >>= return . VInt64
 , do   pWord8    >>= return . VWord8
 , do   pWord16    >>= return . VWord16
 , do   pWord32    >>= return . VWord32
 , do   pWord64    >>= return . VWord64
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


