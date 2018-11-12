
module Salt.Core.Codec.Text.Parser.Term where
import Salt.Core.Codec.Text.Parser.Type
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Prim.Values
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                              ((<?>))
import qualified Text.Parsec                    as P


-- | Parse a term, and wrap the result in an source location annotation.
pTerm  :: Parser (Term Location)
pTerm
 = do   (Range l1 _, m)  <- pWithRange pTerm_
        return  $ MAnn l1 m

pTerm_  :: Parser (Term Location)
pTerm_
 = P.choice
 [ do   -- '[' Term,* ']'
        ms      <- pSquared $ P.sepEndBy pTerm (pTok KComma)
        return  $ MTerms ms


 , do   -- 'λ' TermParams '->' Term
        pTok KFun
        mps     <- pTermParams
        pTok KArrowRight
        mBody   <- pTerm
        case mps of
         MPTerms bts    -> return $ MAbm bts mBody
         MPTypes bts    -> return $ MAbt bts mBody


 , do   -- 'let' '[' Bind;+ ']' 'in' Term
        -- TODO: support none binders.
        -- TODO: support missing type sigs.
        pTok KLet
        bts     <- P.choice
                [ do    pSquared
                         $ flip P.sepEndBy1 (pTok KComma)
                         $  do  v  <- pVar
                                P.choice
                                 [ do   pTok KColon; t <- pType; return (BindName v, t)
                                 , do   return (BindName v, THole) ]

                , do    v       <- pVar
                        P.choice
                         [ do   pTok KColon; t <- pType; return [(BindName v, t)]
                         , do   return [(BindName v, THole)]]
                ]
        pTok KEquals
        mBind   <- pTerm
        pTok KIn
        mBody   <- pTerm
        return  $ MLet bts mBind mBody


  , do  -- 'do' '{' Stmt;* '}'
        --   This is sugar for let expressions that does not require
        --   all bindings to have a name, as we execute some for their
        --   actions only.
        --  TOOD: support type sigs on binders.
        pTok KDo
        binds   <- pBraced $ P.sepEndBy1 pTermStmt (pTok KSemi)
        case reverse binds of
         (BindNone, mBody) : bmsRest
           -> let (bsBind, msBind) = unzip $ reverse bmsRest
                  btsBind  = zip bsBind $ repeat THole
              in  return $ foldr (\(bt, m) m' -> MLet [bt] m m')
                                 mBody (zip btsBind msBind)

         []     -> P.unexpected "empty do block"
         _      -> P.unexpected "do block without result value"


 , do   -- Con TypeArg* TermArg*
        nCon    <- pCon

        tsArgs  <- P.choice
                [ do    P.many1 pTermArgType
                , do    return [] ]

        msArgs  <- pSquared $ P.sepEndBy pTerm (pTok KSemi)
--        msArgs  <- P.choice
--                [ do    pBraced $ P.sepEndBy1 pTerm (pTok KSemi)
--                , do    P.many pTermArgProj ]

        return  $ MCon nCon tsArgs msArgs


 , do   -- Prm TypeArg* TermArg*
        nPrm    <- pPrm

        tsArgs  <- P.choice
                [ do    P.many1 pTermArgType
                , do    return [] ]

        msArgs  <- P.choice
                [  do   pSquared $ P.sepEndBy pTerm (pTok KComma)
                ,       return [] ]

--      msArgs  <- P.choice
--              [ do    pBraced $ P.sepEndBy1 pTerm (pTok KSemi)
--              , do    P.many pTermArgProj ]

        case (takePrimValueOfName nPrm, tsArgs, msArgs) of
         (Just v,  [], []) -> return $ MVal v
         (Just v,  [], _)  -> return $ MApm (MVal v) msArgs
         (Just v,  _,  _)  -> return $ MApm (MApt (MVal v) tsArgs) msArgs
         (Nothing, _,  _)  -> return $ MPrim nPrm tsArgs msArgs


 , do   -- TermArg [Term; ...]
        -- TermArg TermArg
        mFun    <- pTermArgProj

        P.choice
         [ do   msArgs  <- pSquared $ P.sepEndBy pTerm (pTok KComma)
                return $ MApp mFun (MGTerms msArgs)

         , do   mArg    <- pTerm
                return $ MApp mFun (MGTerm  mArg)

         , do   return mFun
         ]
 ]
 <?> "a term"


pTermArgs :: Parser (TermArgs Location)
pTermArgs
 = P.choice
 [ do   -- '[' Term;+ ']'
        ms      <- pSquared $ P.sepEndBy pTerm (pTok KSemi)
        return  $ MGTerms ms

 , do   -- '@' '[' Type;+ ']'
        pTok KAt
        ts      <- pSquared $ P.sepEndBy pType (pTok KSemi)
        return  $ MGTypes ts

 , do   -- TermProj
        m       <- pTermArgProj
        return  $ MGTerm m
 ]


pTermArgType :: Parser (Type Location)
pTermArgType
 = do   -- '@' Type
        pTok KAt
        t       <- pTypeArg
        return t


pTermArgProj :: Parser (Term Location)
pTermArgProj
 = do   mTerm   <- pTermArg
        nsLabel <- P.many (do pTok KDot; pLbl)
        return  $  foldl (flip MProject) mTerm nsLabel
 <?> "a term or record projection"


pTermArg :: Parser (Term Location)
pTermArg
 = P.choice
 [ do   pVar >>= return . MVar . Bound
 , do   pCon >>= \n -> return $ MCon n [] []
 , do   pSym >>= return . MSymbol
 , do   pNat >>= return . MNat

        -- #name
        -- This matches primitive values.
        -- Primitive operators that should be applied to arguments are
        -- handled by the general term parser.
 , do   nPrm <- pPrm
        case takePrimValueOfName nPrm of
         Just v  -> return $ MVal v
         Nothing -> P.unexpected "primitive value"

 , do   -- '(' Term ')'
        pTok KRBra
        t       <- pTerm
        pTok KRKet
        return t

 , do   -- '[r|' (Lbl '=' Term),* ']'
        -- '⟨' (Lbl '=' Term)* '⟩'
        -- Lookahead to distinguish record literals from the
        -- other collection literals below.
        pTermRecord

 , do   -- '[l|' Term,* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "l")

        pTok KSBra; pVar; pTok KBar
        msElem  <- P.sepEndBy1 pTerm (pTok KComma)
        pTok KSKet
        return  $ MList msElem


 , do   -- '[s|' Term,* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "s")

        pTok KSBra; pVar; pTok KBar
        msElem  <- P.sepEndBy1 pTerm (pTok KComma)
        pTok KSKet
        return  $ MSet msElem


 , do   -- '[m|' (Term ':=' Term),* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "m")

        pTok KSBra; pVar; pTok KBar
        mmsElem <- flip P.sepEndBy1 (pTok KComma)
                $  do m1 <- pTerm; pTok KColonEquals; m2 <- pTerm; return (m1, m2)
        pTok KSKet
        let (mks, mvs) = unzip mmsElem
        return  $ MMap mks mvs
 ]
 <?> "a argument term"


pTermParams :: Parser (TermParams Location)
pTermParams
 = P.choice
 [ do   -- '[' (Var ':' Type')* ']'
        bts     <- pSquared $ flip P.sepEndBy (pTok KComma)
                $  do n <- pVar; pTok KColon; t <- pType; return (BindName n, t)
        return  $ MPTerms bts
 ]


pTermRecord :: Parser (Term Location)
pTermRecord
 = P.choice
 [ do   -- '[r' (Lbl '=' Term)* '⟩'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "s")

        pTok KSBra; pVar; pTok KBar

        lms <- P.sepEndBy
                ((do l   <- pLbl; pTok KEquals; m <- pTerm; return (l, m))
                 <?> "a record field")
                (pTok KComma)
        pTok KSKet
        let (ls, ms) = unzip lms
        return $ MRecord ls ms


 , do   -- '⟨' (Lbl '=' Term)* '⟩'
        pTok KABra
        lms <- P.sepEndBy
                ((do l   <- pLbl; pTok KEquals; m <- pTerm; return (l, m))
                 <?> "a record field")
                (pTok KComma)
        pTok KAKet
        let (ls, ms) = unzip lms
        return $ MRecord ls ms
 ]
 <?> "a record term"


pTermBind :: Parser (Bind, Term Location)
pTermBind
 = do   -- Var '=' Term
        nVar    <- pVar
        pTok KEquals
        mBody   <- pTerm
        return  (BindName nVar, mBody)
 <?> "a term binder"


pTermStmt :: Parser (Bind, Term Location)
pTermStmt
 = P.choice
 [ do   -- Var '=' Term
        -- We need the lookahead here because plain terms
        -- in the next choice can also start with variable name.
        P.try $ P.lookAhead $ do
                pVar
                pTok KEquals

        nVar    <- pVar
        pTok KEquals
        mBody   <- pTerm
        return  (BindName nVar, mBody)

 , do   -- Term
        mBody   <- pTerm
        return  (BindNone, mBody)
 ]
 <?> "a statement"


---------------------------------------------------------------------------------------------------
pValue :: Parser (Value Location)
pValue
 = P.choice
 [ do   pSym    >>= return . VSymbol
 , do   pNat    >>= return . VNat
 , do   pTermValueRecord ]


pTermValueRecord :: Parser (Value Location)
pTermValueRecord
 = do   -- '⟨' (Lbl '=' Value)* '⟩'
        pTok KSBra
        lvs <- P.sepEndBy1
                (do l   <- pLbl
                    pTok KEquals
                    v   <- pValue
                    return (l, v))
                (pTok KComma)
        pTok KSKet
        return $ VRecord lvs

