
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


 , do   -- 'let' '[' Var,* ']' '=' Term ';' Term
        -- TODO: support none binders.
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
        pTok KSemi
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
         ([(BindNone, THole)], mBody) : bmsRest
           -> let (btsBind, msBind) = unzip $ reverse bmsRest
              in  return $ foldr (\(bts, m) m' -> MLet bts m m')
                                 mBody (zip btsBind msBind)

         []     -> P.unexpected "empty do block"
         _      -> P.unexpected "do block without result value"


 , do   -- Con TypeArg* TermArg*
        nCon    <- pCon
        pTermAppArgs (MCon nCon)


 , do   -- Prm TermArgs*
        nPrm    <- pPrm
        case takePrimValueOfName nPrm of
         Just v  -> pTermAppArgs (MVal v)
         Nothing -> pTermAppArgs (MPrm nPrm)


 , do   -- TermArg TermArgs*
        mFun    <- pTermArgProj
        pTermAppArgs mFun
 ]
 <?> "a term"


-- | Parse arguments to the given function,
--   returning the constructed application.
pTermAppArgs :: Term Location -> Parser (Term Location)
pTermAppArgs mFun
 = P.choice
 [ do   gsArgs  <- P.many1 pTermArgs
        return $ foldl MApp mFun gsArgs

 , do   return mFun]


-- | Parse some term arguments.
pTermArgs :: Parser (TermArgs Location)
pTermArgs
 = P.choice
 [ do   -- '@' '[' Type;+ ']'
        pTok KAt
        ts      <- pSquared $ P.sepEndBy pType (pTok KComma)
        return  $ MGTypes ts

 , do   -- '[' Term;+ ']'
        ms      <- pSquared $ P.sepEndBy pTerm (pTok KComma)
        return  $ MGTerms ms

 , do   -- TermProj
        m       <- pTermArgProj
        return  $ MGTerm m
 ]
 <?> "some term arguments"


pTermArgType :: Parser (Type Location)
pTermArgType
 = do   -- '@' Type
        pTok KAt
        t       <- pTypeArg
        return t
 <?> "a term argument"


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
 , do   pCon >>= return . MCon
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

 , do   -- '[record|' (Lbl '=' Term),* ']'
        -- '⟨' (Lbl '=' Term)* '⟩'
        -- Lookahead to distinguish record literals from the
        -- other collection literals below.
        pTermRecord

 , do   -- '[list|' Term,* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "list")

        pTok KSBra; pVar; pTok KBar
        msElem  <- P.sepEndBy1 pTerm (pTok KComma)
        pTok KSKet
        return  $ MList msElem


 , do   -- '[set|' Term,* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "set")

        pTok KSBra; pVar; pTok KBar
        msElem  <- P.sepEndBy1 pTerm (pTok KComma)
        pTok KSKet
        return  $ MSet msElem


 , do   -- '[map|' (Term ':=' Term),* ']'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "map")

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
 [ do   -- '[record' (Lbl '=' Term)* '⟩'
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; pTok KBar
                guard (n == Name "record")

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


-- TODO: use the binding forms in the let parser as well.
pTermStmt :: Parser ([(Bind, Type Location)], Term Location)
pTermStmt
 = P.choice
 [ do   -- '[' (Var : Type),* ']' = Term
        -- TODO: support none binders.
        bts     <- pSquared
                $ flip P.sepEndBy1 (pTok KComma)
                $ do  v  <- pVar
                      P.choice
                        [ do   pTok KColon; t <- pType; return (BindName v, t)
                        , do   return (BindName v, THole) ]
        pTok KEquals
        mBody   <- pTerm
        return  (bts, mBody)


 , do   -- Var '=' Term
        -- We need the lookahead here because plain terms
        -- in the next choice can also start with variable name.
        -- TODO: allow type sigs.
        P.try $ P.lookAhead $ do
                pVar
                pTok KEquals

        nVar    <- pVar
        pTok KEquals
        mBody   <- pTerm
        return  ([(BindName nVar, THole)], mBody)

 , do   -- Term
        mBody   <- pTerm
        return  ([(BindNone, THole)], mBody)
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

