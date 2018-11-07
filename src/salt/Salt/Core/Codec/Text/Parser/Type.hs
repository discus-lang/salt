
module Salt.Core.Codec.Text.Parser.Type where
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


pType :: Parser (Type Location)
pType
 = P.choice
 [ do   -- λ TypeParams ⇒ Type
        pTok KFun
        bks     <- pTypeParams
        pTok KArrowRightFat
        tBody   <- pType
        return  $  TAbs bks tBody


 , do   -- TypesHead
        -- TypesHead '->' TypesResult
        TGTypes tsHead <- pTypesHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypesResult
                return $ TFun tsHead tsResult

         , do   case tsHead of
                 [t]    -> return t
                 []     -> P.unexpected "empty type sequence"
                 _      -> P.unexpected "type sequence" ]
 ]
 <?> "a type"


pTypesHead :: Parser (TypeArgs Location)
pTypesHead
 = P.choice
 [ do   -- Prm TypeArg*
        nPrm    <- pPrm

        P.choice
         [ do   -- Primitive constructor directly applied to an argument vector.
                tsArgs  <- pBraced $ P.sepEndBy pType (pTok KSemi)
                return  $  TGTypes [TApt (TPrm nPrm) tsArgs]

         , do   -- Primitive constructor applied to separate arguments.
                tsArgs  <- P.many pTypeArg
                case tsArgs of
                 []     -> return $ TGTypes [TPrm nPrm]
                 _      -> return $ TGTypes [TApt (TPrm nPrm) tsArgs]
         ]

 , do   --
        ts      <- pBraced $ P.sepEndBy1 pType (pTok KSemi)
        return  $ TGTypes ts

 , do   t       <- pTypeRecord
        return  $ TGTypes [t]


 , do   -- Type Type*
        tFun    <- pTypeArg
        tsArgs  <- P.many pTypeArg
        case tsArgs of
         []     -> return $ TGTypes [tFun]
         _      -> return $ TGTypes [TApt tFun tsArgs]
 ]
 <?> "a head type"


pTypesResult :: Parser [Type Location]
pTypesResult
 = do   TGTypes tsHead <- pTypesHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypesResult
                return [TFun tsHead tsResult]

         , do   return tsHead ]
 <?> "a result type"


pTypeArgs :: Parser (TypeArgs Location)
pTypeArgs
 = P.choice
 [ do   -- '{' Type;+ '}'
        ts      <- pBraced $ P.sepEndBy pType (pTok KSemi)
        return  $ TGTypes ts

 , do   t       <- pTypeArg
        return  $ TGTypes [t]
 ]


pTypeArg :: Parser (Type Location)
pTypeArg
 = P.choice
 [ do   pVar >>= return . TVar . Bound

 , do   -- Prm
        nPrm    <- pPrm
        return $ TPrm nPrm

 , do   -- Con
        nCon    <- pCon
        return $ TCon nCon

 , do   -- '[' (Lbl ':' Type)* ']'
        pTypeRecord

 , do   -- '(' Term ')'
        pTok KRBra
        t       <- pType
        pTok KRKet
        return t
 ]
 <?> "an argument type"


pTypeParams :: Parser (TypeParams Location)
pTypeParams
 = P.choice
 [ do   -- '{' (Var ':' Type')* '}'
        bts     <- pBraced $ flip P.sepEndBy1 (pTok KSemi)
                $  do n <- pVar; pTok KColon; t <- pType; return (BindName n, t)
        return  $ TPTypes bts
 ]


pTypeRecord :: Parser (Type Location)
pTypeRecord
 = do   -- '[' (Lbl ':' Type)* ']'
        pTok KSBra
        lts <- P.sepEndBy
                (do l   <- pLbl
                    pTok KColon
                    t   <- pType
                    return (l, t))
                (pTok KComma)
        pTok KSKet
        return $ TRecord (map fst lts) (map snd lts)
 <?> "a record type"


