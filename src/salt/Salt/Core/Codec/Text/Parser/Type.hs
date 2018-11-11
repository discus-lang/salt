
module Salt.Core.Codec.Text.Parser.Type where
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


-- | Parser for a type expression.
pType :: Parser (Type Location)
pType
 = P.choice
 [ do   -- 'λ' TypeParams '⇒' Type
        pTok KFun
        bks     <- pTypeParams
        pTok KArrowRightFat
        tBody   <- pType
        return  $  TAbs bks tBody

 , do   -- '∀' TypeParams '.' Type
        pTok KForall
        TPTypes bks <- pTypeParams
        pTok KDot
        tBody   <- pType
        return  $  TForall bks tBody

 , do   -- '∃' TypeParams '.' Type
        pTok KExists
        TPTypes bks <- pTypeParams
        pTok KDot
        tBody   <- pType
        return  $  TExists bks tBody

 , do   -- TypesHead '->' TypesResult
        -- TypesHead
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


-- | Parser for a type that can be used in the head of a function type.
pTypesHead :: Parser (TypeArgs Location)
pTypesHead
 = P.choice
 [ do   -- '{' Type+ '}'
        ts      <- pBraced $ P.sepEndBy pType (pTok KSemi)
        return  $ TGTypes ts

 , do   -- (Prm | TypeArg) TypeArg*
        tFun    <- P.choice
                [  do   pPrm >>= return . TPrm
                ,  do   pTypeArg ]

        P.choice
         [ do   -- '{' Type;+ '}'
                tsArgs  <- pBraced $ P.sepEndBy pType (pTok KSemi)
                return  $  TGTypes [TApt tFun tsArgs]

         , do   -- TypeArg*
                tsArgs  <- P.many pTypeArg
                case tsArgs of
                 []     -> return $ TGTypes [tFun]
                 _      -> return $ TGTypes [TApt tFun tsArgs]
         ]
 ]
 <?> "a head type"


-- | Parser that can be used as the result in a function type.
pTypesResult :: Parser [Type Location]
pTypesResult
 = do   TGTypes tsHead <- pTypesHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypesResult
                return [TFun tsHead tsResult]

         , do   return tsHead ]
 <?> "a result type"


-- | Parser for a type that can be used as the argument in a type-type application.
pTypeArg :: Parser (Type Location)
pTypeArg
 = P.choice
 [ do   -- Var
        pVar >>= return . TVar . Bound

 , do   -- Con
        pCon >>= return . TCon

 , do   -- Prm
        pPrm >>= return . TPrm

 , do   -- '[' (Lbl ':' Type)* ']'
        pTypeRecord

 , do   -- '(' Term ')'
        pTok KRBra
        t       <- pType
        pTok KRKet
        return t
 ]
 <?> "an argument type"


-- | Parser for some type parameters.
--   There needs to be at least one parameter because types
--   like  'λ{}.T' and '∀{}.T' aren't useful and we prefer not to worry
--   about needing to define them to be equal to 'T'.
pTypeParams :: Parser (TypeParams Location)
pTypeParams
 = P.choice
 [ do   -- '{' (Var ':' Type')+ '}'
        bts     <- pBraced $ flip P.sepEndBy1 (pTok KSemi)
                $  do n <- pVar; pTok KColon; t <- pType; return (BindName n, t)
        return  $ TPTypes bts
 ]
 <?> "type parameters"


-- | Parser for a record type.
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

