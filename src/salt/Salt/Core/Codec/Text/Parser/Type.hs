
module Salt.Core.Codec.Text.Parser.Type where
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Control.Monad
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

 , do   -- '∙'
        pTok KHole
        return THole

 , do   -- TypesHead '->' TypesResult
        -- TypesHead '=>' TypesResult
        -- TypesHead '!'  Type
        -- TypesHead
        TGTypes tsHead <- pTypesHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypesResult
                return $ TFun tsHead tsResult

         , do   pTok KArrowRightFat
                tsResult <- pType
                return $ TArr tsHead tsResult

         , do   pTok KBang
                tResult <- pType
                return $ TSusp tsHead [tResult]

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
 [ do   -- (Prm | TypeArg) TypeArg*
        tFun    <- P.choice
                [  do   pPrm >>= return . TPrm
                ,  do   pTypeArg ]

        P.choice
         [ do   -- '[' Type;+ ']'
                tsArgs  <- pSquared $ P.sepEndBy pType (pTok KComma)
                return  $  TGTypes [TApt tFun tsArgs]

         , do   -- TypeArg*
                tsArgs  <- P.many pTypeArg
                case tsArgs of
                 []     -> return $ TGTypes [tFun]
                 _      -> return $ TGTypes [TApt tFun tsArgs]
         ]

        -- '[' Type+ ']'
 , do   ts      <- pSquared $ P.sepEndBy pType (pTok KComma)
        return  $ TGTypes ts
 ]
 <?> "a head type"


-- | Parser for a type that can be used as the result in a function type.
pTypesResult :: Parser [Type Location]
pTypesResult
 = do   TGTypes tsHead <- pTypesHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypesResult
                return [TFun tsHead tsResult]

         , do   pTok KBang
                tResult <- pType
                return [TSusp tsHead [tResult]]

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

        -- Record Types -------------------------
 , do   -- '∏' '[' (Lbl ':' Type)* ']'
        pTok KProd
        lts     <- pSquared pTypeFields
        return $ TRecord (map fst lts) (map snd lts)

 , do   -- '[' 'record' '|' (Lbl ':' Type)* ']'
        -- lookahead as the '[' overlaps with type vector syntax from pTypesHead.
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; guard (n == "record"); pTok KBar

        pSquared $ do
                pVar; pTok KBar
                lts  <- pTypeFields
                return $ TRecord (map fst lts) (map snd lts)

 , do   -- '[' (Lbl ':' Type)+ ']'
        -- lookahead as the '[' overlaps with type vector syntax from pTypesHead.
        P.try $ P.lookAhead $ do
                pTok KSBra; pVar; pTok KColon

        pSquared $ do
                lts  <- pTypeFields
                return $ TRecord (map fst lts) (map snd lts)

        -- Variant Types ------------------------
 , do   -- '∑' '[' (Lbl ':' Type)* ']'
        pTok KSum
        lts     <- pSquared pTypeFields
        return $ TVariant (map fst lts) (map snd lts)

 , do   -- '[' 'variant' '|' (Lbl ':' Type)* ']'
        -- lookahead as the '[' overlaps with type vector syntax from pTypesHead.
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; guard (n == "variant"); pTok KBar

        pSquared $ do
                pVar; pTok KBar
                lts  <- pTypeFields
                return $ TVariant (map fst lts) (map snd lts)

 , do   -- '<' (Lbl ':' Type)+ '>'
        pAngled $ do
                lts  <- pTypeFields
                return $ TVariant (map fst lts) (map snd lts)

 , do   -- '(' Term ')'
        pTok KRBra
        t       <- pType
        pTok KRKet
        return t
 ]
 <?> "an argument type"


-- | Parser for some type parameters.
--   There needs to be at least one parameter because types
--   like  'λ[].T' and '∀[].T' aren't useful and we prefer not to worry
--   about needing to define them to be equal to 'T'.
pTypeParams :: Parser (TypeParams Location)
pTypeParams
 = do   bts     <- pTypeSigs
        return  $ TPTypes bts
 <?> "type parameters"


-- | Parser for some type signatures.
pTypeSigs :: Parser [(Bind, Type Location)]
pTypeSigs
 = pSquared
        $ flip P.sepEndBy (pTok KComma)
        $ do b <- pBind; pTok KColon; t <- pType; return (b, t)


-- | Parser for some type fields.
pTypeFields :: Parser [(Name, TypeArgs Location)]
pTypeFields
 = flip P.sepEndBy (pTok KComma)
 $ do   n       <- pLbl
        pTok KColon
        ts      <- P.choice
                [ P.try $ do
                        t  <- pType
                        return $ TGTypes [t]

                , do    ts <- pSquared $ P.sepEndBy pType (pTok KComma)
                        return $ TGTypes ts
                ]
        return (n, ts)

