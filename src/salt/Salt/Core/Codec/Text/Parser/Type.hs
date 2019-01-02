
module Salt.Core.Codec.Text.Parser.Type where
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Control.Monad
import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P


------------------------------------------------------------------------------------------- Type --
-- | Parser for a type expression.
pType :: Parser (Type Location)
pType
 = P.choice
 [ do   -- 'λ' TypeParams '⇒' Type
        pTok KFun
        bks <- pTypeParams  <?> "some parameters for the abstraction"
        pTok KArrowRightFat <?> "more parameters for the abstraction, or a '⇒' to start the body"
        tBody   <- pType    <?> "a body type for the abstraction"
        return $ TAbs bks tBody

 , do   -- '∀' TypeParams '.' Type
        pTok KForall
        TPTypes bks <- pTypeParams 
         <?> "some parameters for the forall type"
        pTok KDot           <?> "more parameters for the forall type, or a '.' to start the body"
        tBody <- pType      <?> "a body for the forall type"
        return  $  TForall bks tBody

 , do   -- '∃' TypeParams '.' Type
        pTok KExists
        TPTypes bks <- pTypeParams      
         <?> "some parameters for the exists type"
        pTok KDot           <?> "more type parameters, or a '.' to start the body type"
        tBody <- pType      <?> "a body for the exists type"
        return  $  TExists bks tBody

 , do   -- '∙'
        pTok KHole
        return THole

 , do   -- TypesHead '->' TypesResult
        -- TypesHead '=>' TypesResult
        -- TypesHead '!'  Type
        -- TypesHead '+'  Type
        -- TypesHead
        TGTypes tsHead <- pTypesHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypesResult    <?> "a result for the function type"
                return $ TFun tsHead tsResult

         , do   pTok KArrowRightFat
                tsResult <- pType           <?> "a result for the kind arrow"
                return $ TArr tsHead tsResult

         , do   pTok KBang      
                tResult <- pType            <?> "an effect for the suspension type"
                return $ TSusp tsHead tResult

         , do   pTok KPlus
                case tsHead of
                 [t] -> do
                        tResult <- pType    <?> "a component of the sum type"
                        return  $ TSum [t, tResult]
                 _   -> P.unexpected "type sequence used in sum type"

         , do   case tsHead of
                 [t]    -> return t
                 []     -> P.unexpected "empty type sequence"
                 _      -> P.unexpected "type sequence" ]
 ]


-- | Parser for a type that can be used in the head of a function type.
pTypesHead :: Parser (TypeArgs Location)
pTypesHead
 = P.choice
 [ do   -- (Prm | TypeArg) ( TypeArg+ | ('[' Type,+ ']') )*
        tFun    <- P.choice
                [  do   pPrm >>= return . TPrm
                ,  do   pTypeArg ]

        -- Take multiple TypeArgs at once to curry them together into a
        -- single application. Each occurrence of an uncurried form with
        -- square brackets is a separate application.
        tsArgs  <- P.many $ P.choice
                 -- TypeArg+
                 [ P.many1 (pTypeArg <?> "an argument type")

                 -- '[' Type,+ ']'
                 , pTypeVector
                 ]
                 
        let tApp = foldl TApt tFun tsArgs
        return $ TGTypes [tApp]

        -- '[' Type,+ ']'
 , do   ts      <- pTypeVector
        return  $ TGTypes ts
 ]


-- | Parser for a type that can be used as the result in a function type.
pTypesResult :: Parser [Type Location]
pTypesResult
 = do   TGTypes tsHead <- pTypesHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypesResult    <?> "a result type, or type vector"
                return [TFun tsHead tsResult]

         , do   pTok KBang
                tResult <- pType            <?> "an effect type"
                return [TSusp tsHead tResult]

         , do   return tsHead ]


---------------------------------------------------------------------------------------- TypeArg --
-- | Parser for a type that can be used as the argument in a type-type application.
pTypeArg :: Parser (Type Location)
pTypeArg
 = P.choice
 [ do   -- Var
        -- Var ^ Nat
        n <- pVar
        P.choice
         [ do   pTok KHat
                b <- pNat <?> "the number of bump levels for the variable"
                return  $ TVar $ BoundWith n b
         ,      return  $ TVar $ BoundWith n 0 ]

 , do   -- Con
        pCon >>= return . TCon

 , do   -- Prm
        pPrm >>= return . TPrm

        -- Record Types -------------------------
 , do   -- '∏' '[' (Lbl ':' Type)* ']'
        pTok KProd
        lts <- pSquared pTypeRecordFields
         <?> "fields for the record type"
        return $ TRecord (map fst lts) (map snd lts)

 , do   -- '[' 'record' '|' (Lbl ':' Type)* ']'
        -- lookahead as the '[' overlaps with type vector syntax from pTypesHead.
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; guard (n == "record"); pTok KBar

        pSquared $ do
                pVar; pTok KBar
                lts  <- pTypeRecordFields   <?> "fields for the record type"
                return $ TRecord (map fst lts) (map snd lts)

 , do   -- '[' (Lbl ':' Type)+ ']'
        -- lookahead as the '[' overlaps with type vector syntax from pTypesHead.
        P.try $ P.lookAhead $ do
                pTok KSBra; pVar; pTok KColon

        pSquared $ do
                lts  <- pTypeRecordFields    <?> "fields for the record type"
                return $ TRecord (map fst lts) (map snd lts)

        -- Variant Types ------------------------
 , do   -- '∑' '[' (Lbl ':' Type)* ']'
        pTok KSum
        lts <- pSquared pTypeVariantFields
         <?> "alternatives for the variant type"
        return $ TVariant (map fst lts) (map snd lts)

 , do   -- '[' 'variant' '|' (Lbl ':' Type)* ']'
        -- lookahead as the '[' overlaps with type vector syntax from pTypesHead.
        P.try $ P.lookAhead $ do
                pTok KSBra; n <- pVar; guard (n == "variant"); pTok KBar

        pSquared $ do
                pVar; pTok KBar
                lts  <- pTypeVariantFields
                 <?> "alternatives for the variant type"
                return $ TVariant (map fst lts) (map snd lts)

 , do   -- '<' (Lbl ':' Type)+ '>'
        pAngled $ do
                lts  <- pTypeVariantFields
                 <?> "alternatives for the variant type"
                return $ TVariant (map fst lts) (map snd lts)

        -- Effect Types -------------------------
        -- 'pure'
 , do   pTok KPure
        return TPure

        -- 'sync'
 , do   pTok KSync
        return TSync

 , do   -- '(' Term ')'
        pTok KRBra
        t <- pType
         <?> "a type"
        pTok KRKet
        return t
 ]


------------------------------------------------------------------------------------- TypeParams --
-- | Parser for some type parameters.
--   There needs to be at least one parameter because types
--   like  'λ[].T' and '∀[].T' aren't useful and we prefer not to worry
--   about needing to define them to be equal to 'T'.
pTypeParams :: Parser (TypeParams Location)
pTypeParams
 = do   bts     <- pTypeSigs
        return  $ TPTypes bts


-- | Parser for some type signatures.
pTypeSigs :: Parser [(Bind, Type Location)]
pTypeSigs
 = pSquared $ flip P.sepBy1 (pTok KComma)
 $ do   b <- pBind  <?> "a binder for a type parameter"
        pTok KColon <?> "a ':' to specify the kind of the type parameter"
        t <- pType  <?> "the kind of the type parameter '" ++ showBind b ++ "'"
        return (b, t)


---------------------------------------------------------------------------- TypeRecord / Vector --
-- | Parser for some record type fields.
pTypeRecordFields :: Parser [(Name, TypeArgs Location)]
pTypeRecordFields
 = flip P.sepBy (pTok KComma)
 $ do   n   <- pLbl             <?> "a record field label"
        pTok KColon             <?> "a ':' to specify the type of the record field"
        tgs <- pTypeArgsField   <?> "the type of the record field '" ++ showLbl n ++ "'"
        return (n, tgs)


-- | Parser for some variant type fields.
pTypeVariantFields :: Parser [(Name, TypeArgs Location)]
pTypeVariantFields
 = flip P.sepBy (pTok KComma)
 $ do   n   <- pLbl             <?> "a variant field label"
        pTok KColon             <?> "a ':' to specify the type of the variant field"
        tgs <- pTypeArgsField   <?> "the type of the variant field '" ++ showLbl n ++ "'"
        return (n, tgs)


-- | Parser for type args specified in a record or variant field.
pTypeArgsField :: Parser (TypeArgs Location)
pTypeArgsField
 = P.choice
 [ -- We need to try this first as [record| ] etc overlaps with the first
   -- part of the type vector syntax we try next.
   P.try $ do
        t <- pType
        return $ TGTypes [t]
         
 , do   pTok KSBra
        ts  <- P.sepBy
                (pType <?> "a component of the type vector, or ']' to end it") 
                (pTok KComma)        
        pTok KSKet
        return $ TGTypes ts
 ]

-- | Parser for a type vector.
pTypeVector :: Parser [Type Location]
pTypeVector 
 = do   pTok KSBra
        ts  <- P.sepBy
                (pType <?> "a component of the type vector, or ']' to end it") 
                (pTok KComma)        
        pTok KSKet
        return ts
