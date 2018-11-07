
module Salt.Core.Codec.Text.Parser.Type where
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp
import qualified Salt.Core.Prim     as Prim

import Text.Parsec                      ((<?>))
import qualified Text.Parsec            as P
import qualified Data.Map.Strict        as Map


pType :: Parser (Type Location)
pType
 = P.choice
 [ do   -- TypeHead
        -- TypeHead '->' Type
        TGTypes tsHead <- pTypeHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypeResults
                return $ TFun tsHead tsResult

         , do   case tsHead of
                 [t]    -> return t
                 []     -> P.unexpected "empty type sequence"
                 _      -> P.unexpected "type sequence" ]
 ]
 <?> "a type"


pTypeHead :: Parser (TypeArgs Location)
pTypeHead
 = P.choice
 [ do   nCon    <- pCon
        tsArgs  <- P.many pTypeArg

        let mkPrim = Map.lookup nCon Prim.primTypeCtors
        case (mkPrim, tsArgs) of
         (Just _,   _) -> return $ TGTypes [TPrim nCon tsArgs]
         (Nothing, []) -> return $ TGTypes [TCon nCon]
         (Nothing, _)  -> return $ TGTypes [TApp (TCon nCon) tsArgs]

 , do   ts      <- pBraced $ P.sepEndBy1 pType (pTok KSemi)
        return  $ TGTypes ts
 ]
 <?> "a head type"


pTypeResults :: Parser [Type Location]
pTypeResults
 = do   TGTypes tsHead <- pTypeHead
        P.choice
         [ do   pTok KArrowRight
                tsResult <- pTypeResults
                return [TFun tsHead tsResult]

         , do   return tsHead ]
 <?> "a result type"


pTypeArg :: Parser (Type Location)
pTypeArg
 = P.choice
 [ do   pVar >>= return . TVar . Bound

 , do   -- Con
        nCon    <- pCon
        let mkPrim = Map.lookup nCon Prim.primTypeCtors
        case mkPrim of
         Nothing -> return $ TCon nCon
         Just _  -> return $ TPrim nCon []

 , do   -- '[' (Lbl ':' Type)* ']'
        pTypeRecord
 ]
 <?> "an argument type"


pTypeRecord :: Parser (Type Location)
pTypeRecord
 = do   -- '[' (Lbl ':' Type)* ']'
        pTok KSBra
        lts <- P.sepEndBy1
                (do l   <- pLbl
                    pTok KColon
                    t   <- pType
                    return (l, t))
                (pTok KComma)
        pTok KSKet
        return $ TRecord (map fst lts) (map snd lts)
 <?> "a record type"


