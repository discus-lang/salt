
module Salt.Core.Codec.Text.Parser.Base where
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import qualified Text.Lexer.Inchworm.Source     as IW
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P


---------------------------------------------------------------------------------------------------
type Parser a   = P.Parsec [At Token] () a
data Range      = Range !IW.Location !IW.Location


-- | Get the current position in the input stream,
--   using the Inchworm location type that was originally
--   attached to the tokens.
getLocation :: Parser IW.Location
getLocation
 = do   sp      <- P.getPosition
        let loc =  IW.Location (P.sourceLine sp) (P.sourceColumn sp)
        return  $ loc


-- | Parse a thing, also returning the range from the source file.
pWithRange  :: Parser a -> Parser (Range, a)
pWithRange p
 = do   l1      <- getLocation
        x       <- p
        l2      <- getLocation
        return  (Range l1 l2, x)


pTok :: Token -> Parser ()
pTok t
 = P.token show locOfTok
 $ \(At _ t') -> if t == t' then Just () else Nothing


pTokOf :: (Token -> Maybe a) -> Parser a
pTokOf f
 = P.token show locOfTok
 $ \(At _ tok) -> f tok


locOfTok :: At Token -> P.SourcePos
locOfTok (At (Location l c) _)
 = P.newPos "file" l c


pBraced :: Parser a -> Parser a
pBraced p
 = do   pTok KCBra; x <- p; pTok KCKet; return x


pSquared :: Parser a -> Parser a
pSquared p
 = do   pTok KSBra; x <- p; pTok KSKet; return x


pAngled :: Parser a -> Parser a
pAngled p
 = do   pTok KABra; x <- p; pTok KAKet; return x


---------------------------------------------------------------------------------------------------
pVar :: Parser Name
pVar    = pTokOf $ \case { KVar s -> Just (Name s); _ -> Nothing }

pCon :: Parser Name
pCon    = pTokOf $ \case { KCon s -> Just (Name s); _ -> Nothing }

pSym :: Parser Name
pSym    = pTokOf $ \case { KSym s -> Just (Name s); _ -> Nothing }

pPrm :: Parser Name
pPrm    = pTokOf $ \case { KPrm s -> Just (Name s); _ -> Nothing }

pPrmOf :: Text -> Parser ()
pPrmOf t = pTokOf $ \case { KPrm s | s == t -> Just (); _ -> Nothing }

pLbl :: Parser Name
pLbl    = pTokOf $ \case { KVar s -> Just (Name s); _ -> Nothing }

pNat :: Parser Integer
pNat    = pTokOf $ \case { KNat i -> Just i; _ -> Nothing }

pInt :: Parser Integer
pInt    = pTokOf $ \case { KInt i -> Just i; _ -> Nothing }

pText :: Parser Text
pText   = pTokOf $ \case { KText t -> Just t; _ -> Nothing }

