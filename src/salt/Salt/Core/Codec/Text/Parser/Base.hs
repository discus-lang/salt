
module Salt.Core.Codec.Text.Parser.Base where
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Codec.Text.Pretty
import Salt.Core.Exp
import qualified Salt.Data.Pretty               as Pretty

import qualified Text.Lexer.Inchworm.Source     as IW
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P
import qualified Data.Int                       as I
import qualified Data.Word                      as W


------------------------------------------------------------------------------------------ Types --
-- | Generic type of parsers.
type Parser a   = P.Parsec [At Token] IW.Location a
type RL         = IW.Range IW.Location


------------------------------------------------------------------------------ Location Handling --
-- | Get the current positino in the source stream.
locHere :: Parser IW.Location
locHere
 = do   sp      <- P.getPosition
        let loc =  IW.Location (P.sourceLine sp) (P.sourceColumn sp)
        return  $ loc


-- | Get the position of the end of the last token.
locPrev :: Parser IW.Location
locPrev
 = do   loc     <- P.getState
        return  $ loc


-- | Get the location of a token.
locOfTok :: At Token -> P.SourcePos
locOfTok (At (IW.Range (Location l c) _) _)
 = P.newPos "file" l c


------------------------------------------------------------------------------ Primitive Parsers --
-- | Parse the given token.
pTok :: Token -> Parser ()
pTok t
 = do   Range _ lEnd
         <- P.token showTokenForError locOfTok
         $ \(At r t') -> if t == t' then Just r else Nothing
        P.putState lEnd

-- | Parse a token that matches the given function.
pTokOf :: (Token -> Maybe a) -> Parser a
pTokOf f
 = do   (Range _ lEnd, x)
         <- P.token showTokenForError locOfTok
         $ \(At r tok) -> case f tok of
                                Nothing -> Nothing
                                Just x  -> Just (r, x)
        P.putState lEnd
        return x


-- | Parse a thing, and also return its range in the source file.
pRanged :: Parser a -> Parser (Range Location, a)
pRanged p
 = do   lHere   <- locHere
        x       <- p
        lPrev   <- locPrev
        return  $ (Range lHere lPrev, x)


--------------------------------------------------------------------------------------- Wrapping --
-- | Parse a thing wrapped in braces.
pBraced :: Parser a -> Parser a
pBraced p
 = do   pTok KCBra; x <- p; pTok KCKet; return x


-- | Parse a thing wrapped in square brackets.
pSquared :: Parser a -> Parser a
pSquared p
 = do   pTok KSBra; x <- p; pTok KSKet; return x


-- | Parse a thing wrapped in angle brackets.
pAngled :: Parser a -> Parser a
pAngled p
 = do   pTok KABra; x <- p; pTok KAKet; return x


--------------------------------------------------------------------------------- Shared Parsers --
-- | Parser for a binder.
pBind :: Parser Bind
pBind    = P.choice
  [ BindName <$> pVar
  , const BindNone <$> pHole ]


------------------------------------------------------------------------------ Dual Form Parsers --
-- Parsers for logical tokens that have both unicode and ascii forms.
--  We treat them the same when parsing, but print the tokens back
--  in the original form in error messages.

pLeft :: Parser ()
pLeft   = P.choice [ pTok KSymLeft,     pTok KAsciiLeft ]

pRight :: Parser ()
pRight  = P.choice [ pTok KSymRight,    pTok KAsciiRight ]

pFatRight :: Parser ()
pFatRight = P.choice [ pTok KSymFatRight, pTok KAsciiFatRight ]

pFun :: Parser ()
pFun    = P.choice [ pTok KSymFun,      pTok KAsciiFun ]

pHole :: Parser ()
pHole   = P.choice [ pTok KSymHole,     pTok KAsciiHole]

pForall :: Parser ()
pForall = P.choice [ pTok KSymForall,   pTok KAsciiForall]

pExists :: Parser ()
pExists = P.choice [ pTok KSymExists,   pTok KAsciiExists]


----------------------------------------------------------------------------------- Name Parsers --
-- | Parser for a variable name.
pVar :: Parser Name
pVar    = pTokOf $ \case { KVar s -> Just (Name s); _ -> Nothing }

-- | Parser for a constructor name.
pCon :: Parser Name
pCon    = pTokOf $ \case { KCon s -> Just (Name s); _ -> Nothing }

-- | Parser for a symbol name.
pSym :: Parser Name
pSym    = pTokOf $ \case { KSym s -> Just (Name s); _ -> Nothing }

-- | Parser for a primitive name.
pPrm :: Parser Name
pPrm    = pTokOf $ \case { KPrm s -> Just (Name s); _ -> Nothing }

-- | Parser for a primitive with this specific name.
pPrmOf :: Text -> Parser ()
pPrmOf t = pTokOf $ \case { KPrm s | s == t -> Just (); _ -> Nothing }

-- | Parser for a record or variant label.
pLbl :: Parser Name
pLbl    = pTokOf $ \case { KVar s -> Just (Name s); _ -> Nothing }

-- | Parser for a natural number.
pNat :: Parser Integer
pNat    = pTokOf $ \case { KNat i -> Just i; _ -> Nothing }

-- | Parser for an integer.
pInt :: Parser Integer
pInt    = pTokOf $ \case { KInt i -> Just i; _ -> Nothing }

-- | Parser for a word.
pWord :: Parser Integer
pWord    = pTokOf $ \case { KWord i -> Just i; _ -> Nothing }

-- | Parser for an int8.
pInt8 :: Parser I.Int8
pInt8    = pTokOf $ \case { KInt8 i -> Just i; _ -> Nothing }

-- | Parser for an int16.
pInt16 :: Parser I.Int16
pInt16    = pTokOf $ \case { KInt16 i -> Just i; _ -> Nothing }

-- | Parser for an int32.
pInt32 :: Parser I.Int32
pInt32    = pTokOf $ \case { KInt32 i -> Just i; _ -> Nothing }

-- | Parser for an int64.
pInt64 :: Parser I.Int64
pInt64    = pTokOf $ \case { KInt64 i -> Just i; _ -> Nothing }

-- | Parser for a Word8.
pWord8 :: Parser W.Word8
pWord8    = pTokOf $ \case { KWord8 i -> Just i; _ -> Nothing }

-- | Parser for a Word16.
pWord16 :: Parser W.Word16
pWord16    = pTokOf $ \case { KWord16 i -> Just i; _ -> Nothing }

-- | Parser for a Word32.
pWord32 :: Parser W.Word32
pWord32    = pTokOf $ \case { KWord32 i -> Just i; _ -> Nothing }

-- | Parser for a Word64.
pWord64 :: Parser W.Word64
pWord64    = pTokOf $ \case { KWord64 i -> Just i; _ -> Nothing }

-- | Parser for a Haskell-style string.
pText :: Parser Text
pText   = pTokOf $ \case { KText t -> Just t; _ -> Nothing }


------------------------------------------------------------------------------------- Show Names --
-- | Show a label name for inclusion in a parser error message.
showLbl :: Name -> String
showLbl n = Pretty.render $ pprLbl n

-- | Show a variable name for inclusion in a parser error message.
showVar :: Name -> String
showVar n = Pretty.render $ pprVar n

-- | Show a binder for inclusion in a parser error message.
showBind :: Bind -> String
showBind (BindName n)   = showVar n
showBind BindNone       = "_"

-- | Show an unexpected token when constructing error messages.
showTokenForError :: At Token -> String
showTokenForError (At _ k)
 = "'" ++ showTokenAsSource k ++ "'"
