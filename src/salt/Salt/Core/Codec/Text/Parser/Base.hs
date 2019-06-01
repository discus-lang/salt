
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
import Debug.Trace


------------------------------------------------------------------------------------------ Types --
-- | Generic type of parsers.
type Parser a   = P.Parsec [At Token] State a
type RL         = IW.Range IW.Location

-- | Current parser state.
data State
        = State
        { -- | The location of the previously parsed token.
          statePrev             :: At Token

          -- | Synthetic tokens injected into the front of the stream.
        , stateInjected         :: [At Token]

          -- | The context that we are in when managing the offside rule.
        , stateOffside          :: [Offside] }
        deriving Show


-- | Context for handling of the offside rule.
data Offside
        = OffsideImplicit
        { offsideColumn         :: Int
        , offsideTokenStart     :: Token
        , offsideTokenSep       :: Token
        , offsideTokenEnd       :: Token }
        deriving Show


-- | Parser context carried down into the abstract syntax tree.
data Context
        = Context
        { contextParseTerm      :: Context -> Parser (Term RL)
        , contextParseTermApp   :: Context -> Parser (Term RL)
        , contextParseTermArg   :: Context -> Parser (Term RL) }


------------------------------------------------------------------------------ Location Handling --
-- | Get the current position in the source stream.
locHere :: Parser IW.Location
locHere
 = do   sp      <- P.getPosition
        let loc =  IW.Location (P.sourceLine sp) (P.sourceColumn sp)
        return  $ loc


-- | Get the position of the end of the last token.
locPrev :: Parser IW.Location
locPrev
 = do   At (Range loc _) _
         <- fmap statePrev P.getState
        return  $ loc


-- | Get the location of a token.
locOfTok :: At Token -> P.SourcePos
locOfTok (At (IW.Range (Location l c) _) _)
 = P.newPos "file" l c


------------------------------------------------------------------------------ Primitive Parsers --
-- | Parse the given token.
pTok :: Token -> Parser ()
pTok tMatch
 = pTokOf $ \tNext -> if tNext == tMatch then Just () else Nothing


-- | Parse a token that matches the given function.
pTokOf :: (Token -> Maybe a) -> Parser a
pTokOf fMatch
 = do   pInjectForContext
        fmap stateInjected P.getState
         >>= \case []  -> pTokOfInput fMatch
                   ats -> pTokOfInjected fMatch ats


-- | Parse a token from the input stream.
pTokOfInput :: (Token -> Maybe a) -> Parser a
pTokOfInput fMatch
 = do   (aTok, x)
         <- P.token showTokenForError locOfTok $ \aTok@(At _r tok)
         -> case fMatch tok of
               Nothing -> Nothing
               Just x  -> Just (aTok, x)

        P.modifyState $ \s -> s
                { statePrev = aTok }
        return x


-- | Parse a token that has been injected into the state.
pTokOfInjected :: (Token -> Maybe a) -> [At Token] -> Parser a
pTokOfInjected fMatch (aTok@(At _ tMagic) : atsRest)
 | Just x <- fMatch tMagic
 = do   P.modifyState $ \s -> s
                { statePrev     = aTok
                , stateInjected = atsRest }
        return x

pTokOfInjected _ _
 = P.parserZero


-- | Inject tokens for an implicit context.
pInjectForContext :: Parser ()
pInjectForContext
 = goStart
 where  -- See if we are in an implicit block context.
        goStart
         = do ctx <- fmap stateOffside  P.getState
              case ctx of
               OffsideImplicit nCol _tStart tSep tEnd : ctxRest
                 -> goImplicit nCol tSep tEnd ctxRest
               _ -> return ()

        -- Inject tokens for an implicit block context.
        goImplicit nCol tSep tEnd ctxRest
         = do   At (Range (Location nLinePrev _) _) _
                  <- fmap statePrev P.getState

                At (Range lHere@(Location nLineHere nColHere) _) tNext
                 <- P.lookAhead P.anyToken

                let rHere = Range lHere lHere

                -- On a new line at the block column, inject the separator.
                if      (nLineHere > nLinePrev) && (nColHere == nCol)
                 then P.modifyState $ \s -> s
                        { stateInjected = stateInjected s ++ [At rHere tSep]}

                -- On a new line to the left of the block column, inject the closing token.
                -- The new line may end multiple contexts, so after popping the first one
                -- try any others that might still be on the stack.
                else if (nLineHere > nLinePrev) && (nColHere <  nCol)
                 then do
                        P.modifyState $ \s -> s
                         { stateInjected = stateInjected s ++ [At rHere tEnd]
                         , stateOffside  = ctxRest }
                        goStart

                -- Also end blocks if we hit the end of the file.
                else if tNext == KMetaEnd
                 then do
                        P.modifyState $ \s -> s
                         { stateInjected = stateInjected s ++ [At rHere tEnd]
                         , stateOffside  = ctxRest }
                        goStart

                else    return ()

pDumpState :: Parser ()
pDumpState
 = do   atPrev  <- fmap statePrev     P.getState
        ctx     <- fmap stateOffside  P.getState
        ts      <- fmap stateInjected P.getState
        trace (unlines
                [ show atPrev
                , show ctx
                , show ts ]) $ return ()


-- | Enter into a possibly implicit block context.
--
--   We take the usual start, separator and end tokens.
--
--   If the next token in the stream is the same as the block start token
--   then this treat this as an explicit block and continue as normal.
--
--   Otherwise, push an implicit context record into the state so we know
--   that we now need to inject synthetic separator and block end tokens
--   based on layout.
--
pTokBlock :: Token -> Token -> Token -> Parser ()
pTokBlock tStart tSep tEnd
 = do
        -- Peek at the next token to see if we're entering the context explicitly.
        tok@(At (Range (Location _ col) _) tNext)
         <- P.lookAhead P.anyToken

        if tNext == tStart
        then    pTok tStart
        else do let ctx = OffsideImplicit col tStart tSep tEnd
                P.modifyState $ \s -> s
                        { statePrev     = tok
                        , stateOffside  = ctx : stateOffside s }


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


------------------------------------------------------------------------------------- Annotation --
pMAnn :: Parser (Term RL) -> Parser (Term RL)
pMAnn p
 = do   (r, m) <- pRanged p
        case m of
         MAnn{} -> return m
         _      -> return $ MAnn r m


-- | Parse some type parameters wrapped in source range annotations.
pMPAnn :: Parser (TermParams RL) -> Parser (TermParams RL)
pMPAnn p
 = do   (r, tgs) <- pRanged p
        return $ MPAnn r tgs


-- | Parse some type arguments wrapped in source range annotations.
pMGAnn :: Parser (TermArgs RL) -> Parser (TermArgs RL)
pMGAnn p
 = do   (r, tgs) <- pRanged p
        return $ MGAnn r tgs

