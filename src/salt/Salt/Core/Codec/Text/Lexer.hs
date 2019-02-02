
module Salt.Core.Codec.Text.Lexer
        ( lexSource
        , LexerError(..)
        , IW.Range (..), IW.Location(..)

        , scanner
        , checkMatch
        , matchVar
        , matchCon
        , matchPrm
        , matchSym )
where
import Salt.Core.Codec.Text.Token
import Control.Monad
import Data.Text                        (Text)
import qualified Text.Lexer.Inchworm.Char as IW
import qualified Data.Text                as Text
import qualified Data.Char                as Char
import qualified Data.Either              as Either
import qualified System.IO.Unsafe         as System


---------------------------------------------------------------------------------------------------
-- | Lexer error.
data LexerError
        = LexerError
        { errorLine     :: Int
        , errorColumn   :: Int
        , errorRest     :: String }
        deriving Show


-- | Lex a Salt source file.
lexSource :: String -> Either [LexerError] [At Token]
lexSource sSource
 = System.unsafePerformIO
 $ do   -- Break up the file into lines and lex each line at a time.
        --   There aren't any tokens that span lines, and doing it this way
        --   means we can produce multiple lexer errors at once.
        let ls = lines sSource
        esResult <- zipWithM lexLine [0..] ls
        case Either.partitionEithers esResult of
         ([], tokss)    -> return $ Right $ concat tokss
         (errs, _)      -> return $ Left errs


-- | Lex a single source line.
--
--   We take a line offset to add to any tokens and error messages produced.
lexLine :: Int -> String -> IO (Either LexerError [At Token])
lexLine nLineOffset sSource
 = do
        (toks, loc, strRest)
         <- IW.scanStringIO sSource scanner

        let IW.Location nLine nColumn = loc

        let bumpLoc   (IW.Location nLine' nColumn')
             =        (IW.Location (nLine' + nLineOffset) nColumn')

        let bumpRange (IW.Range locFirst locFinal)
             =        (IW.Range (bumpLoc locFirst) (bumpLoc locFinal))

        let bumpAt (At r t) = At (bumpRange r) t

        case strRest of
         [] -> return $ Right $ map bumpAt toks
         _  -> return $ Left  $ LexerError (nLineOffset + nLine) nColumn strRest


---------------------------------------------------------------------------------------------------
-- | Scanner for Salt.
scanner :: Monad m => IW.Scanner m IW.Location [Char] (At Token)
scanner
 = IW.skip Char.isSpace
 $ IW.alts
        [ -- Line comments.
          fmap (stamp (KMetaComment . Text.pack))
           $ IW.scanHaskellCommentLine

          -- Block comments.
        , fmap (stamp (KMetaComment . Text.pack))
           $ IW.scanHaskellCommentBlock

          -- Compound operators.
        , fmap (stamp id)
           $ IW.munchPred Nothing (\_ix c -> elem c ['<', '-', '>', ':', '='])
           $ \case
                "<-"    -> Just KAsciiLeft
                "->"    -> Just KAsciiRight
                "=>"    -> Just KAsciiFatRight
                ":="    -> Just KColonEquals
                _       -> Nothing

          -- Single character operators.
        , fmap (stamp id)
           $ IW.from $ \case
                '('     -> Just KRBra
                ')'     -> Just KRKet
                '{'     -> Just KCBra
                '}'     -> Just KCKet
                '['     -> Just KSBra
                ']'     -> Just KSKet
                '<'     -> Just KABra
                '>'     -> Just KAKet
                '@'     -> Just KAt
                '.'     -> Just KDot
                '|'     -> Just KBar
                '^'     -> Just KHat
                ';'     -> Just KSemi
                '!'     -> Just KBang
                '+'     -> Just KPlus
                ','     -> Just KComma
                ':'     -> Just KColon
                '='     -> Just KEquals
                '`'     -> Just KBacktick
                '←'     -> Just KSymLeft
                '→'     -> Just KSymRight
                '⇒'     -> Just KSymFatRight
                'λ'     -> Just KSymFun
                '∙'     -> Just KSymHole;       '_'   -> Just KAsciiHole
                '∑'     -> Just KSymSum
                '∏'     -> Just KSymProd
                '∀'     -> Just KSymForall
                '∃'     -> Just KSymExists
                _       -> Nothing

          -- Keywords.
        , fmap (stamp id)
           $ IW.munchPred Nothing (\_ix c -> isIdentChar c)
           $ \case
                "type"          -> Just KType
                "term"          -> Just KTerm
                "test"          -> Just KTest
                "watch"         -> Just KWatch

                "forall"        -> Just KAsciiForall
                "exists"        -> Just KAsciiExists
                "pure"          -> Just KPure
                "sync"          -> Just KSync

                "the"           -> Just KThe
                "of"            -> Just KOf

                "fun"           -> Just KAsciiFun
                "box"           -> Just KBox
                "run"           -> Just KRun

                "let"           -> Just KLet
                "do"            -> Just KDo
                "where"         -> Just KWhere

                "if"            -> Just KIf
                "case"          -> Just KCase
                "then"          -> Just KThen
                "else"          -> Just KElse

                "proc"          -> Just KProc
                "yield"         -> Just KYield
                "call"          -> Just KCall
                "seq"           -> Just KSeq
                "with"          -> Just KWith
                "launch"        -> Just KLaunch
                "return"        -> Just KReturn
                "cell"          -> Just KCell
                "update"        -> Just KUpdate
                "when"          -> Just KWhen
                "match"         -> Just KMatch
                "loop"          -> Just KLoop
                "break"         -> Just KBreak
                "continue"      -> Just KContinue

                "bloc"          -> Just KBloc

                _               -> Nothing

          -- Names.
        , fmap (stamp KVar) scanVarName
        , fmap (stamp KCon) scanConName
        , fmap (stamp KSym) scanSymName
        , fmap (stamp KPrm) scanPrmName
        , fmap (stamp id)   scanQuotedIdent

          -- Literals.
        , fmap (stamp KNat) $ scanNat
        , fmap (stamp KInt) $ IW.scanInteger
        , fmap (stamp (KText . Text.pack)) $ IW.scanHaskellString
        ]
 where  -- Stamp a token with source location information.
        stamp k (range, t)
          = At range (k t)


-- | Check if a Text value matches a predicate outside of the lexer.
--   This is used by the pretty-printer to decide whether to print an identifier
--   as a normal identifier, or if it must be quoted.
checkMatch :: (Int -> Char -> Bool) -> Text -> Bool
checkMatch match text
 = all (uncurry match) ([0..] `zip` Text.unpack text)


isIdentChar :: Char -> Bool
isIdentChar c = Char.isAlphaNum c || c == '\''
{-# INLINE isIdentChar #-}


scanVarName :: Monad m => IW.Scanner m loc [Char] (IW.Range loc, Text)
scanVarName
 = IW.munchPred Nothing matchVar (Just . Text.pack)
{-# INLINE scanVarName #-}


matchVar :: Int -> Char -> Bool
matchVar 0 c = Char.isLower c
matchVar _ c = isIdentChar c
{-# INLINE matchVar #-}


scanConName :: Monad m => IW.Scanner m loc [Char] (IW.Range loc, Text)
scanConName
 = IW.munchPred Nothing matchCon (Just . Text.pack)
{-# INLINE scanConName #-}


matchCon :: Int -> Char -> Bool
matchCon 0 c = Char.isUpper c
matchCon _ c = isIdentChar c
{-# INLINE matchCon #-}


scanSymName :: Monad m => IW.Scanner m loc [Char] (IW.Range loc, Text)
scanSymName
 = IW.munchPred Nothing matchSym acceptRequireLength2
{-# INLINE scanSymName #-}


acceptRequireLength2 :: [Char] -> Maybe Text.Text
acceptRequireLength2 []     = Nothing
acceptRequireLength2 [_]    = Nothing
acceptRequireLength2 (_:cs) = Just $ Text.pack cs
{-# INLINE acceptRequireLength2 #-}


matchSym :: Int -> Char -> Bool
matchSym 0 c = c == '\''
matchSym 1 c = Char.isAlpha c
matchSym _ c = isIdentChar c
{-# INLINE matchSym #-}


scanPrmName :: Monad m => IW.Scanner m loc [Char] (IW.Range loc, Text)
scanPrmName
 = IW.munchPred Nothing matchPrm acceptRequireLength2
{-# INLINE scanPrmName #-}


matchPrm :: Int -> Char -> Bool
matchPrm 0 c = c == '#'
matchPrm 1 c = Char.isAlpha c
matchPrm _ c = isIdentChar c
{-# INLINE matchPrm #-}


scanQuotedIdent :: Monad m => IW.Scanner m loc [Char] (IW.Range loc, Token)
scanQuotedIdent
 = wrap <$> IW.accepts "##" () <*> mKlass <*> IW.scanHaskellString
 where
  mKlass :: Monad m => IW.Scanner m loc [Char] (IW.Range loc, (Text -> Token))
  mKlass = IW.munchPred Nothing (\_ix c -> isIdentChar c) accept
  accept "Var" = Just KVar
  accept "Con" = Just KCon
  accept "Prm" = Just KPrm
  accept "Sym" = Just KSym
  accept _     = Nothing

  wrap (loc, _) (_, klass) (_, ident)
   = (loc, klass $ Text.pack ident)
{-# INLINE scanQuotedIdent #-}


scanNat :: Monad m => IW.Scanner m loc [Char] (IW.Range loc, Integer)
scanNat
 = IW.munchPred Nothing match accept
 where  match _ c = Char.isDigit c
        accept cs = Just (read cs)
{-# INLINE scanNat #-}

