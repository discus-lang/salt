
module Salt.Core.Codec.Text.Lexer
        ( IW.Location(..)
        , scanner
        , checkMatch
        , matchVar
        , matchCon
        , matchPrm
        , matchSym )
where
import Salt.Core.Codec.Text.Token
import Data.Text                        (Text)
import qualified Text.Lexer.Inchworm.Char as IW
import qualified Data.Char                as Char
import qualified Data.Text                as Text


-- | Scanner for Salt.
--   TODO: bake filename into tokens.
scanner :: Monad m => FilePath -> IW.Scanner m IW.Location [Char] (At Token)
scanner _fileName
 = IW.skip Char.isSpace
 $ IW.alts
        [ fmap (stamp (KComment . Text.pack)) $ IW.scanHaskellCommentLine
        , fmap (stamp (KComment . Text.pack)) $ IW.scanHaskellCommentBlock

        , fmap (stamp id)
           $ IW.munchPred Nothing (\_ix c -> elem c ['<', '-', '>', ':', '='])
           $ \case
                "<-"    -> Just KArrowLeft
                "->"    -> Just KArrowRight
                "=>"    -> Just KArrowRightFat
                ":="    -> Just KColonEquals
                _       -> Nothing

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
                ':'     -> Just KColon
                ';'     -> Just KSemi
                ','     -> Just KComma
                '.'     -> Just KDot
                '|'     -> Just KBar
                '@'     -> Just KAt
                '='     -> Just KEquals
                '`'     -> Just KBacktick
                '_'     -> Just KHole
                '!'     -> Just KBang

                '∙'     -> Just KHole
                '⟨'     -> Just KABra
                '⟩'     -> Just KAKet
                '∏'     -> Just KProd
                '∑'     -> Just KSum
                'λ'     -> Just KFun
                '∀'     -> Just KForall
                '∃'     -> Just KExists
                '←'     -> Just KArrowLeft
                '→'     -> Just KArrowRight
                '⇒'     -> Just KArrowRightFat

                _       -> Nothing

        , fmap (stamp id)
           $ IW.munchPred Nothing (\_ix c -> isIdentChar c)
           $ \case
                "type"          -> Just KType
                "term"          -> Just KTerm
                "test"          -> Just KTest

                "forall"        -> Just KForall
                "exists"        -> Just KExists
                "the"           -> Just KThe
                "as"            -> Just KAs

                "where"         -> Just KWhere

                "fun"           -> Just KFun
                "let"           -> Just KLet
                "do"            -> Just KDo

                "if"            -> Just KIf
                "then"          -> Just KThen
                "else"          -> Just KElse

                "case"          -> Just KCase
                "of"            -> Just KOf
                "otherwise"     -> Just KOtherwise

                "box"           -> Just KBox
                "run"           -> Just KRun

                _               -> Nothing

        , fmap (stamp KVar) scanVarName
        , fmap (stamp KCon) scanConName
        , fmap (stamp KSym) scanSymName
        , fmap (stamp KPrm) scanPrmName

        , fmap (stamp id) scanQuotedIdent

        , fmap (stamp KNat) $ scanNat
        , fmap (stamp KInt) $ IW.scanInteger
        , fmap (stamp (KText . Text.pack)) $  IW.scanHaskellString
        ]
 where  -- Stamp a token with source location information.
        stamp k (l, t)
          = At l (k t)

-- | Check if a Text value matches a predicate outside of the lexer.
-- This is used by the pretty-printer to decide whether to print an identifier
-- as a normal identifier, or if it must be quoted.
checkMatch :: (Int -> Char -> Bool) -> Text -> Bool
checkMatch match text
 = all (uncurry match) ([0..] `zip` Text.unpack text)

isIdentChar :: Char -> Bool
isIdentChar c = Char.isAlphaNum c || c == '\''
{-# INLINE isIdentChar #-}


scanVarName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanVarName
 = IW.munchPred Nothing matchVar (Just . Text.pack)
{-# INLINE scanVarName #-}

matchVar :: Int -> Char -> Bool
matchVar 0 c = Char.isLower c
matchVar _ c = isIdentChar c
{-# INLINE matchVar #-}


scanConName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanConName
 = IW.munchPred Nothing matchCon (Just . Text.pack)
{-# INLINE scanConName #-}

matchCon :: Int -> Char -> Bool
matchCon 0 c = Char.isUpper c
matchCon _ c = isIdentChar c
{-# INLINE matchCon #-}


scanSymName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanSymName
 = IW.munchPred Nothing matchSym acceptRequireLength2
{-# INLINE scanSymName #-}

acceptRequireLength2 :: [Char] -> Maybe Text.Text
acceptRequireLength2 []     = Nothing
acceptRequireLength2 [_]    = Nothing
acceptRequireLength2 (_:cs) = Just $ Text.pack cs

matchSym :: Int -> Char -> Bool
matchSym 0 c = c == '\''
matchSym 1 c = Char.isAlpha c
matchSym _ c = isIdentChar c
{-# INLINE matchSym #-}


scanPrmName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanPrmName
 = IW.munchPred Nothing matchPrm acceptRequireLength2
{-# INLINE scanPrmName #-}

matchPrm :: Int -> Char -> Bool
matchPrm 0 c = c == '#'
matchPrm 1 c = Char.isAlpha c
matchPrm _ c = isIdentChar c
{-# INLINE matchPrm #-}


scanQuotedIdent :: Monad m => IW.Scanner m loc [Char] (loc, Token)
scanQuotedIdent
 = wrap <$> IW.accepts "##" () <*> mKlass <*> IW.scanHaskellString
 where
  mKlass :: Monad m => IW.Scanner m loc [Char] (loc, (Text -> Token))
  mKlass = IW.munchPred Nothing (\_ix c -> isIdentChar c) accept
  accept "Var" = Just KVar
  accept "Con" = Just KCon
  accept "Prm" = Just KPrm
  accept "Sym" = Just KSym
  accept _     = Nothing

  wrap (loc, _) (_, klass) (_, ident) = (loc, klass $ Text.pack ident)
{-# INLINE scanQuotedIdent #-}


scanNat :: Monad m => IW.Scanner m loc [Char] (loc, Integer)
scanNat
 = IW.munchPred Nothing match accept
 where  match _ c = Char.isDigit c
        accept cs = Just (read cs)
{-# INLINE scanNat #-}

