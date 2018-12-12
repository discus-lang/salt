
module Salt.Core.Codec.Text.Lexer
        ( IW.Location(..)
        , scanner)
where
import Salt.Core.Codec.Text.Token
import Data.Text                        (Text)
import qualified Text.Lexer.Inchworm.Char as IW
import qualified Data.Char as Char
import qualified Data.Text as Text


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
           $ IW.munchPred Nothing (\_ix c -> isVarChar c)
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

        , fmap (stamp KNat) $ scanNat
        , fmap (stamp KInt) $ IW.scanInteger
        , fmap (stamp (KText . Text.pack)) $  IW.scanHaskellString
        ]
 where  -- Stamp a token with source location information.
        stamp k (l, t)
          = At l (k t)


scanVarName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanVarName
 = IW.munchPred Nothing match accept
 where   match 0 c = Char.isLower c
         match _ c = Char.isAlphaNum c || c == '\''
         accept cs = Just $ Text.pack cs
{-# INLINE scanVarName #-}


isVarChar :: Char -> Bool
isVarChar c = Char.isAlphaNum c || c == '\''


scanConName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanConName
 = IW.munchPred Nothing match accept
 where  match 0 c       = Char.isUpper c
        match _ c       = Char.isAlphaNum c || c == '\''
        accept cs       = Just $ Text.pack cs
{-# INLINE scanConName #-}


scanSymName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanSymName
 = IW.munchPred Nothing match accept
 where  match 0 c       = c == '\''
        match _ c       = Char.isAlphaNum c
        accept []       = Nothing
        accept (_ : cs) = Just $ Text.pack cs
{-# INLINE scanSymName #-}


scanPrmName :: Monad m => IW.Scanner m loc [Char] (loc, Text)
scanPrmName
 = IW.munchPred Nothing match accept
 where   match 0 c       = c == '#'
         match 1 c       = Char.isAlpha c
         match _ c       = Char.isAlphaNum c || c == '\''
         accept []       = Nothing
         accept (_ : cs) = Just $ Text.pack cs
{-# INLINE scanPrmName #-}


scanNat :: Monad m => IW.Scanner m loc [Char] (loc, Integer)
scanNat
 = IW.munchPred Nothing match accept
 where  match _ c = Char.isDigit c
        accept cs = Just (read cs)
{-# INLINE scanNat #-}

