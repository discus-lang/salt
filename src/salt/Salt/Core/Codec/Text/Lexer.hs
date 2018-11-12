
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
scanner :: FilePath -> IW.Scanner IO IW.Location [Char] (At Token)
scanner _fileName
 = IW.skip Char.isSpace
 $ IW.alts
        [ fmap (stamp (KComment . Text.pack)) $ IW.scanHaskellCommentLine
        , fmap (stamp (KComment . Text.pack)) $ IW.scanHaskellCommentBlock

        , fmap (stamp id)
           $ IW.munchPred Nothing (\_ix c -> elem c ['-', '>', ':', '='])
           $ \case
                "->"            -> Just KArrowRight
                "=>"            -> Just KArrowRightFat
                ":="            -> Just KColonEquals
                _               -> Nothing

        , fmap (stamp id) $ IW.accept '(' KRBra
        , fmap (stamp id) $ IW.accept ')' KRKet
        , fmap (stamp id) $ IW.accept '{' KCBra
        , fmap (stamp id) $ IW.accept '}' KCKet
        , fmap (stamp id) $ IW.accept '[' KSBra
        , fmap (stamp id) $ IW.accept ']' KSKet
        , fmap (stamp id) $ IW.accept '<' KABra
        , fmap (stamp id) $ IW.accept '>' KAKet
        , fmap (stamp id) $ IW.accept ':' KColon
        , fmap (stamp id) $ IW.accept ';' KSemi
        , fmap (stamp id) $ IW.accept ',' KComma
        , fmap (stamp id) $ IW.accept '.' KDot
        , fmap (stamp id) $ IW.accept '|' KBar
        , fmap (stamp id) $ IW.accept '@' KAt
        , fmap (stamp id) $ IW.accept '=' KEquals

        , fmap (stamp id) $ IW.accept '⟨' KABra
        , fmap (stamp id) $ IW.accept '⟩' KAKet
        , fmap (stamp id) $ IW.accept '→' KArrowRight
        , fmap (stamp id) $ IW.accept '⇒' KArrowRightFat
        , fmap (stamp id) $ IW.accept 'λ' KFun
        , fmap (stamp id) $ IW.accept '∀' KForall
        , fmap (stamp id) $ IW.accept '∃' KExists

        , fmap (stamp id)
           $ IW.munchPred Nothing (\_ix c -> isVarChar c)
           $ \case
                "type"          -> Just KType
                "term"          -> Just KTerm
                "test"          -> Just KTest

                "forall"        -> Just KForall
                "exists"        -> Just KExists

                "where"         -> Just KWhere

                "fun"           -> Just KFun
                "let"           -> Just KLet
                "in"            -> Just KIn
                "do"            -> Just KDo

                "if"            -> Just KIf
                "then"          -> Just KThen
                "else"          -> Just KElse

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

