-- Suppress warnings about Data.Monoid in GHC 8.2 -> 8.4 transition.
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Pretty printer utilities.
--
--   This is a re-export of Daan Leijen's pretty printer package (@wl-pprint@),
--   but with a `Pretty` class that includes a `pprPrec` function.
module Salt.Data.Pretty
        ( T.Textual(..)
        , T.Text
        , (T.%), (T.%%)
        , (<>)

        , Pretty        (..)
        , Monoid        (..)
        , P.Doc

        -- * Character documents
        , P.lparen,   P.rparen
        , P.langle,   P.rangle
        , P.lbrace,   P.rbrace
        , P.lbracket, P.rbracket
        , P.squote,   P.dquote
        , P.semi,     P.colon, P.comma, P.space, P.dot, P.backslash, P.equals

        -- * Bracketing combinators
        , P.enclose, P.parens, P.squotes, P.dquotes, P.angles, P.braces, P.brackets
        , P.encloseSep, P.tupled

        -- * Layout combinators
        , P.linebreak, P.softline, P.softbreak
        , P.nest,   P.align, P.hang, P.indent
        , P.pspace, P.pline, P.plinebreak, P.psoftline, P.psoftbreak
        , P.sep,    P.cat
        , P.punctuate
        , P.fill, P.width

        -- * Rendering
        , render
        , putDoc, putDocLn

        -- * Utils
        , padL
        , pprParen)
where
import Data.Set                                 (Set)
import Salt.Data.Textual                        ((%))
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import qualified Salt.Data.Textual              as T
import qualified Salt.Data.PrettyPrint          as P

-- With GHC <  8.4.1 we need Data.Monoid to get (<>)
-- With GHC >= 8.4.1 it comes in the prelude.
import Data.Monoid


-- Textual -------------------------------------------------------------------
instance T.Textual P.Doc where
 empty          = P.text ""
 paste  t1 t2   = P.hcat [t1, t2]
 pastes t1 t2   = P.hsep [t1, t2]
 line           = P.line
 char           = P.char
 string         = P.string
 text           = P.string . Text.unpack
 int            = P.int
 integer        = P.integer
 float          = P.float
 double         = P.double
 hcat           = P.hcat
 hsep           = P.hsep
 vcat           = P.vcat
 vsep           = P.vsep


-- Pretty Class --------------------------------------------------------------
class Pretty c a where
 ppr            :: c -> a -> P.Doc

instance Pretty c () where
 ppr _ x        = P.text $ show x

instance Pretty c Bool where
 ppr _ x        = P.text $ show x

instance Pretty c Int where
 ppr _ x        = P.text $ show x

instance Pretty c Integer where
 ppr _ x        = P.text $ show x

instance Pretty c Char where
 ppr _ x        = P.text $ show x

instance Pretty c a => Pretty c [a] where
 ppr c xs       = P.encloseSep P.lbracket P.rbracket P.comma
                $ map (ppr c) xs

instance Pretty c a => Pretty c (Set a) where
 ppr c xs       = P.encloseSep P.lbracket P.rbracket P.comma
                $ map (ppr c) $ Set.toList xs

instance (Pretty c a, Pretty c b) => Pretty c (a, b) where
 ppr c (a, b)   = P.parens $ ppr c a % P.comma % ppr c b


padL :: Int -> P.Doc -> P.Doc
padL n d
 = let  len     = length $ render d
        pad     = n - len
   in   if pad > 0
         then  d % P.text (replicate pad ' ')
         else  d


-- | Render a doc with the given mode.
render :: P.Doc -> String
render doc
 = P.displayS (P.renderPretty 0.8 100000 doc) ""


-- | Put a `Doc` to `stdout`
putDoc :: P.Doc -> IO ()
putDoc doc
        = putStr   $ render doc

-- | Put a `Doc` to `stdout`
putDocLn  :: P.Doc -> IO ()
putDocLn doc
        = putStrLn $ render doc


-- Utils ---------------------------------------------------------------------
-- | Wrap a `Doc` in parens if the predicate is true.
pprParen :: Bool -> P.Doc -> P.Doc
pprParen b c
 = if b then P.parens c
        else c


