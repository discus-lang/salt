
module Salt.Core.Codec.Text.Token
        ( At            (..)
        , IW.Range      (..)
        , IW.Location   (..)
        , Token         (..)
        , showTokenAsSource)
where
import qualified Text.Lexer.Inchworm.Source     as IW
import Data.Text        as T


-- | A thing with attached location information.
data At a
        = At (IW.Range IW.Location) a
        deriving Show


-- | A source token.
data Token
        -- Meta
        = KEnd                  -- signal end of input.
        | KComment Text         -- comment text

        -- Punctuation
        | KRBra         | KRKet       -- round bracket   ()
        | KCBra         | KCKet       -- curley brackets {}
        | KSBra         | KSKet       -- square brackets []
        | KABra         | KAKet       -- angle  brackets <>
        | KAt
        | KDot
        | KBar
        | KHat
        | KSemi
        | KBang
        | KPlus
        | KComma
        | KColon
        | KEquals
        | KBacktick
        | KColonEquals
        | KSymLeft      | KAsciiLeft
        | KSymRight     | KAsciiRight
        | KSymFatRight  | KAsciiFatRight
        | KSymFun       | KAsciiFun
        | KSymHole      | KAsciiHole
        | KSymSum
        | KSymProd

        -- Keywords
        | KType         | KTerm         | KTest
        | KPure         | KSync
        | KThe          | KOf
        | KBox          | KRun
        | KLet          | KDo           | KWhere
        | KIf           | KThen         | KElse
        | KCase         | KOtherwise
        | KSymForall    | KAsciiForall
        | KSymExists    | KAsciiExists

        -- Names
        | KVar  Text            -- Plain variable name, "foo"
        | KCon  Text            -- Constructor name,    "Foo"
        | KSym  Text            -- Symbol name,         "'Foo"
        | KPrm  Text            -- Primitive name       "#foo"

        -- Literals
        | KNat  Integer
        | KInt  Integer
        | KText Text
        deriving (Show, Eq)


-- | Show the token in source in source format.
--   This is used when printing tokens back in parse error messages.
showTokenAsSource :: Token -> String
showTokenAsSource kk
 = case kk of
        -- Meta
        KEnd            -> "END"
        KComment tx     -> "--" ++ T.unpack tx

        -- Punctuation
        KRBra           -> "("
        KRKet           -> ")"
        KCBra           -> "{"
        KCKet           -> "}"
        KSBra           -> "["
        KSKet           -> "]"
        KABra           -> "<"
        KAKet           -> ">"
        KAt             -> "@"
        KDot            -> "."
        KBar            -> "|"
        KHat            -> "^"
        KSemi           -> ";"
        KBang           -> "!"
        KPlus           -> "+"
        KComma          -> ","
        KColon          -> ":"
        KEquals         -> "="
        KBacktick       -> "`"
        KColonEquals    -> ":="
        KSymLeft        -> "←";         KAsciiLeft      -> "<-"
        KSymRight       -> "→";         KAsciiRight     -> "->"
        KSymFatRight    -> "⇒";         KAsciiFatRight  -> "=>"
        KSymFun         -> "λ";         KAsciiFun       -> "fun"
        KSymHole        -> "∙";         KAsciiHole      -> "_"
        KSymSum         -> "∑"
        KSymProd        -> "∏"

        -- Keywords
        KType           -> "type"
        KTerm           -> "term"
        KTest           -> "test"
        KPure           -> "pure"
        KSync           -> "sync"
        KThe            -> "the"
        KOf             -> "of"
        KBox            -> "box"
        KRun            -> "run"
        KLet            -> "let"
        KDo             -> "do"
        KWhere          -> "where"
        KIf             -> "if"
        KThen           -> "then"
        KElse           -> "else"
        KCase           -> "case"
        KOtherwise      -> "otherwise"
        KSymForall      -> "∀";         KAsciiExists    -> "exists"
        KSymExists      -> "∃";         KAsciiForall    -> "forall"

        -- Names
        KVar  tx        -> T.unpack tx
        KCon  tx        -> T.unpack tx
        KSym  tx        -> "'" ++ T.unpack tx
        KPrm  tx        -> "#" ++ T.unpack tx

        -- Literals
        KNat  i         -> show i
        KInt  i         -> show i
        KText tx        -> show tx

