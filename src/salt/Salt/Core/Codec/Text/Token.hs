
module Salt.Core.Codec.Text.Token
        ( At            (..)
        , IW.Location   (..)
        , Token         (..)
        , showTokenAsSource)
where
import qualified Text.Lexer.Inchworm.Source     as IW
import Data.Text        as T


-- | A thing with attached location information.
data At a
        = At IW.Location a
        deriving Show


-- | A source token.
--
--   TODO: add separate forms for unicode and sugared, like "→" and "->"
--         we want to be able to print tokens as written in error messages.
--
data Token
        -- Meta
        = KEnd                  -- signal end of input.
        | KComment Text         -- comment text

        -- Punctuation
        | KRBra   | KRKet       -- round bracket   ()
        | KCBra   | KCKet       -- curley brackets {}
        | KSBra   | KSKet       -- square brackets []
        | KABra   | KAKet       -- angle  brackets <>
        | KColon  | KSemi
        | KComma  | KDot  | KBar | KAt | KBacktick
        | KHole   | KHat
        | KEquals | KColonEquals
        | KArrowLeft
        | KArrowRight
        | KArrowRightFat
        | KLambda
        | KProd   | KSum
        | KBang   | KPlus

        -- Keywords
        | KType   | KTerm   | KTest
        | KForall | KExists | KPure | KSync
        | KThe    | KOf
        | KFun    | KBox    | KRun
        | KLet    | KDo     | KWhere
        | KIf     | KThen   | KElse
        | KCase   | KOtherwise

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
        KColon          -> ":"
        KSemi           -> ";"
        KComma          -> ","
        KDot            -> "."
        KBar            -> "|"
        KAt             -> "@"
        KBacktick       -> "`"
        KHole           -> "∙"
        KHat            -> "^"
        KEquals         -> "="
        KColonEquals    -> ":="
        KArrowLeft      -> "←"
        KArrowRight     -> "→"
        KArrowRightFat  -> "⇒"
        KLambda         -> "λ"
        KProd           -> "∏"
        KSum            -> "∑"
        KBang           -> "!"
        KPlus           -> "+"

        -- Keywords
        KType           -> "type"
        KTerm           -> "term"
        KTest           -> "test"
        KForall         -> "∀"
        KExists         -> "∃"
        KPure           -> "pure"
        KSync           -> "sync"
        KThe            -> "the"
        KOf             -> "of"
        KFun            -> "fun"
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

        -- Names
        KVar  tx        -> T.unpack tx
        KCon  tx        -> T.unpack tx
        KSym  tx        -> "'" ++ T.unpack tx
        KPrm  tx        -> "#" ++ T.unpack tx

        -- Literals
        KNat  i         -> show i
        KInt  i         -> show i
        KText tx        -> show tx

