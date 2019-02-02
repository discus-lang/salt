
module Salt.Core.Codec.Text.Token
        ( At            (..)
        , IW.Range      (..)
        , IW.Location   (..)
        , Token         (..)
        , showTokenAsSource
        , rangeOfTokenList)
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
        = KMetaEnd              -- signal end of input.
        | KMetaComment Text     -- comment text

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
        | KType         | KTerm         | KTest         | KWatch
        | KPure         | KSync
        | KThe          | KOf
        | KBox          | KRun
        | KLet          | KDo           | KWhere
        | KIf           | KCase         | KThen         | KElse
        | KSymForall    | KAsciiForall
        | KSymExists    | KAsciiExists

        | KProc
        | KYield        | KCall
        | KSeq          | KWith
        | KLaunch       | KReturn
        | KCell         | KUpdate
        | KWhen         | KMatch
        | KLoop         | KBreak        | KContinue

        | KBloc

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
        KMetaEnd        -> "END"
        KMetaComment tx -> "--" ++ T.unpack tx

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
        KWatch          -> "watch"
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
        KCase           -> "case"
        KThen           -> "then"
        KElse           -> "else"
        KSymForall      -> "∀";         KAsciiExists    -> "exists"
        KSymExists      -> "∃";         KAsciiForall    -> "forall"

        KProc           -> "proc"
        KYield          -> "yield"
        KCall           -> "call"
        KWith           -> "with"
        KSeq            -> "seq"
        KLaunch         -> "launch"
        KReturn         -> "return"
        KCell           -> "cell"
        KUpdate         -> "update"
        KWhen           -> "when"
        KMatch          -> "match"
        KLoop           -> "loop"
        KBreak          -> "break"
        KContinue       -> "continue"

        KBloc           -> "bloc"

        -- Names
        KVar  tx        -> T.unpack tx
        KCon  tx        -> T.unpack tx
        KSym  tx        -> "'" ++ T.unpack tx
        KPrm  tx        -> "#" ++ T.unpack tx

        -- Literals
        KNat  i         -> show i
        KInt  i         -> show i
        KText tx        -> show tx


-- | Get a range covering the location from the start of the first token,
--   to the end of the last token.
rangeOfTokenList :: [At t] -> IW.Range IW.Location
rangeOfTokenList toks
 = let  lFirst  = case toks of
                   At (IW.Range l _) _ : _ -> l
                   _ -> IW.Location 0 0

        lLast   = case Prelude.reverse toks of
                   At (IW.Range _ l) _ : _ -> l
                   _ -> IW.Location 0 0

   in   IW.Range lFirst lLast
