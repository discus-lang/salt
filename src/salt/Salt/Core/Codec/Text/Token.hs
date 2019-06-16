
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
import Data.Int         as I
import Data.Word        as W


-- | A thing with attached location information.
data At a
        = At (IW.Range IW.Location) a
        deriving Show


-- | A source token.
data Token
        -- Meta
        = KMetaStart            -- signal start of input
        | KMetaEnd              -- signal end of input.
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
        | KType         | KTerm         | KProc
        | KTest         | KWatch        | KEmit
        | KPure         | KSync
        | KThe          | KOf
        | KBox          | KRun
        | KLet          | KRec          | KIn           | KDo           | KWhere
        | KPrivate      | KExtend       | KUsing
        | KAlloc        | KRead         | KWrite
        | KIf           | KIfs          | KThen         | KElse
        | KCase
        | KSymForall    | KAsciiForall
        | KSymExists    | KAsciiExists
        | KPack         | KUnpack       | KAs

        | KSeq
        | KLaunch       | KReturn
        | KCell         | KUpdate
        | KWhen         | KWhens
        | KMatch
        | KLoop         | KBreak        | KContinue     | KWhile
        | KEnter        | KLeave        | KWith
        | KEnd

        -- Names
        | KVar  Text            -- Plain variable name, "foo"
        | KCon  Text            -- Constructor name,    "Foo"
        | KSym  Text            -- Symbol name,         "'Foo"
        | KPrm  Text            -- Primitive name       "#foo"

        -- Literals
        | KText    Text

        | KInt     Integer
        | KNat     Integer        -- Nat is an arbitrary sized natural number.
        | KWord    Integer

        | KInt8    I.Int8
        | KInt16   I.Int16
        | KInt32   I.Int32
        | KInt64   I.Int64

        | KWord8   W.Word8
        | KWord16  W.Word16
        | KWord32  W.Word32
        | KWord64  W.Word64

        deriving (Show, Eq)

-- | Show the token in source in source format.
--   This is used when printing tokens back in parse error messages.
showTokenAsSource :: Token -> String
showTokenAsSource kk
 = case kk of
        -- Meta
        KMetaStart      -> "START"
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
        KProc           -> "proc"
        KWatch          -> "watch"
        KEmit           -> "emit"

        KPure           -> "pure"
        KSync           -> "sync"
        KThe            -> "the"
        KOf             -> "of"
        KBox            -> "box"
        KRun            -> "run"
        KLet            -> "let"
        KPrivate        -> "private"
        KExtend         -> "extend"
        KAlloc          -> "Alloc"
        KRead           -> "Read"
        KWrite          -> "Write"
        KRec            -> "rec"
        KIn             -> "in"
        KDo             -> "do"
        KWhere          -> "where"
        KIf             -> "if"
        KIfs            -> "ifs"
        KCase           -> "case"
        KThen           -> "then"
        KElse           -> "else"
        KSymForall      -> "∀";         KAsciiForall    -> "forall"
        KSymExists      -> "∃";         KAsciiExists    -> "exists"
        KPack           -> "pack"
        KUnpack         -> "unpack"
        KAs             -> "as"

        KWith           -> "with"
        KUsing          -> "using"
        KSeq            -> "seq"
        KEnd            -> "end"
        KLaunch         -> "launch"
        KReturn         -> "return"
        KCell           -> "cell"
        KUpdate         -> "update"
        KWhen           -> "when"
        KWhens          -> "whens"
        KMatch          -> "match"
        KLoop           -> "loop"
        KBreak          -> "break"
        KContinue       -> "continue"
        KWhile          -> "while"
        KEnter          -> "enter"
        KLeave          -> "leave"

        -- Names
        KVar  tx        -> T.unpack tx
        KCon  tx        -> T.unpack tx
        KSym  tx        -> "'" ++ T.unpack tx
        KPrm  tx        -> "#" ++ T.unpack tx

        -- Literals
        KText    tx     -> show tx

        KNat     n      -> show n
        KInt     i      -> "#int'"    ++ show i
        KWord    i      -> "#word'"   ++ show i

        KInt8    i      -> "#int8'"   ++ show i
        KInt16   i      -> "#int16'"  ++ show i
        KInt32   i      -> "#int32'"  ++ show i
        KInt64   i      -> "#int64'"  ++ show i

        KWord8   w      -> "#word8'"  ++ show w
        KWord16  w      -> "#word16'" ++ show w
        KWord32  w      -> "#word32'" ++ show w
        KWord64  w      -> "#word64'" ++ show w


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
