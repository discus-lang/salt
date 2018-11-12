
module Salt.Core.Codec.Text.Token
        ( At            (..)
        , Token         (..)
        , IW.Location   (..))
where
import qualified Text.Lexer.Inchworm.Source     as IW
import Data.Text


-- | A thing with attached location information.
data At a
        = At IW.Location a
        deriving Show


-- | A source token.
data Token
        -- Meta
        = KEnd                  -- signal end of input.
        | KComment Text         -- comment text

        -- Punctuation
        | KRBra   | KRKet       -- round bracket ()
        | KCBra   | KCKet       -- curley brackets {}
        | KSBra   | KSKet       -- square brackets []
        | KABra   | KAKet       -- angle  brackets <>
        | KColon  | KSemi
        | KComma  | KDot  | KBar | KAt
        | KEquals | KColonEquals
        | KArrowRight
        | KArrowRightFat
        | KLambda

        -- Keywords
        | KType   | KTerm  | KTest
        | KForall | KExists
        | KWhere  | KAnd   | KAs     | KTo
        | KFun    | KLet   | KIn     | KDo
        | KIf     | KThen  | KElse

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

