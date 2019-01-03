
module Salt.Data.Ranges
        ( Ranges        (..)
        , Range         (..)
        , Location      (..)
        , one
        , range
        , ranges)
where
import Text.Lexer.Inchworm.Source (Range (..), Location(..))

data Ranges l
        = Ranges l [Ranges l] l
        deriving (Show, Eq)

one :: Range l -> Ranges l
one (Range l0 l1) = Ranges l0 [] l1

range :: l -> l -> Ranges l
range l0 l1 = Ranges l0 [] l1

ranges :: l -> [Ranges l] -> l -> Ranges l
ranges l0 rs l1 = Ranges l0 rs l1