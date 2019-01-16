
module Salt.Data.Location
        ( Range (..)
        , Location (..)
        , RL, rlNone)
where
import Text.Lexer.Inchworm.Source

type RL = Range Location
rlNone  = Range (Location 0 0) (Location 0 0)
