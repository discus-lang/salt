
module Salt.Core.Exp.Term
        ( module Salt.Core.Exp.Term.Base
        , module Salt.Core.Exp.Term.Compounds
        , module Salt.Core.Exp.Term.Patterns
        , module Salt.Core.Exp.Term.Predicates
        , Fragment(..))
where
import Salt.Core.Exp.Term.Base
import Salt.Core.Exp.Term.Compounds
import Salt.Core.Exp.Term.Patterns
import Salt.Core.Exp.Term.Predicates


-- | Fragment
data Fragment
        = FragTerm              -- ^ A plain term declaration.
        | FragProcBody          -- ^ A procedure body.
        | FragProcYield         -- ^ The top level expression computing a value to yield.
        | FragProcExp           -- ^ A term expression within a procedire.
        deriving (Show, Eq)

