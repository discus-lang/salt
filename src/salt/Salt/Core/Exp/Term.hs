
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


-- | Language fragment that a subexpression is being restricted to.
data Fragment
        -- | A functional term that computes a value.
        --
        --   Functional terms can create and apply arbitrary function abstractions,
        --   and define locally recursive computations.
        = FragTerm

        -- | The body of a procedure that can use procedural
        --   control flow constructs.
        | FragProcBody

        -- | A simple expression within a procedure.
        --
        --   Procedural expressions can only apply primops and call existing functions.
        --   They cannot define new function abstractions or use local recursion.
        | FragProcExp
        deriving (Show, Eq)

