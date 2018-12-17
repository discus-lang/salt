
module Salt.Core.Check
        ( module Salt.Core.Check.Type
        , module Salt.Core.Check.Term
        , module Salt.Core.Check.Term.Base
        , module Salt.Core.Check.Module
        , module Salt.Core.Check.Pretty
        , module Salt.Core.Check.Context
        , module Salt.Core.Exp
        , contextEmpty)
where
import Salt.Core.Check.Type
import Salt.Core.Check.Term
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Module
import Salt.Core.Check.Pretty
import Salt.Core.Check.Context
import Salt.Core.Exp
import qualified Data.Map.Strict        as Map


-- | Construct an empty context.
contextEmpty :: Context a
contextEmpty
        = Context
        { contextModuleTerm     = Map.empty
        , contextLocal          = []
        , contextCheckTerm      = checkTermWith }

