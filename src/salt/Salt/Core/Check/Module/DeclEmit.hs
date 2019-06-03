
module Salt.Core.Check.Module.DeclEmit where
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Type.Base
import qualified Salt.Data.List as List
import qualified Data.Set       as Set


-- | Check emit declarations.
checkDeclEmit  :: CheckDecl a

-- (t-decl-emit) ------------------------------------------
checkDeclEmit _a ctx (DEmit (DeclEmit a' mn m))
 = do   let wh    = [WhereEmitDecl a' mn]

        -- TODO: check type is valid
        -- TOOD: check names don't conflict.
        (m', _t, _effs)
         <- checkTerm a' wh ctx Synth m

        return  $ DEmit $ DeclEmit a' mn m'
