
module Salt.Core.Check.Module.DeclEmit where
import Salt.Core.Check.Module.Base
import Salt.Core.Check.Term.Base


-- | Check emit declarations.
checkDeclEmit  :: CheckDecl a

-- (t-decl-emit) ------------------------------------------
checkDeclEmit _a ctx (DEmit (DeclEmit a' mn m))
 = do   let wh    = [WhereEmitDecl a' mn]

        -- TODO: check type is valid
        -- TOOD: check names don't conflict.
        (m', _t, _effs)
         <- synthTerm a' wh ctx m

        return  $ DEmit $ DeclEmit a' mn m'

checkDeclEmit _ _ decl
 = return decl