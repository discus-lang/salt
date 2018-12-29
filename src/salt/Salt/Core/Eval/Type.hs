
module Salt.Core.Eval.Type where
import Salt.Core.Eval.Error
import Salt.Core.Eval.Base
import Control.Exception


---------------------------------------------------------------------------------------------------
-- | Evaluate a type in the given environment.
--

evalType :: EvalType a (Type a) (Type a)

-- (evt-ann) ----------------------------------------------
evalType s _a env (TAnn a' t)
 = evalType s a' env t

-- (evt-ref) ----------------------------------------------
evalType _s _a _env tt@TRef{}
 = return tt

-- (evt-var) ----------------------------------------------
evalType s a env (TVar u)
 = resolveTypeBound (stateModule s) env u
 >>= \case
        -- Type is bound at top level.
        Just (TypeDefDecl  t) -> evalType s a env t

        -- Type is bound in the local environment.
        Just (TypeDefLocal t) -> return t

        -- Can't find the binding site for this bound variable.
        _ -> throw $ ErrorTypeVarUnbound a u env


-- (evt-abs) ----------------------------------------------
evalType _s _a _env (TAbs _tps _mBody)
 = error "finish abs"

-- (evt-key) ----------------------------------------------
evalType _s _a _env (TKey _tk _args)
 = error "finish key"
