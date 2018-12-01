
module Salt.Core.Eval.Term where
import Salt.Core.Eval.Data
import Salt.Core.Analysis.Support
import Salt.Core.Transform.MapAnnot
import Control.Exception
import qualified Salt.Core.Prim.Ops     as Ops
import qualified Data.Map               as Map
import qualified Data.Set               as Set


---------------------------------------------------------------------------------------------------
-- | Evaluate a term in the given environment.
evalTerm :: Annot a
         => State a -> a -> Env a -> Term a -> IO [Value a]

-- Pass through annotations.
evalTerm s _a env (MAnn a' m)
 = evalTerm s a' env m


-- References.
evalTerm _s _ _  (MRef (MRVal v))
 = return [v]


-- Variables
evalTerm s a env (MVar u@(Bound n))
 -- Bound in local environment.
 | Just v       <- envLookup n env
 = return [v]

 -- Bound in top-level environment.
 | Just (DeclTerm _a _n mps _tr mBody)
                <- Map.lookup n (stateDeclTerms s)
 = return [VClosure $ CloTerm envEmpty mps mBody]

 | otherwise
 = throw $ ErrorVarUnbound a u env


-- Function abstraction.
--   We trim the closure environment at the point the closure is
--   produced so that the closure is easier to read in debug logs.
evalTerm _s _a env (MAbm bts mBody)
 = do   let nsTermFree  = freeTermVarsOf mBody
        let Env nmsEnv  = env
        let env'        = Env [ (n, m) | (n, m) <- nmsEnv
                                       , Set.member n nsTermFree ]
        return [VClosure (CloTerm env' [MPTerms bts] mBody)]


-- Multi value return.
evalTerm s a env (MKey MKTerms [MGTerms ms])
 = evalTerms s a env ms


-- Prim-term application
evalTerm s a env (MKey MKApp [MGTerm (MPrm nPrim), mgsArg])
 | case mgsArg of
        MGTerm{}  -> True
        MGTerms{} -> True
        _         -> False
 = case Map.lookup nPrim Ops.primOps of
        Just (Ops.PP _name _type step _docs)
         -> do  vsArg   <- evalTermArgs s a env mgsArg
                let vsResult = step [] vsArg
                return vsResult

        Just (Ops.PO _name _type exec _docs)
         -> do  vsArg    <- evalTermArgs s a env mgsArg
                vsResult <- exec [] vsArg
                return vsResult

        Nothing -> throw $ ErrorPrimUnknown a nPrim


-- Term-term application.
evalTerm s a env (MKey MKApp [MGTerm mFun, mgsArg])
 = do   vsCloTerm <- evalTerm s a env mFun
        case vsCloTerm of
         [VClosure (CloTerm env' [MPTerms bts] mBody)]
          -> do let bs    = map fst bts
                vsArg <- evalTermArgs s a env mgsArg
                let env'' = envExtends (zip bs vsArg) env'
                vsRes <- evalTerm     s a env'' mBody
                return vsRes
         [] -> throw $ ErrorAppTermNotEnough a []
         _  -> throw $ ErrorAppTermTooMany   a vsCloTerm


-- Let-binding.
evalTerm s a env (MKey MKLet [MGTerms [mBind, MAbs (MPTerms bts) mBody]])
 = do   vsBind <- evalTerm s a env mBind
        let nWanted = length bts
        let nHave   = length vsBind
        if  nWanted == nHave
         then do
                let bs = map fst bts
                let env' =  envExtends (zip bs vsBind) env
                vsResult <- evalTerm s a env' mBody
                return vsResult
        else if nHave < nWanted
         then throw $ ErrorAppTermNotEnough a vsBind
        else  throw $ ErrorAppTermTooMany   a vsBind


-- Data constructor application.
evalTerm s a env (MKey (MKCon nCon) [MGTypes ts, MGTerms msArg])
 = do   vsArg  <- evalTerms s a env msArg
        return  [VData nCon ts vsArg]


-- Record constructor application.
evalTerm s a env (MKey (MKRecord nsField) [MGTerms msArg])
 | length nsField == length msArg
 = do   vsArg <- evalTerms s a env msArg
        return [VRecord $ zip nsField vsArg]


-- Record field projection.
evalTerm s a env (MKey (MKProject nField) [MGTerms [mRecord]])
 = do   vRec  <- evalTerm1 s a env mRecord
        case vRec of
         VRecord nvs
          -> case lookup nField nvs of
                Nothing -> throw $ ErrorProjectMissingField a vRec nField
                Just v  -> return [v]
         _ -> throw $ ErrorProjectTypeMismatch a vRec nField


-- If-then-else
-- TODO: throw real exception on type errors.
evalTerm s a env (MKey MKIf [MGTerms msCond, MGTerms msThen, MGTerm mElse])
 = let
        loop [] []
         = do   evalTerm s a env mElse

        loop (mCond : msCond') (mThen : msThen')
         = do   vCond   <- evalTerm1 s a env mCond
                case vCond of
                 VBool True   -> evalTerm s a env mThen
                 VBool False  -> loop msCond' msThen'
                 _            -> error "if-then-else runtime type error"

        loop _ _ = error "if cond then length mismatch"

   in   loop msCond msThen


-- List construction.
evalTerm s a env (MKey MKList [MGTypes [t], MGTerms ms])
 = do   vsArg <- evalTerms s a env ms
        return [VList t vsArg]


-- Set construction.
evalTerm s a env (MKey MKSet  [MGTypes [t], MGTerms ms])
 = do   vsArg <- evalTerms s a env ms
        return [VSet t $ Set.fromList $ map stripAnnot $ vsArg]


-- No match.
evalTerm _s a _ mm
 =      throw $ ErrorInvalidConstruct a mm


---------------------------------------------------------------------------------------------------
-- | Like `evalTerm`, but expect a single result value.
evalTerm1
        :: Annot a
        => State a -> a -> Env a
        -> Term a -> IO (Value a)
evalTerm1 s a env m
 = do   vs      <- evalTerm s a env m
        case vs of
         [v]    -> return v
         []     -> throw $ ErrorAppTermNotEnough a []
         vs'    -> throw $ ErrorAppTermTooMany   a vs'


-- | Evaluate a list of terms, producing a single value for each.
evalTerms
        :: Annot a
        => State a -> a -> Env a
        -> [Term a] -> IO [Value a]
evalTerms s a env ms
 = mapM (evalTerm1 s a env) ms


---------------------------------------------------------------------------------------------------
evalTermArgs
        :: Annot a
        => State a -> a -> Env a
        -> TermArgs a -> IO [Value a]
evalTermArgs s a env mgs
 = case mgs of
        MGTerm  m   -> evalTerm s a env m
        MGTerms ms  -> mapM (evalTerm1 s a env) ms
        MGTypes _   -> error "cannot evaluate type arguments"

