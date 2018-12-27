
module Salt.Core.Eval.Term where
import Salt.Core.Eval.Error
import Salt.Core.Eval.State
import Salt.Core.Analysis.Support       ()
import Salt.Core.Transform.MapAnnot
import Control.Exception
import Control.Monad
import qualified Salt.Core.Prim.Ops     as Ops
import qualified Data.Map               as Map
import qualified Data.Set               as Set
-- import qualified Text.Show.Pretty       as Text


---------------------------------------------------------------------------------------------------
-- | The usual shape of evaluation functions.
type Eval a x y
        = Annot a => State a -> a -> Env a -> x -> IO y


---------------------------------------------------------------------------------------------------
-- | Evaluate a term in the given environment.
evalTerm :: Eval a (Term a) [Value a]

-- Pass through annotations.
evalTerm s _a env (MAnn a' m)
 = evalTerm s a' env m

-- Multi value return.
evalTerm s a env (MKey MKTerms [MGTerms ms])
 = evalTerms s a env ms

-- Type ascription.
evalTerm s a env (MKey MKThe [MGTypes [_], MGTerm m])
 = evalTerm s a env m

-- Values.
evalTerm _s _ _  (MRef (MRVal v))
 = return [v]


-- Variables
evalTerm s a env (MVar u@(Bound n))
 -- Bound in local environment.
 | Just v       <- envLookupValue n env
 = return [v]

 -- Bound in top-level environment.
 | Just (DeclTerm a' _n mps _tr mBody)
   <- Map.lookup n (stateDeclTerms s)
 = case mps of
        []      -> evalTerm s a' envEmpty mBody
        mp0 : mps0
         -> let makeClosure mp1 []
                 = VClosure $ Closure envEmpty mp1 mBody

                makeClosure mp1 (mp1' : mps1')
                 = VClosure $ Closure envEmpty mp1 (MVal (makeClosure mp1' mps1'))

            in   return [makeClosure mp0 mps0]

 | otherwise
 = throw $ ErrorVarUnbound a u env


-- Function abstraction.
evalTerm _s _a env (MAbm bts mBody)
 = do   -- TODO: subst into types in params.
        return [VClosure (Closure env (MPTerms bts) mBody)]


-- Prim-term application
evalTerm s a env (MAps (MPrm nPrim) mgssArg)
 = case Map.lookup nPrim Ops.primOps of
        Just (Ops.PP _name _type step _docs)
         -> do  nssArg   <- mapM (evalTermArgs s a env) mgssArg
                let vsResult = step nssArg
                return vsResult

        Just (Ops.PO _name _type _effs exec _docs)
         -> do  nssArg   <- mapM (evalTermArgs s a env) mgssArg
                vsResult <- exec nssArg
                return vsResult

        Nothing -> throw $ ErrorPrimUnknown a nPrim


-- Term-term, term/type application.
evalTerm s a env (MApp mFun mgs)
 = do   vsCloTerm <- evalTerm s a env mFun
        case vsCloTerm of
         [VClosure (Closure env' mps@(MPTerms bts) mBody)]
          -> case mgs of
                MGTerm m
                 -> do  let bs    = map fst bts
                        vsArg   <- evalTerm s a env m
                        when (not $ length vsArg == length bs)
                         $ throw $ ErrorWrongTermArity a (length bs) vsArg

                        let env'' = envExtendsValue (zip bs vsArg) env'
                        evalTerm s a env'' mBody

                MGTerms ms
                 -> do  let bs  = map fst bts
                        vsArg   <- mapM (evalTerm1 s a env) ms
                        when (not $ length vsArg == length bs)
                         $ throw $ ErrorWrongTermArity a (length bs) vsArg

                        let env'' = envExtendsValue (zip bs vsArg) env'
                        evalTerm s a env'' mBody

                _ -> throw $ ErrorAppTermWrongArgs a mps mgs

         [VClosure (Closure env' mps@(MPTypes bks) mBody)]
          -> case mgs of
                MGTypes ts
                 -> do  let bs  = map fst bks

                        -- TODO: drop subst into tsArg'
                        -- TODO: we need type bindings in the environment.
                        let tsArg = ts
                        when (not $ length tsArg == length bs)
                         $ throw $ ErrorWrongTypeArity a (length bs) tsArg

                        let env'' = envExtendsType (zip bs tsArg) env'
                        evalTerm s a env'' mBody

                _ -> throw $ ErrorAppTermWrongArgs a mps mgs

         _  -> throw $ ErrorAppTermBadClosure a vsCloTerm


-- Let-binding.
evalTerm s a env (MLet bts mBind mBody)
 = do   vsBind <- evalTerm s a env mBind
        let nWanted = length bts
        let nHave   = length vsBind
        if  nWanted == nHave
         then do
                let bs = map fst bts
                let env' =  envExtendsValue (zip bs vsBind) env
                vsResult <- evalTerm s a env' mBody
                return vsResult
         else throw $ ErrorWrongTermArity a nWanted vsBind


-- Data constructor application.
evalTerm s a env (MData nCon tsArg msArg)
 = do   vsArg  <- evalTerms s a env msArg
        -- TODO: subst into types.
        return  [VData nCon tsArg vsArg]


-- Record constructor application.
evalTerm s a env (MRecord nsField msArg)
 | length nsField == length msArg
 = do   vssArg <- mapM (evalTerm s a env) msArg
        return  [VRecord $ zip nsField vssArg]


-- Record field projection.
evalTerm s a env (MProject nField mRecord)
 = do   vRec  <- evalTerm1 s a env mRecord
        case vRec of
         VRecord nvs
          -> case lookup nField nvs of
                Nothing -> throw $ ErrorProjectMissingField a vRec nField
                Just vs -> return vs
         _ -> throw $ ErrorProjectNotRecord a vRec nField


-- Case matching.
evalTerm s a env mm@(MVarCase mScrut msAlt0)
 = do   vScrut  <- evalTerm1 s a env mScrut

        let (nScrut, vsData)
             = case vScrut of
                VVariant l _ vs -> (l, vs)
                _ -> throw $ ErrorCaseScrutNotVariant a vScrut

        let go (MVarAlt nAlt btsPat mBody : msAlt)
                |  nAlt == nScrut = (btsPat, mBody)
                |  otherwise      = go msAlt
            go [] = throw $ ErrorCaseNoMatch a vScrut
            go _  = throw $ ErrorInvalidConstruct a mm

        let (btsPat, mBody) = go msAlt0

        when (not $ length btsPat == length vsData)
         $ throw $ ErrorWrongTermArity a (length btsPat) vsData

        let bs   = map fst btsPat
        let env' = envExtendsValue (zip bs vsData) env

        evalTerm s a env' mBody


-- If-then-else
evalTerm s a env mm@(MKey MKIf [MGTerms msCond, MGTerms msThen, MGTerm mElse])
 = loop msCond msThen
 where
        -- Try all the conditions from top to bottom.
        loop (mCond : msCond') (mThen : msThen')
         = do   vCond   <- evalTerm1 s a env mCond
                case vCond of
                 VBool True   -> evalTerm s a env mThen
                 VBool False  -> loop msCond' msThen'
                 _            -> throw $ ErrorIfsScrutNotBool a vCond

        -- No condition evaluated to true, so run the else branch.
        loop [] []
         = do   evalTerm s a env mElse

        -- We have a different number of condition and branch terms.
        loop _ _
         = throw $ ErrorInvalidConstruct a mm


-- Box a computation.
evalTerm _s _a env (MBox mBody)
 =      return  [VClosure (Closure env (MPTerms []) mBody)]


-- Run a suspension
evalTerm s a env (MRun mSusp)
 = do   vSusp <- evalTerm1 s a env mSusp
        case vSusp of
         VClosure (Closure env' (MPTerms []) mBody)
           -> evalTerm s a env' mBody
         _ -> throw $ ErrorRunNotSuspension a vSusp


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
evalTerm1 :: Eval a (Term a) (Value a)
evalTerm1 s a env m
 = do   vs      <- evalTerm s a env m
        case vs of
         [v]    -> return v
         _      -> throw $ ErrorWrongTermArity a 1 vs


-- | Evaluate a list of terms, producing a single value for each.
evalTerms :: Eval a [Term a] [Value a]
evalTerms s a env ms
 = mapM (evalTerm1 s a env) ms


---------------------------------------------------------------------------------------------------
evalTermArgs :: Eval a (TermArgs a) (TermNormals a)
evalTermArgs s a env mgs
 = case mgs of
        MGTerm  m
         -> do  vs <- evalTerm s a env m
                return $ NVs vs

        MGTerms ms
         -> do  vs <- mapM (evalTerm1 s a env) ms
                return $ NVs vs

        MGTypes ts
         -> do  -- TODO: drop env as subst. into type.
                return $ NTs ts

