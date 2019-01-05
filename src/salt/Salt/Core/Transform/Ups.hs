
module Salt.Core.Transform.Ups where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Term
import Salt.Core.Exp.Ups
import Salt.Core.Exp.Name
import qualified Data.Map       as Map
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
-- | Apply a type `Ups` to a type.
upsApplyType :: Ups -> Type a -> Type a
upsApplyType ups tt
 = case tt of
        -- Descend into annotations.
        TAnn a t -> TAnn a (upsApplyType ups t)

        -- References don't include any variables to bump.
        TRef{}   -> tt

        -- Apply the ups to the type bound.
        TVar u   -> TVar $ upsApplyBound ups u

        -- Carry ups under abstraction.
        TAbs tps tBody
         -> let nsBind  = [ n | (BindName n, _) <- takeTPTypes tps ]
                ups'    = upsBumpNames nsBind ups
            in  TAbs tps $ upsApplyType ups' tBody

        -- Apply ups to other types generically.
        TKey k tgss
         -> TKey k $ map (upsApplyTypeArgs ups) tgss


-- | Apply an `Ups` to some type arguments.
upsApplyTypeArgs :: Ups -> TypeArgs a -> TypeArgs a
upsApplyTypeArgs ups tgs
 = case tgs of
        TGAnn _a tgs'   -> upsApplyTypeArgs ups tgs'
        TGTypes ts      -> TGTypes $ map (upsApplyType ups) ts


---------------------------------------------------------------------------------------------------
-- | Apply type and term `Ups` to a `Term`.
upsApplyTerm :: Ups -> Ups -> Term a -> Term a
upsApplyTerm upsT upsM mm
 = case mm of
        -- Descend into annotations.
        MAnn a m -> MAnn a (upsApplyTerm upsT upsM m)

        -- Only values references include variables to bump.
        --  Bodies of abstractions may refer to types and terms defined at top level.
        MRef (MRVal v) -> MRef (MRVal $ upsApplyValue upsT upsM v)
        MRef {}        -> mm

        -- Apply the ups to the term bound.
        MVar u   -> MVar $ upsApplyBound upsM u

        -- Carry ups under type abstractions.
        MAbs (MPTypes bks) mBody
         -> let nsBind  = [ n | (BindName n, _) <- bks ]
                bks'    = [ (b, upsApplyType upsT k) | (b, k) <- bks ]
                upsT'   = upsBumpNames nsBind upsT
            in  MAbs (MPTypes bks') $ upsApplyTerm upsT' upsM mBody

        -- Carry ups under term abstractions.
        MAbs (MPTerms bts) mBody
         -> let nsBind  = [ n | (BindName n, _) <- bts ]
                bts'    = [ (b, upsApplyType upsT t) | (b, t) <- bts ]
                upsM'   = upsBumpNames nsBind upsM
            in  MAbs (MPTerms bts') $ upsApplyTerm upsT upsM' mBody

        -- Apply ups to other terms generically.
        MKey k mgss
         -> MKey k $ map (upsApplyTermArgs upsT upsM) mgss


-- | Apply type `Ups` to some `TermParams`.
upsApplyTermParams :: Ups -> TermParams a -> TermParams a
upsApplyTermParams ups mps
 = case mps of
        MPTypes bks     -> MPTypes [ (b, upsApplyType ups k) | (b, k) <- bks ]
        MPTerms bts     -> MPTerms [ (b, upsApplyType ups t) | (b, t) <- bts ]


-- | Apply type and term `Ups` to some `TermArgs`.
upsApplyTermArgs :: Ups -> Ups -> TermArgs a -> TermArgs a
upsApplyTermArgs upsT upsM mgs
 = case mgs of
        MGTypes ts      -> MGTypes (map (upsApplyType upsT) ts)
        MGTerms ms      -> MGTerms (map (upsApplyTerm upsT upsM) ms)
        MGTerm  m       -> MGTerm  (upsApplyTerm upsT upsM m)

-- | Apply type and term `Ups` to a `Value`.
--
--   The bodies of abstractions may use `Bound`s to refer to types and terms
--   at top level, which we need to bump when we carry them under binders.
--
---
--   All the cases here are listed out so if we add new values we don't
--   forget to apply the ups. Code with unbumped variables is hard to debug.
--
upsApplyValue :: Ups -> Ups -> Value a -> Value a
upsApplyValue upsT upsM vv
 = case vv of
        VUnit           -> vv
        VBool{}         -> vv
        VNat{}          -> vv

        VInt{}          -> vv
        VInt8{}         -> vv
        VInt16{}        -> vv
        VInt32{}        -> vv
        VInt64{}        -> vv

        VWord{}         -> vv
        VWord8{}        -> vv
        VWord16{}       -> vv
        VWord32{}       -> vv
        VWord64{}       -> vv

        VText{}         -> vv
        VSymbol{}       -> vv

        VData n ts vs
         -> VData n     (map (upsApplyType  upsT)      ts)
                        (map (upsApplyValue upsT upsM) vs)

        VRecord nvss
         -> VRecord     [ (n, map (upsApplyValue upsT upsM) vs) | (n, vs) <- nvss ]

        VVariant n t vs
         -> VVariant n  (upsApplyType upsT t)  (map (upsApplyValue upsT upsM) vs)

        VList t vs
         -> VList       (upsApplyType upsT t)  (map (upsApplyValue upsT upsM) vs)

        VSet t vs
         -> VSet        (upsApplyType upsT t)  (Set.map (upsApplyValue upsT upsM) vs)

        VMap tk tv kvs
         -> VMap        (upsApplyType upsT tk) (upsApplyType upsT tv)
                        (Map.fromList [ ( upsApplyValue upsT upsM vk
                                        , upsApplyValue upsT upsM ve)
                                      | (vk, ve) <- Map.toList kvs ])

        VClosure (TermClosure env (MPTypes bks) mBody)
         -> let env'    = upsApplyTermEnv upsT upsM env
                nsBind  = [ n | (BindName n, _) <- bks ]
                bks'    = [ (b, upsApplyType upsT k) | (b, k) <- bks ]
                upsT'   = upsBumpNames nsBind upsT
                mBody'  = upsApplyTerm upsT' upsM mBody
            in  VClosure $ TermClosure env' (MPTypes bks') mBody'

        VClosure (TermClosure env (MPTerms bts) mBody)
         -> let env'    = upsApplyTermEnv upsT upsM env
                nsBind  = [ n | (BindName n, _) <- bts ]
                bts'    = [ (b, upsApplyType upsT t) | (b, t) <- bts ]
                upsM'   = upsBumpNames nsBind upsM
                mBody'  = upsApplyTerm upsT upsM' mBody
            in  VClosure $ TermClosure env' (MPTerms bts') mBody'


-- | Apply type and term `Ups` to an `Env`
upsApplyTermEnv :: Ups -> Ups -> TermEnv a -> TermEnv a
upsApplyTermEnv upsT upsM (TermEnv bs)
 = TermEnv $ map (upsApplyTermEnvBinds upsT upsM) bs


-- | Apply type and term `Ups` to some `EnvBinds`
upsApplyTermEnvBinds :: Ups -> Ups -> TermEnvBinds a -> TermEnvBinds a
upsApplyTermEnvBinds upsT upsM eb
 = case eb of
        TermEnvTypes nts
         -> TermEnvTypes  $ Map.fromList
                [ (n, upsApplyType upsT t)
                | (n, t) <- Map.toList nts ]

        TermEnvValues nvs
         -> TermEnvValues $ Map.fromList
                [ (n, upsApplyValue upsT upsM v)
                | (n, v) <- Map.toList nvs ]

