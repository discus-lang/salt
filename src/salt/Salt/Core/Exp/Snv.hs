
module Salt.Core.Exp.Snv where
import Salt.Core.Exp.Name
import Salt.Core.Exp.Ups


-- | A substitution of things for names.
data Snv x
        = Snv [((Name, Depth), x)]
        deriving (Show, Eq, Ord)


-- | An empty substitution.
snvEmpty :: Snv x
snvEmpty = Snv []


-- | Construct a substitution where all the bindings are at depth 0.
snvOfBinds :: [(Name, x)] -> Snv x
snvOfBinds nxs
 = Snv [ ((n, 0), x) | (n, x) <- nxs ]


-- | Check if the given subsitution is empty.
snvIsEmpty :: Snv x -> Bool
snvIsEmpty (Snv bs) = null bs


-- | Bump a subsitution due to pushing it under some binders.
--
--   This adjusts the depth fields in the substitution, but does not apply
--   ups to the bindings. This needs to be done separately.
--
snvBump :: [Name] -> Snv x -> Snv x
snvBump ns (Snv bs)
 = Snv $ map snvBump1 bs
 where
        snvBump1 ((n, d), x)
         = ( (n, d + if elem n ns then 1 else 0)
           , x)


-- | Apply a substitution to a bound occurrence of a variable.
--
--   Given a bound and a subsitution, if the substitution has a binder of the
--   same name as the bound, and the depth of substitution is more than the depth
--   of the bound then also decrement the depth in the bound. This is done to handle
--   substitutions arising from beta contraction in a context with shadowed binders.
--
snvApplyBound :: Snv x -> Bound -> Either Bound x
snvApplyBound (Snv bs) u@(BoundWith name depth)
 = case bs of
        [] -> Left u

        ((name', depth'), x) : bs'
         -- Substitution matches the bound variable.
         | name  == name',  depth == depth'
         -> Right $ x

         -- Decrement the depth to handle subsitutions arising from beta-contraction
         -- in a context with shadowed binders.
         | name  == name',  depth >  depth'
         -> Left $ BoundWith name (depth - 1)

         -- Subsitution does not match this variable,
         -- so check the others.
         | otherwise
         -> snvApplyBound (Snv bs') u


