
module Salt.Core.Prim.Ops.Base
        ( module Salt.Core.Transform.MapAnnot
        , module Salt.Core.Exp
        , Prim(..)
        , typeOfPrim
        , effectOfPrim)
where
import Salt.Core.Transform.MapAnnot
import Salt.Core.Exp


-- | Holds information about a primitive operator.
--   We keep all the info about an operator in once place,
--   instead of spread out all over the compiler and external documentation.
data Prim
        -- Define a pure primitive operator.
        -- Provided the values match the expected types,
        -- these operators always succeed, and perform no actions.
        = PP
        { name  :: Name
        , tsig  :: Type ()
        , step  :: forall a. [TermNormals a] -> [Value a]
        , docs  :: Text }

        -- Define an operator that performs an action in the local process.
        | PO
        { name  :: Name
        , tsig  :: Type ()
        , teff  :: [Type ()]
        , exec  :: forall a. Show a => [TermNormals a] -> IO [Value a]
        , docs  :: Text }


-- | Get the value type of a primitive.
typeOfPrim :: Prim -> Type ()
typeOfPrim pp
 = case pp of
        PP {tsig} -> tsig
        PO {tsig} -> tsig


-- | Get the effect type of a primitive.
effectOfPrim :: Prim -> Type ()
effectOfPrim pp
 = case pp of
        PP {}     -> TPure
        PO {teff} -> TSum teff
