
module Salt.Core.Prim.Ops.Base
        ( module Salt.Core.Transform.StripAnnot
        , module Salt.Core.Exp
        , Prim(..)
        , typeParamsOfPrim
        , typeOfPrim
        , qualifiedTypeOfPrim )
where
import Salt.Core.Transform.StripAnnot
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
        , tpms  :: [(Bind, Type ())]
        , tsig  :: Type ()
        , step  :: forall a. Show a => [TermNormals a] -> [Value a]
        , docs  :: Text }

        -- Define an operator that performs an action in the local process.
        | PO
        { name  :: Name
        , tpms  :: [(Bind, Type ())]
        , tsig  :: Type ()
        , exec  :: forall a. Show a => [TermNormals a] -> IO [Value a]
        , docs  :: Text }


-- | Get the type params of a primitive.
-- TODO: Currently tpms cannot distinguish between different qualifiers, this
-- flaw was shared by the previous version relying on the :*> pattern.
typeParamsOfPrim :: Prim -> [(Bind, Type())]
typeParamsOfPrim pp
 = case pp of
        PP {tpms} -> tpms
        PO {tpms} -> tpms

-- | Get the value type of a primitive.
typeOfPrim :: Prim -> Type ()
typeOfPrim pp
 = case pp of
        PP {tsig} -> tsig
        PO {tsig} -> tsig

-- | Get the qualified value type of a primitive.
-- TODO: This is a 'crutch' method to aid the transition to tpms as a separate
-- field on Prim, we may not want to keep this longer term.
qualifiedTypeOfPrim :: Prim -> Type ()
qualifiedTypeOfPrim pp
 = case pp of
        PP {tpms, tsig} | (length tpms) > 0 -> tpms :*> tsig
        PP {tsig} -> tsig
        PO {tpms, tsig} | (length tpms) > 0 -> tpms :*> tsig
        PO {tsig} -> tsig

