
module Salt.Core.Exp.Type.Compounds where
import Salt.Core.Exp.Type.Patterns
import Salt.Core.Exp.Type.Base
import Salt.Core.Exp.Name


-- | Take the top-level annotation from a type, if there is one.
takeAnnotOfType :: Type a -> Maybe a
takeAnnotOfType tt
 = case tt of
        TAnn a _ -> Just a
        _        -> Nothing


-- | Given some type parameters and a body type,
--   if we have any parameters then build a type abstraction,
--   otherwise return the body type with no parameters.
makeTAbsOfParams :: [TypeParams a] -> Type a -> Type a
makeTAbsOfParams tps tBody
 = case tps of
        []      -> tBody
        _       -> foldr TAbs tBody tps


-- | Take the binders from a TPTypes.
takeTPTypes :: TypeParams a -> [(Bind, Type a)]
takeTPTypes tps
 = case tps of
        TPAnn _ tps'    -> takeTPTypes tps'
        TPTypes bks     -> bks


-- | Take the arguments from a TGTypes.
takeTGTypes :: TypeArgs a -> [Type a]
takeTGTypes tgs
 = case tgs of
        TGAnn _ tgs'    -> takeTGTypes tgs'
        TGTypes ts      -> ts


-- | Take the parameter and result types from a function type,
--   if this is one.
takeTFun :: Type a -> Maybe ([Type a], [Type a])
takeTFun tt
 = case tt of
        TFun tsParam tsResult   -> Just (tsParam, tsResult)
        _                       -> Nothing


-- | If this is a `forall` type then split off the parameters and body.
--   TODO: this one splits deep params, distinguish between one level split and deep split.
takeTForalls :: Type a -> Maybe ([[(Bind, Type a)]], Type a)
takeTForalls tt
 = start
 where start
        = case tt of
                TForall tps tBody   -> Just $ go [tps] tBody
                _ -> Nothing

       go tpss tBody
        = case tBody of
                TForall tps' tBody' -> go (tps' : tpss) tBody'
                _ -> (map takeTPTypes $ reverse tpss, tBody)

