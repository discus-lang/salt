
module Salt.Core.Exp.Type.Compounds where
import Salt.Core.Exp.Type.Patterns
import Salt.Core.Exp.Type.Base
import Salt.Core.Exp.Name
import Control.Monad (unless)


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

-- | Take the name of a TypeRef if this is one, or `Nothing`.
takeTypeRefName :: TypeRef a -> Maybe Name
takeTypeRefName (TRPrm name) = Just name
takeTypeRefName (TRCon name) = Just name
takeTypeRefName _            = Nothing

-- | Take the name of a TRef if this is one, or `Nothing`.
takeTRefName :: Type a -> Maybe Name
takeTRefName (TAnn _ t) = takeTRefName t
takeTRefName (TRef tr)  = takeTypeRefName tr
takeTRefName _          = Nothing

-- | Take the Bound of a TVar if this is one, or `Nothing`.
takeTVarBound :: Type a -> Maybe Bound
takeTVarBound (TAnn _ t) = takeTVarBound t
takeTVarBound (TVar b)   = Just b
takeTVarBound _          = Nothing

-- | Flatten a type application into the type name and its arguments, or
-- `Nothing` if this is not a type application.
takeTypeApp :: Type a -> Maybe (Name, [Type a])
takeTypeApp (TAnn _ t) = takeTypeApp t
takeTypeApp (TKey TKApp [left, right]) = do
    leftType <- case takeTGTypes left of
        [l] -> Just l
        _      -> Nothing
    typeName <- takeTRefName leftType

    -- right :: [Type a]
    typeArgs <- return (takeTGTypes right)

    return (typeName, typeArgs)
takeTypeApp _ = Nothing

-- | Take the Type if it is a simple effect, or `Nothing`.
takeSimpleEffect :: Type a -> Maybe (Type a)
takeSimpleEffect eff = do
    (left, _) <- takeTypeApp eff

    let simpleEffects = map fromString ["Alloc", "Read", "Write"]

    if (left `elem` simpleEffects)
        then Just eff
        else Nothing

-- | Take the Bound of a Type if it is a simple effect, or `Nothing`.
takeSimpleEffectBound :: Type a -> Maybe Bound
takeSimpleEffectBound eff = do
    (left, right) <- takeTypeApp eff

    let simpleEffects = map fromString ["Alloc", "Read", "Write"]

    unless (left `elem` simpleEffects)
        Nothing

    right' <- mapM takeTVarBound right

    case right' of
        [bound] -> Just bound
        _       -> Nothing

-- | Get Name(s) out of Bind(s), ignoring any BindNone.
getBindingNames :: [Bind] -> [Name]
getBindingNames []              = []
getBindingNames (BindNone  :xs) = getBindingNames xs
getBindingNames (BindName n:xs) = n:getBindingNames xs

