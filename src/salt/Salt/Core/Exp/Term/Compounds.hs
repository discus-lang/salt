
module Salt.Core.Exp.Term.Compounds where
import Salt.Core.Exp.Term.Patterns
import Salt.Core.Exp.Term.Base
import Salt.Core.Exp.Type
import Salt.Core.Exp.Name


------------------------------------------------------------------------------------------ Annot --
-- | Take the top-level annotation from a type, if there is one.
takeAnnotOfTerm :: Term a -> Maybe a
takeAnnotOfTerm tt
 = case tt of
        MAnn a _ -> Just a
        _        -> Nothing


-- | Unwrap annotations from a term until we get the first non-annotation.
unwrapAnnotOfTerm :: a -> Term a -> (a, Term a)
unwrapAnnotOfTerm a mm
 = case mm of
        MAnn a' mm' -> unwrapAnnotOfTerm a' mm'
        _           -> (a, mm)


------------------------------------------------------------------------------------------ Names --
-- | Take the name of a primitive from a term, if this is one.
--   We look through any annotations and type ascriptions.
takeMPrm :: Term a -> Maybe Name
takeMPrm mm
 = case mm of
        MAnn _ m        -> takeMPrm m
        MThe _ m        -> takeMPrm m
        MPrm n          -> Just n
        _               -> Nothing


-- | Take the name of a symbol from a value, if this is one.
takeVSymbol :: Value a -> Maybe Name
takeVSymbol vv
 = case vv of
        VSymbol n       -> Just n
        _               -> Nothing


------------------------------------------------------------------------------------------- Bind --
-- | Take the binder of a `TermBind`.
bindOfTermBind :: TermBind a -> Bind
bindOfTermBind (MBind b _mps _t _m)
 = b


-- | Take the name of a `TermBind`.
takeNameOfTermBind :: TermBind a -> Maybe Name
takeNameOfTermBind (MBind b _mps _t _m)
 = case b of
        BindName n      -> Just n
        BindNone        -> Nothing


----------------------------------------------------------------------------------------- Params --
-- | Take the contents of a `TermParams`, looking through any annotations,
--   producing either a list of type or term binders.
takeTermParams :: TermParams a -> Either [(Bind, Kind a)] [(Bind, Type a)]
takeTermParams mps
 = case mps of
        MPAnn _ mps'    -> takeTermParams mps'
        MPTypes bks     -> Left bks
        MPTerms bts     -> Right bts


-- | Unwrap annotations from an `TermParams` until we get to the first non-annotation.
unwrapTermParams :: a -> TermParams a -> (a, TermParams a)
unwrapTermParams a mps
 = case mps of
        MPAnn a' mps'   -> unwrapTermParams a' mps'
        _               -> (a, mps)


-- | Take the contents of an `MPTypes`, if this is one,
--   looking through any annotations.
takeMPTypes :: TermParams a -> Maybe [(Bind, Kind a)]
takeMPTypes mps
 = case mps of
        MPAnn _a mps'   -> takeMPTypes mps'
        MPTypes bks     -> Just bks
        _               -> Nothing


-- | Take the contents of an `MPTypes`, if this is one,
--   looking through any annotations.
takeMPTerms :: TermParams a -> Maybe [(Bind, Type a)]
takeMPTerms mps
 = case mps of
        MPAnn _ mps'    -> takeMPTerms mps'
        MPTerms bts     -> Just bts
        _               -> Nothing


-- | Take the contents of an `MPTypes`, also producing the inner-most annotation.
takeAnnMPTypes :: a -> TermParams a -> Maybe (a, [(Bind, Kind a)])
takeAnnMPTypes a mps
 = case mps of
        MPAnn a' mps'   -> takeAnnMPTypes a' mps'
        MPTypes bks     -> Just (a, bks)
        _               -> Nothing


-- | Take the contents of an `MPTypes`, also producing the inner-most annotation.
takeAnnMPTerms :: a -> TermParams a -> Maybe (a, [(Bind, Type a)])
takeAnnMPTerms a mps
 = case mps of
        MPAnn a' mps'   -> takeAnnMPTypes a' mps'
        MPTerms bts     -> Just (a, bts)
        _               -> Nothing


-- | Take the list of type names bound by a `TermParams`.
typeNamesOfTermParams :: TermParams a -> [Name]
typeNamesOfTermParams mps
 = case mps of
        MPAnn _ mps'    -> typeNamesOfTermParams mps'
        MPTypes _       -> []
        MPTerms bts     -> [ n | BindName n <- map fst bts ]


-- | Take the list of term names bound by a `TermParams`.
termNamesOfTermParams :: TermParams a -> [Name]
termNamesOfTermParams mps
 = case mps of
        MPAnn _ mps'    -> termNamesOfTermParams mps'
        MPTypes bts     -> [ n | BindName n <- map fst bts ]
        MPTerms _       -> []


------------------------------------------------------------------------------------------- Args --
-- | Unwrap annotations from some `TermArgs`.
unwrapTermArgs :: a -> TermArgs a -> (a, TermArgs a)
unwrapTermArgs a mgs
 = case mgs of
        MGAnn a' mgs'   -> unwrapTermArgs a' mgs'
        _               -> (a, mgs)


-- | Take the initial sequence of MGTypes from a list of TermArgs
takeMGTypesPrefix :: [TermArgs a] -> [TermArgs a]
takeMGTypesPrefix mgs
 = case mgs of
        mg : mgs'
           | Just _ <- takeMGTypes mg
           -> mg : takeMGTypesPrefix mgs'
        _  -> []


-- | Take the contents of an `MPTypes`, if this is one,
--   looking through any annotations.
takeMGTypes :: TermArgs a -> Maybe [Type a]
takeMGTypes mgs
 = case mgs of
        MGAnn _ mgs'    -> takeMGTypes mgs'
        MGTypes ts      -> Just ts
        _               -> Nothing


-- | Take the contents of an `MPTerms`, if this is one,
--   looking through any annotations.
takeMGTerms :: TermArgs a -> Maybe [Term a]
takeMGTerms mps
 = case mps of
        MGAnn _ mgs'    -> takeMGTerms mgs'
        MGTerms ms      -> Just ms
        _               -> Nothing


-- | Take the contents of an `MPTerm`, if this is one,
--   looking through any annotations.
takeMGTerm :: TermArgs a -> Maybe (Term a)
takeMGTerm mps
 = case mps of
        MGAnn _ mgs'    -> takeMGTerm mgs'
        MGTerm  m       -> Just m
        _               -> Nothing


-- | Take the contents of an `MPTypes`, if this is one,
--   looking through any annotations.
takeAnnMGTypes :: a -> TermArgs a -> Maybe (a, [Type a])
takeAnnMGTypes a mgs
 = case mgs of
        MGAnn a' mgs'   -> takeAnnMGTypes a' mgs'
        MGTypes ts      -> Just (a, ts)
        _               -> Nothing


-- | Take the contents of an `MPTerms`, if this is one,
--   looking through any annotations.
takeAnnMGTerms :: a -> TermArgs a -> Maybe (a, [Term a])
takeAnnMGTerms a mgs
 = case mgs of
        MGAnn a' mgs'   -> takeAnnMGTerms a' mgs'
        MGTerms ms      -> Just (a, ms)
        _               -> Nothing


-- | Take the contents of an `MPTerm`, if this is one,
--   looking through any annotations.
takeAnnMGTerm :: a -> TermArgs a -> Maybe (a, Term a)
takeAnnMGTerm a mgs
 = case mgs of
        MGAnn a' mgs'   -> takeAnnMGTerm a' mgs'
        MGTerm m        -> Just (a, m)
        _               -> Nothing


-------------------------------------------------------------------------------------------- Aps --
takeMAps :: Term a -> Maybe (Term a, [TermArgs a])
takeMAps mm
 = case mm of
        MAnn _a m               -> takeMAps m
        MAps mFun mgssArg       -> Just (mFun, mgssArg)
        _                       -> Nothing


---------------------------------------------------------------------------------------- Closure --
-- | Unpack a Just wrapped closure, if this is one.
takeVMaybeClosure :: Value a -> Maybe (Maybe (TermClosure a))
takeVMaybeClosure vv
 = case vv of
        VSome _ (VClosure c) -> Just $ Just  c
        VNone _         -> Just $ Nothing
        _               -> Nothing


-- | Take a closure from a value, if this is one.
takeVClosure :: Value a -> Maybe (TermClosure a)
takeVClosure (VClosure c) = Just c
takeVClosure _            = Nothing

