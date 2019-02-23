
-- For GHC 8.4 -> 8.6 transition with Data.Semigroup
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Salt.Core.Analysis.Support where
import Salt.Core.Exp
import Data.Maybe
import Data.Semigroup
import Data.Set                 (Set)
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
-- | Get the set of free type bounds in a thing.
freeTypeBoundsOf :: HasSupport a => a -> Set Bound
freeTypeBoundsOf x = supportFreeTypeVars $ supportOf x


-- | Get the set of free type names (bounds at level 0) in a thing.
freeTypeNamesOf  :: HasSupport a => a -> Set Name
freeTypeNamesOf x
 =      Set.fromList [n | BoundWith n 0 <- Set.toList $ freeTypeBoundsOf x]


-- | Get the set of free term bounds in a thing.
freeTermBoundsOf :: HasSupport a => a -> Set Bound
freeTermBoundsOf x = supportFreeTermVars $ supportOf x


-- | Get the set of free term names (bounds at level 0) in a thing.
freeTermNamesOf  :: HasSupport a => a -> Set Name
freeTermNamesOf x
 =      Set.fromList [n | BoundWith n 0 <- Set.toList $ freeTermBoundsOf x]


---------------------------------------------------------------------------------------------------
data Support
        = Support
        { supportFreeTypeVars   :: !(Set Bound)
        , supportFreeTermVars   :: !(Set Bound) }
        deriving Show


instance Semigroup Support where
 (<>) (Support ts1 ms1) (Support ts2 ms2)
        = Support (Set.union ts1 ts2) (Set.union ms1 ms2)

instance Monoid Support where
 mempty  = Support Set.empty Set.empty

 mappend (Support ts1 ms1) (Support ts2 ms2)
        = Support (Set.union ts1 ts2) (Set.union ms1 ms2)


lowerBoundOfName  :: Name -> Bound -> Maybe Bound
lowerBoundOfName n u@(BoundWith n' i)
 | n == n'          = if i == 0 then Nothing else Just (BoundWith n' (i - 1))
 | otherwise        = Just u


lowerBoundOfNames :: Set Name -> Bound -> Maybe Bound
lowerBoundOfNames ns u@(BoundWith n' i)
 | Set.member n' ns = if i == 0 then Nothing else Just (BoundWith n' (i - 1))
 | otherwise        = Just u


-- | Construct a singleton support set with this type bound.
supportBoundType :: Bound -> Support
supportBoundType u = Support (Set.singleton u) Set.empty


-- | Drop a type bind from a support set.
supportBindType :: Bind -> Support -> Support
supportBindType BindNone support = support
supportBindType (BindName n) (Support nsType nsTerm)
 =      Support (Set.fromList $ mapMaybe (lowerBoundOfName n)   $ Set.toList nsType)
                nsTerm


-- | Drop a set of type binds from a support set.
supportBindTypes :: [Bind] -> Support -> Support
supportBindTypes bs (Support nsType nsTerm)
 = let  ns      = Set.fromList [ n | BindName n <- bs ]
   in   Support (Set.fromList $ mapMaybe (lowerBoundOfNames ns) $ Set.toList nsType)
                nsTerm


-- | Construct a singleton support set with this term bound.
supportBoundTerm :: Bound -> Support
supportBoundTerm u = Support (Set.singleton u) Set.empty


-- | Drop a term bind from a support set.
supportBindTerm :: Bind -> Support -> Support
supportBindTerm BindNone support = support
supportBindTerm (BindName n) (Support nsType nsTerm)
 = Support nsType
          (Set.fromList $ mapMaybe (lowerBoundOfName n) $ Set.toList nsTerm)


-- | Drop a set of term binds from a support set.
supportBindTerms :: [Bind] -> Support -> Support
supportBindTerms bs (Support nsType nsTerm)
 = let  ns      = Set.fromList [ n | BindName n <- bs ]
   in   Support nsType
                (Set.fromList $ mapMaybe (lowerBoundOfNames ns) $ Set.toList nsTerm)


-- | Drop the names bound by some `TermParams` from a support set.
supportBindTermParams :: TermParams a -> Support -> Support
supportBindTermParams mps support
 = case mps of
        MPAnn _ mps'    -> supportBindTermParams mps' support
        MPTypes bks     -> supportBindTypes (map fst bks) support
        MPTerms bts     -> supportBindTerms (map fst bts) support


-- | Drop all the names bound by some `TermParams`, inside out.
supportBindTermParamss :: [TermParams a] -> Support -> Support
supportBindTermParamss mpss support
 = case mpss of
        []          -> support
        mps : mpss' -> supportBindTermParams mps $ supportBindTermParamss mpss' support


---------------------------------------------------------------------------------------------------
class HasSupport a where
 supportOf :: a -> Support

instance HasSupport (Type a) where
 supportOf tt
  = case tt of
        TAnn _ t        -> supportOf t
        TRef r          -> supportOf r
        TVar u          -> supportBoundType u
        TAbs tps t      -> supportBindTypes (map fst $ takeTPTypes tps) $ supportOf t
        TKey tk ts      -> mconcat (supportOf tk : map supportOf ts)


instance HasSupport (TypeRef a) where
 supportOf tr
  = case tr of
        TRPrm{}         -> mempty
        TRCon{}         -> mempty
        TRClo clo       -> supportOf clo


instance HasSupport (TypeClosure a) where
 supportOf _    = mempty


instance HasSupport TypeKey where
 supportOf _    = mempty


instance HasSupport (TypeArgs a) where
 supportOf tgs
  = case tgs of
        TGAnn _ tgs'    -> supportOf tgs'
        TGTypes ts      -> mconcat $ map supportOf ts


instance HasSupport (Term a) where
 supportOf mm
  = case mm of
        MAnn _ m        -> supportOf m
        MRef r          -> supportOf r
        MVar u          -> supportBoundTerm u

        MAbs mps mBody
         -> mappend (supportOf mps)
                    (supportBindTermParams mps $ supportOf mBody)

        -- TODO: test that the bound indices work with recursive binding.
        -- what happens if a recursive binder has the same name as a fn parameter?
        MRec bms mBody
         -> let nsBind  = map BindName $ mapMaybe takeNameOfTermBind bms
            in  mappend (supportBindTerms nsBind $ mconcat $ map supportOf bms)
                        (supportBindTerms nsBind $ supportOf mBody)

        MKey mk ms      -> mconcat (supportOf mk : map supportOf ms)


-- TODO: test this. make sure the right things are in scope.
instance HasSupport (TermBind a) where
 supportOf (MBind _b mpss tsResult mBind)
  = mconcat
  [ mconcat $ map supportOf mpss
  , mconcat $ map supportOf tsResult
  , supportBindTermParamss mpss $ supportOf mBind]


instance HasSupport (TermRef a) where
 supportOf _    = mempty


instance HasSupport TermKey where
 supportOf _    = mempty


instance HasSupport (TermParams a) where
 supportOf mp
  = case mp of
        MPAnn _a mps    -> supportOf mps
        MPTerms bts     -> mconcat $ map supportOf $ map snd bts
        MPTypes bts     -> mconcat $ map supportOf $ map snd bts


instance HasSupport (TermArgs a) where
 supportOf mg
  = case mg of
        MGAnn _a mgs    -> supportOf mgs
        MGTerm  m       -> supportOf m
        MGTerms ms      -> mconcat $ map supportOf ms
        MGTypes ts      -> mconcat $ map supportOf ts


