
-- For GHC 8.4 -> 8.6 transition with Data.Semigroup
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Salt.Core.Analysis.Support where
import Salt.Core.Exp
import Data.Semigroup
import Data.Set                 (Set)
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
freeTypeVarsOf :: HasSupport a => a -> Set Name
freeTypeVarsOf x = supportFreeTypeVars $ supportOf x

freeTermVarsOf :: HasSupport a => a -> Set Name
freeTermVarsOf x = supportFreeTermVars $ supportOf x


---------------------------------------------------------------------------------------------------
data Support
        = Support
        { supportFreeTypeVars   :: !(Set Name)
        , supportFreeTermVars   :: !(Set Name) }
        deriving Show


instance Semigroup Support where
 (<>) (Support ts1 ms1) (Support ts2 ms2)
        = Support (Set.union ts1 ts2) (Set.union ms1 ms2)

instance Monoid Support where
 mempty  = Support Set.empty Set.empty

 mappend (Support ts1 ms1) (Support ts2 ms2)
        = Support (Set.union ts1 ts2) (Set.union ms1 ms2)


-- | Construct a singleton support set with this type bound.
supportBoundType :: Bound -> Support
supportBoundType (Bound n)
        = Support (Set.singleton n) Set.empty


-- | Drop a type bind from a support set.
supportDropBindType :: Bind -> Support -> Support
supportDropBindType BindNone support = support
supportDropBindType (BindName n) (Support nsType nsTerm)
        = Support (Set.delete n nsType) nsTerm


-- | Drop a set of type binds from a support set.
supportDropBindTypes :: Set Bind -> Support -> Support
supportDropBindTypes bs (Support nsType nsTerm)
 = let  ns      = Set.map (\(BindName n) -> n) bs
   in   Support (Set.difference nsType ns) nsTerm


-- | Construct a singleton support set with this term bound.
supportBoundTerm :: Bound -> Support
supportBoundTerm (Bound n)
        = Support Set.empty (Set.singleton n)


-- | Drop a term bind from a support set.
supportDropBindTerm :: Bind -> Support -> Support
supportDropBindTerm BindNone support = support
supportDropBindTerm (BindName n) (Support nsType nsTerm)
        = Support nsType (Set.delete n nsTerm)


-- | Drop a set of term binds from a support set.
supportDropBindTerms :: Set Bind -> Support -> Support
supportDropBindTerms bs (Support nsType nsTerm)
 = let  ns      = Set.map (\(BindName n) -> n) bs
   in   Support nsType (Set.difference nsTerm ns)


---------------------------------------------------------------------------------------------------
class HasSupport a where
 supportOf :: a -> Support

instance HasSupport (Type a) where
 -- TODO: do support for types.
 supportOf _tt
  = mempty


instance HasSupport (Term a) where
 supportOf mm
  = case mm of
        MAnn _ m        -> supportOf m
        MRef r          -> supportOf r

        MVar u          -> supportBoundTerm u

        MAbs (MPTypes bts) m
         ->     mappend (mconcat $ map supportOf $ map snd bts)
                        (supportDropBindTypes (Set.fromList $ map fst bts) $ supportOf m)

        MAbs (MPTerms bts) m
         ->     mappend (mconcat $ map supportOf $ map snd bts)
                        (supportDropBindTerms (Set.fromList $ map fst bts) $ supportOf m)

        MKey tk ms      -> mconcat (supportOf tk : map supportOf ms)


instance HasSupport (TermRef a) where
 supportOf mr
  = case mr of
        MRVal{}         -> mempty
        MRPrm{}         -> mempty
        MRCon{}         -> mempty
        MRTop{}         -> mempty


instance HasSupport TermKey where
 supportOf _tk          = mempty


instance HasSupport (TermArgs a) where
 supportOf tg
  = case tg of
        MGTerm  m       -> supportOf m
        MGTerms ms      -> mconcat $ map supportOf ms
        MGTypes ts      -> mconcat $ map supportOf ts


instance HasSupport (TermParams a) where
 supportOf tp
  = case tp of
        MPTerms bts     -> mconcat $ map supportOf $ map snd bts
        MPTypes bts     -> mconcat $ map supportOf $ map snd bts

