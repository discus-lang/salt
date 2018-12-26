
-- For GHC 8.4 -> 8.6 transition with Data.Semigroup
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Salt.Core.Analysis.Support where
import Salt.Core.Exp
import Data.Maybe
import Data.Semigroup
import Data.Set                 (Set)
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
freeTypeBoundsOf :: HasSupport a => a -> Set Bound
freeTypeBoundsOf x = supportFreeTypeVars $ supportOf x

freeTermBoundsOf :: HasSupport a => a -> Set Bound
freeTermBoundsOf x = supportFreeTermVars $ supportOf x


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
 =       Support nsType
                (Set.fromList $ mapMaybe (lowerBoundOfName n)  $ Set.toList nsTerm)


-- | Drop a set of term binds from a support set.
supportBindTerms :: [Bind] -> Support -> Support
supportBindTerms bs (Support nsType nsTerm)
 = let  ns      = Set.fromList [ n | BindName n <- bs ]
   in   Support nsType
                (Set.fromList $ mapMaybe (lowerBoundOfNames ns) $ Set.toList nsTerm)


---------------------------------------------------------------------------------------------------
class HasSupport a where
 supportOf :: a -> Support

instance HasSupport (Type a) where
 supportOf tt
  = case tt of
        TAnn _ t        -> supportOf t
        TRef r          -> supportOf r
        TVar u          -> supportBoundType u

        TAbs (TPTypes bks) t
         ->     supportBindTypes (map fst bks) $ supportOf t

        TKey tk ts      -> mconcat (supportOf tk : map supportOf ts)


instance HasSupport TypeRef where
 supportOf _    = mempty


instance HasSupport TypeKey where
 supportOf _    = mempty


instance HasSupport (TypeArgs a) where
 supportOf tg
  = case tg of
        TGTypes ts      -> mconcat $ map supportOf ts


instance HasSupport (Term a) where
 supportOf mm
  = case mm of
        MAnn _ m        -> supportOf m
        MRef r          -> supportOf r
        MVar u          -> supportBoundTerm u

        MAbs (MPTypes bts) m
         ->     mappend (mconcat $ map supportOf $ map snd bts)
                        (supportBindTypes (map fst bts) $ supportOf m)

        MAbs (MPTerms bts) m
         ->     mappend (mconcat $ map supportOf $ map snd bts)
                        (supportBindTerms (map fst bts) $ supportOf m)

        MKey mk ms      -> mconcat (supportOf mk : map supportOf ms)


instance HasSupport (TermRef a) where
 supportOf _    = mempty


instance HasSupport TermKey where
 supportOf _    = mempty


instance HasSupport (TermArgs a) where
 supportOf mg
  = case mg of
        MGTerm  m       -> supportOf m
        MGTerms ms      -> mconcat $ map supportOf ms
        MGTypes ts      -> mconcat $ map supportOf ts


instance HasSupport (TermParams a) where
 supportOf mp
  = case mp of
        MPTerms bts     -> mconcat $ map supportOf $ map snd bts
        MPTypes bts     -> mconcat $ map supportOf $ map snd bts

