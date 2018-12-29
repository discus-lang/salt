
module Salt.Core.Exp.Ups where
import Salt.Core.Exp.Name
import Data.Maybe


-- | Describes how to adjust bump counters on bound variable occurrences.
data Ups
        = Ups [UpsBump]
        deriving (Show, Eq, Ord)

type Depth      = Integer
type Bump       = Integer
type UpsBump    = ((Name, Depth), Bump)


-- | Empty ups list.
upsEmpty :: Ups
upsEmpty = Ups []


-- | Construct an `Ups` that bumps the given names by one, starting at depth zero.
upsOfNames :: [Name] -> Ups
upsOfNames ns
 = Ups [((n, 0), 1) | n <- ns]


-- | Construct an `Ups` that bumps names in the given list of binders.
upsOfBinds :: [Bind] -> Ups
upsOfBinds bs
 = upsOfNames [n | BindName n <- bs ]


-- | Check if the given `Ups` is empty.
upsIsEmpty :: Ups -> Bool
upsIsEmpty (Ups bs)
 = case bs of
        []      -> True
        _       -> False


-- | Bump an Ups due to pushing it under an abstraction with the given names.
upsBumpNames :: [Name] -> Ups -> Ups
upsBumpNames ns0 (Ups bs)
 = Ups $ mapMaybe (upsBump1 ns0) bs
 where
        upsBump1 ns l
         | ((name, depth), inc) <- l
         , elem name ns
         = Just ((name, depth + 1), inc)

         | otherwise
         = Just l


-- | Bump an Ups due to pushing it under an abstraction with the given binders.
upsBumpBinds :: [Bind] -> Ups -> Ups
upsBumpBinds bs ups
 = let  ns = [n | BindName n <- bs ]
   in   upsBumpNames ns ups


-- | Apply an ups to a bound occcurrence of a variable.
upsApplyBound :: Ups -> Bound -> Bound
upsApplyBound (Ups bs) (BoundWith name ix)
 = case bs of
        [] -> BoundWith name ix

        ((name', depth'), inc') : bs'
         |  name   == name'
         ,  depth' <= ix
         -> upsApplyBound (Ups bs') (BoundWith name (ix + inc'))

         |  otherwise
         -> upsApplyBound (Ups bs') (BoundWith name ix)


-- | Combine two lists of ups.
upsCombine :: Ups -> Ups -> Ups
upsCombine (Ups bs1) (Ups bs2)
 = Ups (foldr upsCombineBump bs2 bs1)


-- | Combine a bump with an existing list of them.
--   Applying the result to an expression will achieve the same result as
--   applying the whole list and then the extra one.
upsCombineBump :: UpsBump -> [UpsBump] -> [UpsBump]
upsCombineBump b bs
 | ((name, depth), inc) <- b
 = case bs of
        -- We cannot combine the new bump with anything else,
        -- so add it to the end of the list.
        []
         -> [b]

        b'@((name', depth'), inc') : bs'
         -- Combine the new bump with an existing one of the same name.
         |  name  == name'
         ,  depth == depth'
         -> ((name, depth'), inc + inc') : bs'

         -- Try to combine the new bump with the tail of the list.
         |  otherwise
         -> b' : upsCombineBump b bs'

