
module Salt.Core.Exp.Name
        ( Annot
        , Name  (..), Text, IsString(..)
        , Bind  (..)
        , Bound (..), pattern Bound

        , Ups   (..), Depth, Bump
        , upsEmpty,   upsOfNames
        , upsIsEmpty
        , upsApplyBound, upsBump
        , upsCombine, upsCombineBump)
where
import Data.Text                        (Text)
import Data.String
import Data.Typeable
import Data.Maybe
import qualified Data.Text              as T


---------------------------------------------------------------------------------------------------
-- | The usual constraints on expression annotations.
--   Typeable allows us to throw exceptions that mention them.
type Annot a = (Show a, Typeable a)


---------------------------------------------------------------------------------------------------
-- | Names of things.
data Name
        = Name Text
        deriving (Show, Eq, Ord)

instance IsString Name where
 fromString str = Name $ T.pack str


---------------------------------------------------------------------------------------------------
-- | Binding occurence of variable.
data Bind
        -- | Named binder.
        = BindName Name

        -- | Non-binding binder,
        --   which behaves like binding a name which is not used
        --   elsewhere in the program.
        | BindNone
        deriving (Show, Eq, Ord)

instance IsString Bind where
 fromString str = BindName $ Name $ T.pack str


---------------------------------------------------------------------------------------------------
-- | Bound occurrence of variable.
data Bound
        = BoundWith Name Integer
        deriving (Show, Eq, Ord)

pattern Bound n = BoundWith n 0


instance IsString Bound where
 fromString str = Bound $ Name $ T.pack str


---------------------------------------------------------------------------------------------------
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


-- | An ups that bumps the given names by one, starting at depth zero.
upsOfNames :: [Name] -> Ups
upsOfNames ns
 = Ups [((n, 0), 1) | n <- ns]


-- | Check if the given ups is empty.
upsIsEmpty :: Ups -> Bool
upsIsEmpty (Ups bs)
 = case bs of
        []      -> True
        _       -> False


-- | Apply an ups to a bound occcurrence of a variable.
upsApplyBound :: Ups -> Bound -> Bound
upsApplyBound (Ups bs) (BoundWith name ix)
 = case bs of
        []
         -> BoundWith name ix

        ((name', depth'), inc') : bs'
         |  name   == name'
         ,  depth' <= ix
         -> upsApplyBound (Ups bs') (BoundWith name (ix + inc'))

         |  otherwise
         -> upsApplyBound (Ups bs') (BoundWith name ix)


-- | Bump an Ups due to pushing it under an absraction with the given
--   named binders.
upsBump :: [Name] -> Ups -> Ups
upsBump ns0 (Ups bs)
 = Ups $ mapMaybe (upsBump1 ns0) bs
 where
        upsBump1 ns l
         | ((name, depth), inc) <- l
         , elem name ns
         = Just ((name, depth + 1), inc)

         | otherwise
         = Just l


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

