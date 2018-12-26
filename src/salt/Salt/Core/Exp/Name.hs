
module Salt.Core.Exp.Name
        ( Annot
        , Name  (..), Text, IsString(..)
        , Bind  (..)
        , Bound (..), pattern Bound)
where
import Data.Text                        (Text)
import Data.String
import Data.Typeable
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

        -- | Non-binding binder.
        --   This behaves like a binder where the name is not mentioned
        --   anywhere else in the program.
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

