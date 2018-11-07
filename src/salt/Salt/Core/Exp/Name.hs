
module Salt.Core.Exp.Name
        ( Name  (..), Text, IsString(..)
        , Bind  (..)
        , Bound (..)
        , Annot)
where
import Data.Text                (Text)
import Data.String
import qualified Data.Text      as T
import Data.Typeable

-- | Names of things.
data Name
        = Name Text
        deriving (Show, Eq, Ord)

instance IsString Name where
 fromString str = Name $ T.pack str


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


-- | Bound occurrence of variable.
data Bound
        = Bound Name
        deriving (Show, Eq, Ord)

instance IsString Bound where
 fromString str = Bound $ Name $ T.pack str


-- | The usual constraints on expression annotations.
--   Typeable allows us to throw exceptions that mention them.
type Annot a = (Show a, Typeable a)
