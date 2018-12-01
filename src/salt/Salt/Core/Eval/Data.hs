
module Salt.Core.Eval.Data
        ( module Salt.Core.Exp
        , module Data.Typeable
        , module Data.Function
        , module Data.Maybe
        , module Data.Word
        , Set, Map

        , State(..)
        , Config(..), configDefault
        , Error(..))
where
import Salt.Core.Exp
import Data.Function
import Data.Maybe
import Data.Word
import Data.Set                 (Set)
import Data.Map                 (Map)
import Data.Typeable
import Control.Exception


---------------------------------------------------------------------------------------------------
-- | State of the entire machine.
data State a
        = State
        { stateConfig           :: !Config
        , stateDeclTerms        :: !(Map Name (DeclTerm a)) }
        deriving Show


-- | Evaluator configuration.
data Config
        = Config
        { configThing   :: Bool }
        deriving Show


-- | Default evaluator configuration.
configDefault :: Config
configDefault
        = Config
        { configThing   = False }


---------------------------------------------------------------------------------------------------
data Error a
        -- | Generic error when we don't know how to handle a construct.
        = ErrorInvalidConstruct
        { errorAnnot            :: a
        , errorTerm             :: Term a }

        -- | Variable binding is not in the environment.
        | ErrorVarUnbound
        { errorAnnot            :: a
        , errorVarUnbound       :: Bound
        , errorEnv              :: Env a }

        -- | Runtime type error in application,
        --   as the functional expression is not a closure.
        | ErrorAppTermTypeMismatch
        { errorAnnot            :: a
        , errorAppNotClo        :: Value a }

        -- | Runtime type error in application,
        --   because the function produced too many results.
        | ErrorAppTermTooMany
        { errorAnnot            :: a
        , errorValues           :: [Value a] }

        -- | Runtine type error in application,
        --   because the function did not produce enough results.
        | ErrorAppTermNotEnough
        { errorAnnot            :: a
        , errorValues           :: [Value a] }

        -- | Runtime type error in record projection.
        | ErrorProjectTypeMismatch
        { errorAnnot            :: a
        , errorProjectNotRecord :: Value a
        , errorProjectField     :: Name }

        -- | Missing field in record projection.
        | ErrorProjectMissingField
        { errorAnnot            :: a
        , errorProjectRecord    :: Value a
        , errorProjectField     :: Name }

        -- | Unknown primitive operator.
        | ErrorPrimUnknown
        { errorAnnot            :: a
        , errorPrimUnknown      :: Name }

        -- | Runtime type error in a primitive.
        | ErrorPrimTypeMismatch
        { errorAnnot            :: a
        , errorPrimName         :: Name
        , errorPrimArgs         :: [Value a] }
        deriving Show

instance (Show a, Typeable a) => Exception (Error a)

