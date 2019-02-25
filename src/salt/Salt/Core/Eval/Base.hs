
module Salt.Core.Eval.Base
        ( module Salt.Core.Exp
        , module Data.Typeable
        , module Data.Function
        , module Data.Maybe
        , module Data.Word
        , Set, Map

        , State(..)
        , Config(..), configDefault
        , EvalType
        , EvalTerm)
where
import Salt.Core.Exp
import Data.Function
import Data.Maybe
import Data.Word
import Data.Set                 (Set)
import Data.Map                 (Map)
import Data.Typeable


-- | State of the entire machine.
data State a
        = State
        { stateConfig   :: !Config
        , stateModule   :: !(Module a) }
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


-- | The usual shape of term evaluation functions.
type EvalType a x y
        = Annot a => State a -> a -> TypeEnv a -> x -> IO y


-- | The usual shape of type evaluation functions.
type EvalTerm a x y
        = Annot a => State a -> a -> TermEnv a -> x -> IO y


-- | Values thrown during interpreter evaluation.
--
--   We use the exception mechanism of the meta language (Haskell)
--   to implement object language (Salt) control effects.
data EvalControl a
        = EvalControlReturn [Value a]
        deriving (Show, Typeable)