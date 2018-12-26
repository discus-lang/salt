
module Salt.Core.Eval.State
        ( module Salt.Core.Exp
        , module Data.Typeable
        , module Data.Function
        , module Data.Maybe
        , module Data.Word
        , Set, Map

        , State(..)
        , Config(..), configDefault)
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


