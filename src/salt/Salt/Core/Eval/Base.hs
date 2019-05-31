
module Salt.Core.Eval.Base
        ( module Salt.Core.Exp
        , module Data.Typeable
        , module Data.Function
        , module Data.Maybe
        , module Data.Word
        , Set, Map

        , State(..)
        , newState
        , newCell, delCell, writeCell, readCell

        , Config(..)
        , configDefault

        , EvalType
        , EvalTerm
        , EvalControl (..))
where
import Salt.Core.Exp
import Control.Exception
import Data.Function
import Data.Maybe
import Data.Word
import Data.Set                 (Set)
import Data.Map                 (Map)
import Data.Typeable
import Data.IORef
import qualified Data.Map       as Map


------------------------------------------------------------------------------------------ State --
-- | State of the entire machine.
data State a
        = State
        { -- | Evaluator configuration.
          stateConfig   :: Config

          -- | Enclosing module.
        , stateModule   :: Module a

          -- | Generator for cell identifiers.
        , stateCellGen  :: IORef Int

          -- | Map of cell values.
        , stateCellVals :: IORef (Map Int (Value a)) }


-- | Create a new machine state with an empty set of cells.
newState :: Config -> Module a -> IO (State a)
newState config mm
 = do   refCellGen      <- newIORef 1
        refCellVals     <- newIORef Map.empty
        return  $ State config mm refCellGen refCellVals


-- | Allocate a new cell.
newCell :: State a -> IO Int
newCell state
 = do   iCell  <- readIORef $ stateCellGen state
        modifyIORef' (stateCellGen state) $ \i
         -> i + 1
        return iCell


-- | Delete an existing cell.
--   In the evaluation semantics cells are automatically deleted once they
--   go out of scope, which is a difference from ML-style references.
delCell :: State a -> Int -> IO ()
delCell state iCell
 = do   modifyIORef' (stateCellVals state) $ \mp
         -> Map.delete iCell mp


-- | Write a new value for a cell into the evaluator state.
writeCell :: State a -> Int -> Value a -> IO ()
writeCell state iCell value
 = do   modifyIORef' (stateCellVals state) $ \mp
         -> Map.insert iCell value mp
        return ()


-- | Read the value of a cell from the evaluator state.
readCell :: State a -> Int -> IO (Maybe (Value a))
readCell state iCell
 = do   mp      <- readIORef (stateCellVals state)
        return  $ Map.lookup iCell mp


----------------------------------------------------------------------------------------- Config --
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


------------------------------------------------------------------------------------------- Eval --
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
        | EvalControlBreak
        | EvalControlContinue
        | EvalControlLeave
        deriving (Show, Typeable)

instance (Show a, Typeable a)
      => Exception (EvalControl a)