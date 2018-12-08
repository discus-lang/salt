
import qualified Waves.Prop.Core.Exp.Codec

import Control.Monad (unless)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import System.Exit (exitFailure)

all_tests :: [IO Bool]
all_tests
 = [ Waves.Prop.Core.Exp.Codec.tests
   ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence all_tests

  unless (and results) $ exitFailure
