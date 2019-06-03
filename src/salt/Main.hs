
module Main where
import Salt.Llvm.Syntax                 ()
import Salt.Llvm.Write                  ()
import Salt.Main.Mode.Make
import Salt.Main.Mode.Emit
import Salt.Main.Mode.Test
import Salt.Main.Mode.Check
import Salt.Main.Mode.Parse
import Salt.Main.Mode.Lex
import Salt.Main.Config
import qualified Salt.LSP.Driver        as LSP

import qualified System.Environment     as System
import qualified System.Exit            as System

-- NOTE: StripAnnot is currently only used by the testing framework,
-- but we include it here so it always gets built along with the bin/salt,
-- and we can see when it needs updating.
import Salt.Core.Transform.StripAnnot           ()

main
 = do   args    <- System.getArgs
        config  <- parseArgs args configDefault
        case configMode config of
         Just (ModeLSP   mFileLog)      -> LSP.runLSP mFileLog
         Just (ModeMake  filePath)      -> mainMake   filePath
         Just (ModeEmit  filePath)      -> mainEmits  filePath
         Just (ModeTest  filePath)      -> mainTests  filePath
         Just (ModeTest1 filePath name) -> mainTest   filePath name
         Just (ModeCheck filePath)      -> mainCheck  filePath
         Just (ModeParse filePath)      -> mainParse  filePath
         Just (ModeLex   filePath)      -> mainLex    filePath

         -- Unhandled mode.
         _ -> do  putStr usage
                  System.exitFailure


