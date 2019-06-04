
module Salt.Main.Mode.Make where
import Salt.Main.Mode.Emit
import Salt.Main.Mode.Test
import Salt.Main.Mode.Check
import Salt.Main.Mode.Parse
import Salt.Core.Exp


-- | If there is an 'emit' declaration defined then run that,
--   otherwise run any contained tests.
mainMake :: FilePath -> IO ()
mainMake filePath
 = do   mm         <- runParse filePath
        (mm', ctx) <- runCheck filePath mm

        let emits = [ d | DEmit d <- moduleDecls mm' ]
        let tests = [ d | DTest d <- moduleDecls mm' ]
        if not (null emits)
         then mapM_ (runEmit ctx mm') emits
         else mapM_ (runTest ctx mm') tests

