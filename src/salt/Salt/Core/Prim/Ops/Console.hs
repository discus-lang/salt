
module Salt.Core.Prim.Ops.Console where
import Salt.Core.Prim.Ops.Base
import qualified Data.Text.IO           as Text


primOpsConsole
 = [ PO { name  = "console'print"
        , tsig  = [TText] :-> [TSusp [] TConsole]
        , exec  = \[NVs [VText tx]] -> do Text.putStr tx; return []
        , docs  = "Print a text string to the console." }

   , PO { name  = "console'println"
        , tsig  = [TText] :-> [TSusp [] TConsole]
        , exec  = \[NVs [VText tx]] -> do Text.putStrLn tx; return []
        , docs  = "Print a text string to the console, with a newline on the end." }
   ]

