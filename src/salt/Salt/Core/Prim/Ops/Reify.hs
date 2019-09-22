
module Salt.Core.Prim.Ops.Reify where
import Salt.Core.Prim.Ops.Base
import Salt.Core.Codec.Text.Pretty      ()
import qualified Salt.Data.Pretty       as P
import qualified Data.Text              as T


primOpsReify
 = [ PP { name  = "reify'pretty"
        , tsig  = [("a", TData)] :*> ["a"] :-> [TText]
        , step  = \[NTs [_t], NVs [v]]
                -> [VText $ T.pack $ P.render $ P.ppr () $ stripAnnot v]
        , docs  = "Reify a term into a pretty printed string." }
   ]
