
module Salt.Core.Check.Module.Base
        ( module Salt.Core.Check.Where
        , module Salt.Core.Check.Error
        , CheckDecl
        , checkDecls)
where
import Salt.Core.Check.Where
import Salt.Core.Check.Error
import Salt.Core.Check.Term.Base
import qualified Control.Exception              as Control


type CheckDecl a
        = Annot a => a -> Context a -> Decl a -> IO (Decl a)

checkDecls
        :: forall a. Annot a
        => (Decl a  -> IO (Decl a))
        -> [Decl a] -> IO ([Decl a], [Error a])

checkDecls _check []
 = return ([], [])

checkDecls check (d1 : ds2)
 = do   (d1', errs1)
         <- Control.try (check d1)
         >>= \case
                Right d1'             -> return (d1', [])
                Left (err :: Error a) -> return (d1, [err])

        (ds2', errs2) <- checkDecls check ds2
        return (d1' : ds2', errs1 ++ errs2)



