
module Salt.Core.Check.Term.Bind where
import Salt.Core.Check.Type.Base
import Salt.Core.Check.Term.Base
import Salt.Core.Check.Term.Params


-- | Check a `TermBind`.
checkTermBind
        :: Annot a => a -> [Where a]
        -> Context a -> TermBind a -> IO (TermBind a)

checkTermBind a wh ctx (MBind b mpss tResult mBody)
 = do   (ctx', mpss')
         <- checkTermParamss a wh ctx mpss

        tResult'
         <- checkTypeHas UKind a wh ctx' TData tResult

        (mBody', _tsResult, _esResult)
         <- checkTerm a wh ctx' (Check [tResult]) mBody

        -- TODO: check binding is pure.

        return $ MBind b mpss' tResult' mBody'


-- | Make the type of a `TermBind`.
makeTypeOfTermBind :: TermBind a -> Type a
makeTypeOfTermBind (MBind _b mpss tResult _mBody)
 = loop mpss
 where
        loop [] = tResult

        loop (MPAnn _ mps' : pss')
         = loop (mps' : pss')

        loop (MPTerms bts : pss')
         = TFun (map snd bts) [loop pss']

        loop (MPTypes bts : pss')
         = TForall (TPTypes bts) $ loop pss'

