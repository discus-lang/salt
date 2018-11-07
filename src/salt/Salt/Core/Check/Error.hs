
module Salt.Core.Check.Error where
import Salt.Core.Check.Where
import Salt.Core.Exp
import Control.Exception
import Data.Typeable


data Error a
        -- Structural arity ---------------------
        = ErrorTermsWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypesActual      :: [Type a]
        , errorKindsExpected    :: [Type a] }

        | ErrorTypesWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypes            :: [Type a]
        , errorKinds            :: [Kind a] }

        -- Unknown vars and refs ----------------
        | ErrorUnknownPrimitive
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorUnknownDataCtor
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorUnknownTermBound
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorVar              :: Bound }

        -- Unexpected types ----------------------
        | ErrorTypeMismatch
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypeExpected     :: Type a
        , errorTypeActual       :: Type a }

        | ErrorAppTermTermCannot
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFunType          :: Type a }

        | ErrorAppTermTypeCannot
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFunType          :: Type a }

        | ErrorAppTermTermWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorCtorParamTypes   :: [Type a]
        , errorCtorArgTypes     :: [Type a] }

        | ErrorAppTermTermWrongArityNum
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorCtorParamTypes   :: [Type a]
        , errorCtorArgNum       :: Int }

        | ErrorAppTermTypeWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorCtorParamBinds   :: [(Bind, Type a)]
        , errorCtorArgTypes     :: [Type a] }

        -- Record problems ----------------------
        | ErrorRecordProjectIsNot
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a
        , errorLabel            :: Name }

        | ErrorRecordProjectNoField
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a
        , errorLabel            :: Name }
        deriving Show

instance (Show a, Typeable a) => Exception (Error a)


