
module Salt.Core.Check.Error where
import Salt.Core.Check.Where
import Salt.Core.Exp
import Control.Exception
import Data.Typeable


data Error a
        -- Malformed AST ------------------------
        = ErrorTypeMalformed
        { errorAnnot            :: a
        , errorType             :: Type a}

        -- Structural arity ---------------------
        | ErrorTermsWrongArity
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

        | ErrorUnknownTypeBound
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorVar              :: Bound }

        | ErrorUnknownTermBound
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorVar              :: Bound }

        -- Let bindings --------------------------
        | ErrorLetWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypesActual      :: [Type a]
        , errorBinds            :: [Bind] }

        -- Unexpected types ----------------------
        | ErrorTypeMismatch
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypeExpected     :: Type a
        , errorTypeActual       :: Type a }

        -- type/type
        | ErrorAppTypeTypeCannot
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFunType          :: Type a }

        | ErrorAppTypeTypeWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypesExpected    :: [Type a]
        , errorTypesActual      :: [Type a] }

        | ErrorAppTypeTypeWrongArityNum
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorCtorParamTypes   :: [Type a]
        , errorCtorArgNum       :: Int }

        -- term/type
        | ErrorAppTermTypeCannot
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFunType          :: Type a }

        | ErrorAppTermTypeWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorCtorParamBinds   :: [(Bind, Type a)]
        , errorCtorArgTypes     :: [Type a] }

        -- term/term
        | ErrorAppTermTermCannot
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


