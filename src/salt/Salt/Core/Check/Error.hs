
module Salt.Core.Check.Error where
import Salt.Core.Check.Where
import Salt.Core.Exp
import Control.Exception
import Data.Typeable


data Error a
        -- Malformed AST ------------------------
        = ErrorKindMalformed
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorKind             :: Kind a }

        | ErrorTypeMalformed
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a }

        | ErrorTermMalformed
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTerm             :: Term a }

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

        | ErrorUnknownTypeCtor
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorUnknownTypePrim
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorUnknownKindCtor
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

        -- Purity problems -----------------------
        | ErrorImpureTypeAbstraction
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorEffect           :: Type a }

        | ErrorImpureTermAbstraction
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorEffect           :: Type a }

        -- Unexpected types ----------------------
        | ErrorTypeMismatch
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypeExpected     :: Type a
        , errorTypeActual       :: Type a }

        -- Application problems ------------------
        | ErrorUnsaturatedPrim
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name
        , errorType             :: Type a }

        | ErrorUnsaturatedCtor
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name
        , errorType             :: Type a }

        | ErrorAppNoArguments
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a }

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

        | ErrorRecordDuplicateFields
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFields           :: [Name] }

        | ErrorRecordTypeDuplicateFields
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFields           :: [Name] }

        -- Variant problems ---------------------
        | ErrorVariantAnnotIsNot
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a }

        | ErrorVariantAnnotAltMissing
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a
        , errorLabel            :: Name }

        | ErrorVariantTypeDuplicateAlts
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFields           :: [Name] }

        | ErrorCaseScrutNotVariant
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a }

        | ErrorCaseAltNotInVariant
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorLabel            :: Name
        , errorType             :: Type a }

        | ErrorCaseAltPatMismatch
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorLabel            :: Name
        , errorTypeField        :: Type a
        , errorTypeScrut        :: Type a }

        | ErrorCaseAltsOverlapping
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorLabels           :: [Name] }

        | ErrorCaseAltsInexhaustive
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorLabels           :: [Name]
        , errorScrutType        :: Type a }

        -- Suspension problems ------------------
        | ErrorRunSuspensionIsNot
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypes            :: [Type a] }
        deriving Show

instance (Show a, Typeable a) => Exception (Error a)


