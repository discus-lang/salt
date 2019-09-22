
module Salt.Core.Check.Error where
import Salt.Core.Check.Where
import Salt.Core.Exp
import Control.Exception
import Data.Typeable


-- Error messages that the type checker can throw.
data Error a
        -- Malformed AST ------------------------
        = ErrorTypeMalformed
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorKind             :: Kind a }

        | ErrorTermMalformed
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTerm             :: Term a }

        | ErrorTermNotFragment
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorFragment         :: Fragment
        , errorWhat             :: Text }

        -- Module level problems ----------------
        | ErrorTypeDeclRebound
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorTermDeclRebound
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorTermDeclImpure
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name
        , errorEffect           :: Type a }

        | ErrorTermDeclEmpty
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorTermDeclProcNoParams
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorTestDeclRebound
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorTestDeclImpure
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorNameMaybe        :: Maybe Name
        , errorEffect           :: Type a }

        | ErrorTypeDeclsRecursive
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name
        , errorLoop             :: [(Name, a)] }

        -- Structural arity ---------------------
        | ErrorWrongArity
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypesActual      :: [Type a]
        , errorTypesExpected    :: [Type a] }

        | ErrorWrongArityUp
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypesActual      :: [Type a]
        , errorKindsExpected    :: [Kind a] }

        -- Unknown vars and refs ----------------
        | ErrorUnknownPrim
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorUnknownCtor
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorName             :: Name }

        | ErrorUnknownBound
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorVar              :: Bound }

        -- Unexpected types ----------------------
        | ErrorMismatch
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypeExpected     :: Type a
        , errorTypeActual       :: Type a }

        -- Abstraction problems ------------------
        | ErrorAbsConflict
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorNames            :: [Name] }

        | ErrorAbsImpure
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorEffect           :: Type a }

        | ErrorAbsEmpty
        { errorUniverse         :: Universe
        , errorAnnot            :: a
        , errorWhere            :: [Where a] }

        | ErrorAbsTermNoValueForForall
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorParams           :: [TermParams a] }

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

        | ErrorAppNotFunction
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a }

        | ErrorAppVector
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypes            :: [Type a] }

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

        -- Let bindings --------------------------
        | ErrorLetWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypesActual      :: [Type a]
        , errorBinds            :: [Bind] }

        -- Rec bindings -------------------------
        | ErrorRecConflict
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorNames            :: [Name] }

        | ErrorRecValueRecursion
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorBind             :: Bind  }

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

        | ErrorCaseAltPatWrongArity
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorLabel            :: Name
        , errorTypesPat         :: [Type a]
        , errorTypesField       :: [Type a] }

        | ErrorCaseAltPatBindConflict
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorLabel            :: Name
        , errorNames            :: [Name] }

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

        -- Procedure problems -------------------
        | ErrorProcReturnNoLaunch
        { errorAnnot            :: a
        , errorWhere            :: [Where a] }

        | ErrorProcBreakNoLoop
        { errorAnnot            :: a
        , errorWhere            :: [Where a] }

        | ErrorProcContinueNoLoop
        { errorAnnot            :: a
        , errorWhere            :: [Where a] }

        | ErrorProcUpdateNotCell
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a }

        -- Existential problems -------------------
        | ErrorUnpackNotAppliedToPack
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTerm             :: Term a }

        | ErrorPackTypeParamMismatch
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorTypes            :: [Type a]
        , errorType             :: Type a }

        | ErrorUnpackTypeParamMismatch
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorBinds            :: [Bind]
        , errorType             :: Type a }

        | ErrorPackTypeNotExistential
        { errorAnnot            :: a
        , errorWhere            :: [Where a]
        , errorType             :: Type a }

        deriving Show

instance (Show a, Typeable a) => Exception (Error a)


