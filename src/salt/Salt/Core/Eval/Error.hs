
module Salt.Core.Eval.Error where
import Salt.Core.Exp
import Data.Typeable
import Control.Exception


data Error a
        -- | Generic error when we don't know how to handle a term construct.
        = ErrorInvalidTerm
        { errorAnnot            :: a
        , errorTerm             :: Term a }

        -- | Generic error when we don't know how to handle a type construct.
        | ErrorInvalidType
        { errorAnnot            :: a
        , errorType             :: Type a }

        -- | Wrong number of types for eliminator.
        | ErrorWrongTypeArity
        { errorAnnot            :: a
        , errorNumExpected      :: Int
        , errorTypes            :: [Type a] }

        -- | Wrong number of terms for eliminator.
        | ErrorWrongTermArity
        { errorAnnot            :: a
        , errorNumExpected      :: Int
        , errorValues           :: [Value a] }

        -- Variables --------------------------------------
        -- | Type variable binding is not in the environment.
        | ErrorTypeVarUnbound
        { errorAnnot            :: a
        , errorVarUnbound       :: Bound
        , errorTypeEnv          :: TypeEnv a }

        -- | Term variable binding is not in the environment.
        | ErrorTermVarUnbound
        { errorAnnot            :: a
        , errorVarUnbound       :: Bound
        , errorTermEnv          :: TermEnv a }

        -- | Cell name does not have a matching cell identifier in the environment.
        | ErrorTermCellUnbound
        { errorAnnot            :: a
        , errorCellName         :: Name
        , errorTermEnv          :: TermEnv a }

        -- | Cell identifier does not have a matching cell in the state.
        | ErrorTermCellBroken
        { errorAnnot            :: a
        , errorCellIdent        :: Int
        , errorTermEnv          :: TermEnv a }

        -- Applications -----------------------------------
        -- | Runtime errors in type/type application,
        --   because the type operator to be applied is not a closure.
        | ErrorAppTypeBadClosure
        { errorAnnot            :: a
        , errorTypes            :: [Type a] }

        | ErrorAppTermBadClosure
        { errorAnnot            :: a
        , errorValues           :: [Value a] }

        -- | Runtime type error in application,
        --   as the functional expression is not a closure.
        | ErrorAppTermTypeMismatch
        { errorAnnot            :: a
        , errorAppNotClo        :: Value a }

        -- | Runtime type error in application
        --   because the sort of parameters does not match the sort of arguments.
        | ErrorAppTermMismatch
        { errorAnnot            :: a
        , errorParams           :: TermParams a
        , errorArgs             :: TermNormals a }

        -- | Unknown primitive operator.
        | ErrorPrimUnknown
        { errorAnnot            :: a
        , errorPrimUnknown      :: Name }

        -- | Runtime type error in a primitive.
        | ErrorPrimTypeMismatch
        { errorAnnot            :: a
        , errorPrimName         :: Name
        , errorPrimArgs         :: [Value a] }

        -- Records ----------------------------------------
        -- | Runtime type error in record projection.
        | ErrorProjectNotRecord
        { errorAnnot            :: a
        , errorProjectNotRecord :: Value a
        , errorProjectField     :: Name }

        -- | Missing field in record projection.
        | ErrorProjectMissingField
        { errorAnnot            :: a
        , errorProjectRecord    :: Value a
        , errorProjectField     :: Name }

        -- Variants --------------------------------------
        | ErrorCaseScrutNotVariant
        { errorAnnot            :: a
        , errorCaseScrut        :: Value a }

        | ErrorCaseNoMatch
        { errorAnnot            :: a
        , errorCaseScrut        :: Value a }

        -- If-then-else -----------------------------------
        -- | Scrutinee in 'ifs' construct is not a boolean value.
        | ErrorIfsScrutNotBool
        { errorAnnot            :: a
        , errorValue            :: Value a }

        -- Suspensions ------------------------------------
        -- | Value to run is not a suspension.
        | ErrorRunNotSuspension
        { errorAnnot            :: a
        , errorValue            :: Value a }

        -- Control errors ---------------------------------
        -- | Tried to break out of a launch construct.
        | ErrorLaunchBreak
        { errorAnnot            :: a }

        -- | Tried to continue out of a launch construct.
        | ErrorLaunchContinue
        { errorAnnot            :: a }

        -- | Tried to leave a launch construct.
        | ErrorLaunchLeave
        { errorAnnot            :: a }

        -- | Tried to apply unpack to a value which was not packed.
        | ErrorUnpackAppliedToNotPack
        { errorAnnot            :: a
        , errorTerm             :: Term a }

        deriving Show

instance (Show a, Typeable a) => Exception (Error a)

