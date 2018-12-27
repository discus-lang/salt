
module Salt.Core.Eval.Error where
import Salt.Core.Exp
import Data.Typeable
import Control.Exception


data Error a
        -- | Generic error when we don't know how to handle a construct.
        = ErrorInvalidConstruct
        { errorAnnot            :: a
        , errorTerm             :: Term a }

        -- | Variable binding is not in the environment.
        | ErrorVarUnbound
        { errorAnnot            :: a
        , errorVarUnbound       :: Bound
        , errorEnv              :: Env a }

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

        -- | Runtime type error in application,
        --   as the functional expression is not a closure.
        | ErrorAppTermTypeMismatch
        { errorAnnot            :: a
        , errorAppNotClo        :: Value a }

        -- | Runtime type error in application,
        --   because the function produced too many results.
        | ErrorAppTermBadClosure
        { errorAnnot            :: a
        , errorValues           :: [Value a] }

        -- | Runtime type error in application
        --   because the sort of parameters does not match the sort of arguments.
        | ErrorAppTermWrongArgs
        { errorAnnot            :: a
        , errorParams           :: TermParams a
        , errorArgs             :: TermArgs a }

        -- | Runtime type error in record projection.
        | ErrorProjectTypeMismatch
        { errorAnnot            :: a
        , errorProjectNotRecord :: Value a
        , errorProjectField     :: Name }

        -- | Missing field in record projection.
        | ErrorProjectMissingField
        { errorAnnot            :: a
        , errorProjectRecord    :: Value a
        , errorProjectField     :: Name }

        -- | Unknown primitive operator.
        | ErrorPrimUnknown
        { errorAnnot            :: a
        , errorPrimUnknown      :: Name }

        -- | Runtime type error in a primitive.
        | ErrorPrimTypeMismatch
        { errorAnnot            :: a
        , errorPrimName         :: Name
        , errorPrimArgs         :: [Value a] }
        deriving Show

instance (Show a, Typeable a) => Exception (Error a)

