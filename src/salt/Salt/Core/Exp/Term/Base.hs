
module Salt.Core.Exp.Term.Base where
import Salt.Core.Exp.Type
import Salt.Core.Exp.Name
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Int       as Int
import qualified Data.Word      as Word
import qualified Foreign.Ptr    as FPtr


-- | Annotated Term.
data Term a
        = MAnn  a (Term a)                      -- ^ An annotated term.
        | MRef  (TermRef a)                     -- ^ Term reference.
        | MVar  Bound                           -- ^ Term variable.
        | MAbs  (TermParams a) (Term a)         -- ^ Term abstraction.
        | MRec  [TermBind a]   (Term a)         -- ^ Term recursion.
        | MKey  TermKey [TermArgs a]            -- ^ Term keyword application.
        deriving (Show, Eq, Ord)


-- | Term binding.
data TermBind a
        = MBind Bind [TermParams a] [Type a] (Term a)
        deriving (Show, Eq, Ord)


-- | Term Reference.
data TermRef a
        = MRPrm Name                            -- ^ Primitive reference.
        | MRCon Name                            -- ^ Data constructor reference.
        | MRVal (Value a)                       -- ^ Value reference.
        deriving (Show, Eq, Ord)


-- | Term Parameters.
data TermParams a
        = MPAnn  a  (TermParams a)
        | MPTerms   [(Bind, Type a)]            -- ^ Term parameters for a term.
        | MPTypes   [(Bind, Type a)]            -- ^ Type parameters for a term.
        deriving (Show, Eq, Ord)


-- | Term Arguments.
data TermArgs a
        = MGAnn  a  (TermArgs a)
        | MGTerm    (Term a)                    -- ^ Argument producing a vector of terms.
        | MGTerms   [Term a]                    -- ^ Arguments of individual terms.
        | MGTypes   [Type a]                    -- ^ Arguments of individual types.
        deriving (Show, Eq, Ord)


-- | Term Keyword.
data TermKey
        -- functional term formers.
        = MKTerms                               -- ^ Term sequence former.
        | MKThe                                 -- ^ Type ascription.
        | MKApp                                 -- ^ Term application.
        | MKLet                                 -- ^ Let expression former.
        | MKCon     Name                        -- ^ Data constructor.
        | MKBox                                 -- ^ Box up a computation.
        | MKRun                                 -- ^ Run a computation.
        | MKRecord  [Name]                      -- ^ Record former.
        | MKProject Name                        -- ^ Record field projection.
        | MKVariant Name                        -- ^ Variant former.
        | MKVarCase                             -- ^ Variant case matching.
        | MKVarAlt  Name                        -- ^ Variant case alternative.
        | MKIf                                  -- ^ If-then-else expression.
        | MKList                                -- ^ List constructor.
        | MKSet                                 -- ^ Set constructor.
        | MKMap                                 -- ^ Map constructor.
        | MKPrivate                             -- ^ Region introduction.
        | MKExtend                              -- ^ Region extension.

        -- procedural term formers.
        | MKSeq                                 -- ^ Procedural sequencing.
        | MKLaunch                              -- ^ Define scope of a 'return' statement.
        | MKReturn                              -- ^ Return to the enclosing 'launch' statement.
        | MKCell                                -- ^ Define a new storage cell.
        | MKUpdate                              -- ^ Update a storage cell.
        | MKWhens                               -- ^ Branch on boolean.
        | MKMatch                               -- ^ Branch on variants.
        | MKEnter                               -- ^ Enter a recursive procedure.
        | MKLeave                               -- ^ Leave a recursive procedure.
        | MKLoop                                -- ^ Define a loop.
        | MKBreak                               -- ^ Break to the end of the enclosing loop.
        | MKContinue                            -- ^ Continue to the next loop iteration.
        | MKWhile                               -- ^ Standard while loop.

        | MKPack
        | MKUnpack
        deriving (Show, Eq, Ord)


-- | Term Value.
data Value a
        -- Values that are also literals in the source program.
        = VUnit                                 -- ^ Unit value.
        | VBool     Bool                        -- ^ Boolean value.
        | VNat      Integer                     -- ^ Natural value.
        | VInt      Integer                     -- ^ Integer value.
        | VWord     Integer                     -- ^ Word value.

        | VInt8     Int.Int8                    -- ^ 8 bit Integer value.
        | VInt16    Int.Int16                   -- ^ 16 bit Integer value.
        | VInt32    Int.Int32                   -- ^ 32 bit Integer value.
        | VInt64    Int.Int64                   -- ^ 64 bit Integer value.

        | VWord8    Word.Word8                  -- ^ 8 bit Word value.
        | VWord16   Word.Word16                 -- ^ 16 bit Word value.
        | VWord32   Word.Word32                 -- ^ 32 bit Word value.
        | VWord64   Word.Word64                 -- ^ 64 bit Word value.

        | VText     Text                        -- ^ Text value.
        | VSymbol   Name                        -- ^ Symbol value.

        -- Values that are only used at runtime.
        --   At runtime they are introduced by evaluating constructions,
        --   and do not appear as literals in the source program.
        --   The annotation on map and set elements is forced to () so that
        --   the order of values in the collection does not depend on the
        --   annotation.
        | VData     Name [Type a] [Value a]     -- ^ Constructed data.
        | VRecord   [(Name, [Value a])]         -- ^ Record value.
        | VVariant  Name (Type a) [Value a]     -- ^ Variant value.
        | VList     (Type a) [Value a]          -- ^ List value.
        | VSet      (Type a) (Set (Value ()))   -- ^ Set value.
        | VMap      (Type a) (Type a) (Map (Value ()) (Value a))
                                                -- ^ Map value.
        | VClosure  (TermClosure a)             -- ^ Closure.
        | VBundle   (Bundle a)                  -- ^ Declation bundle.
        | VAddr     FPtr.WordPtr                -- ^ Raw memory address.
        | VLoc      (Type a) Int
        | VPtr      (Type a) (Type a) FPtr.WordPtr -- ^ Ptr with Region and Value types.
        | VExtPair  (Value a) [Type a] (Type a) -- ^ Existential pair
        deriving (Show, Eq, Ord)


-- | Closure value.
data TermClosure a
        = TermClosure (TermEnv a) (TermParams a) (Term a)
        deriving (Show, Eq, Ord)


-- | Environments captured in term closures.
data TermEnv a
        = TermEnv [TermEnvBinds a]
        deriving (Show, Eq, Ord)


-- | Bindings in environments.
data TermEnvBinds a
        = TermEnvTypes          (Map Name (Type a))
        | TermEnvValues         (Map Name (Value a))
        | TermEnvValuesRec      (Map Name (TermClosure a))
        deriving (Show, Eq, Ord)


-- | Normal form arguments for a function application.
--   Argument can be closed types as well as values.
data TermNormals a
        = NTs [Type a]
        | NVs [Value a]
        deriving (Show, Eq, Ord)


-- | Bundle refies a closed collection of desugared type and term declarations.
--   This is used when specifying how to transform code using Salt as its own
--   meta language. We don't use the source version of declarations as they
--   also represent syntactic sugar and include extra meta commands such as for
--   the testing framework, which doesn't make sense as part of a bundle of
--   code for compilation.
data Bundle a
        = Bundle
        { bundleTypes   :: Map Name (BundleType a)
        , bundleTerms   :: Map Name (BundleTerm a) }
        deriving (Show, Eq, Ord)


-- | Type declaration held in a code bundle.
data BundleType a
        = BundleType
        { bundleAnnot           :: a
        , bundleName            :: Name
        , bundleTypeParams      :: [TypeParams a]
        , bundleKindResult      :: Kind a
        , bundleTypeBody        :: Type a }
        deriving (Show, Eq, Ord)


-- | Term declaration held in a code bundle.
data BundleTerm a
        = BundleTerm
        { bundleAnnot           :: a
        , bundleName            :: Name
        , bundleTermParams      :: [TermParams a]
        , bundleTypeResult      :: [Type a]
        , bundleTermBody        :: Term a }
        deriving (Show, Eq, Ord)

