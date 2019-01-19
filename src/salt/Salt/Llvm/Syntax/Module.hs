
module Salt.Llvm.Syntax.Module
        ( -- * Modules
          Module    (..)
        , lookupCallConv

        , Global    (..)
        , typeOfGlobal
        , varOfGlobal

          -- * Static data.
        , Static    (..)
        , typeOfStatic)
where
import Salt.Llvm.Syntax.Function
import Salt.Llvm.Syntax.Exp
import Salt.Llvm.Syntax.Metadata
import Salt.Llvm.Syntax.Type
import Salt.Llvm.Syntax.Attr
import Control.Monad
import Data.List
import Data.Text        (Text)


-- Module ---------------------------------------------------------------------
-- | This is a top level container in LLVM.
data Module
        = Module
        { -- | Comments to include at the start of the module.
          moduleComments  :: [String]

          -- | Alias type definitions.
        , moduleAliases   :: [TypeAlias]

          -- | Global variables to include in the module.
        , moduleGlobals   :: [Global]

          -- | Functions used in this module but defined in other modules.
        , moduleFwdDecls  :: [FunctionDecl]

          -- | Functions defined in this module.
        , moduleFuncs     :: [Function]

          -- | Metdata for alias analysis
        , moduleMDecls    :: [MDecl]
        }


-- | Lookup the calling convention for this function,
--   using the forward declarations as well as the function definitions.
lookupCallConv :: Text -> Module -> Maybe CallConv
lookupCallConv name mm
  = liftM declCallConv
  $ find isFunctionDecl
  $ moduleFwdDecls mm ++ (map funDecl $ moduleFuncs mm)
  where isFunctionDecl decl
         = declName decl == name


-- Global ---------------------------------------------------------------------
-- | A global mutable variable. Maybe defined or external
data Global
        = GlobalStatic   Var Static
        | GlobalExternal Var
        deriving Show


-- | Return the 'LlvmType' of the 'LMGlobal'
typeOfGlobal :: Global -> Type
typeOfGlobal gg
 = case gg of
        GlobalStatic v _  -> typeOfVar v
        GlobalExternal v  -> typeOfVar v


-- | Return the 'LlvmVar' part of a 'LMGlobal'
varOfGlobal :: Global -> Var
varOfGlobal gg
 = case gg of
        GlobalStatic v _  -> v
        GlobalExternal v  -> v


-- Static ---------------------------------------------------------------------
-- | Llvm Static Data.
--   These represent the possible global level variables and constants.
data Static
        -- | A static variant of a literal value.
        = StaticLit     Lit

        -- | For uninitialised data.
        | StaticUninitType Type

        -- | Defines a static 'LMString'.
        | StaticStr     Text Type

        -- | A static array.
        | StaticArray   [Static] Type

        -- | A static structure type.
        | StaticStruct  [Static] Type

        -- | A pointer to other data.
        | StaticPointer Var

        -- Static expressions.
        -- | Pointer to Pointer conversion.
        | StaticBitc    Static Type

        -- | Pointer to Integer conversion.
        | StaticPtoI    Static Type

        -- | Constant addition operation.
        | StaticAdd     Static Static

        -- | Constant subtraction operation.
        | StaticSub     Static Static
        deriving (Show)


-- | Produce the 'LlvmType' of a 'LlvmStatic'.
typeOfStatic :: Static -> Type
typeOfStatic ss
 = case ss of
        StaticLit   l           -> typeOfLit l
        StaticUninitType t      -> t
        StaticStr    _ t        -> t
        StaticArray  _ t        -> t
        StaticStruct _ t        -> t
        StaticPointer v         -> typeOfVar v
        StaticBitc   _ t        -> t
        StaticPtoI   _ t        -> t
        StaticAdd    t _        -> typeOfStatic t
        StaticSub    t _        -> typeOfStatic t


