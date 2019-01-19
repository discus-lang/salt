
module Salt.Llvm.Syntax
        ( -- * Modules
          Module        (..)
        , lookupCallConv

          -- * Global variables
        , Global        (..)
        , typeOfGlobal
        , varOfGlobal

          -- * Static data
        , Static        (..)
        , typeOfStatic

          -- * Function declarations
        , FunctionDecl  (..)
        , ParamListType (..)
        , Param         (..)
        , Align         (..)

          -- * Functions
        , Function      (..)
        , Section       (..)

          -- * Blocks
        , Block         (..)
        , defVarsOfBlock

          -- * Block labels
        , Label         (..)

          -- * Instructions
        , Instr         (..)
        , branchTargetsOfInstr
        , defVarOfInstr

          -- * Metadata
        , Metadata      (..)
        , MDecl         (..)
        , MRef          (..)
        , rval
        , tbaaNode

          -- * Expression types
        , Type          (..)
        , TypeAlias     (..)
        , isInt
        , isFloat
        , isPointer
        , takeBytesOfType

          -- * Expressions
        , Exp           (..)
        , typeOfExp
        , isXVar, isXLit, isXUndef
        , isClosedExp

          -- * Variables
        , Var           (..)
        , nameOfVar
        , typeOfVar

          -- * Names
        , Name          (..)
        , textOfName

          -- * Literals
        , Lit           (..)
        , typeOfLit
        , makeLitString

          -- * Primitive operators
        , Op            (..)
        , Cond          (..)
        , ICond         (..)
        , FCond         (..)
        , Conv          (..)

          -- * Attributes
        , FuncAttr      (..)
        , ParamAttr     (..)
        , CallConv      (..)
        , CallType      (..)
        , Linkage       (..))
where
import Salt.Llvm.Syntax.Attr
import Salt.Llvm.Syntax.Exp
import Salt.Llvm.Syntax.Function
import Salt.Llvm.Syntax.Instr
import Salt.Llvm.Syntax.Metadata
import Salt.Llvm.Syntax.Module
import Salt.Llvm.Syntax.Prim
import Salt.Llvm.Syntax.Type
