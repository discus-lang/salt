
module Salt.Llvm.Write.Exp where
import Salt.Llvm.Write.Type      ()
import Salt.Llvm.Write.Prim      ()
import Salt.Llvm.Syntax.Exp
import Salt.Llvm.Write.Base


instance Write Config Name where
 write o nn
  = case nn of
        NameGlobal str  -> do text o "@"; text o str
        NameLocal  str  -> do text o "%"; text o str


instance Write Config Var where
 write o (Var n t)
  = do write o t; space o; write o n


instance Write Config Lit where
 write o ll
  = case ll of
        LitInt   _ i    -> write o i
        LitFloat _ f    -> write o f
        LitNull  _      -> text o "null"
        LitUndef _      -> text o "undef"

        LitString _ txEnc _
         -> do text o "c"; dquotes o (text o txEnc)


instance Write Config Exp where
 write o xx
  = case xx of
        XVar v          -> write o (nameOfVar v)
        XLit l          -> write o l
        XUndef _        -> text  o "undef"
