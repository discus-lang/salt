
module Salt.Llvm.Write.Type where
import Salt.Llvm.Syntax.Type
import Salt.Llvm.Write.Attr     ()
import Salt.Llvm.Write.Base


instance Write Config Param where
 write o (Param t _attrs)
        = write o t


instance Write Config FunctionDecl where
 write o (FunctionDecl n l c r vargs params a strategy)
  = do
        write o l; space o
        write o c; space o
        write o r; space o
        text  o " @"
        text o n

        brackets o $ do
                punc o "," params
                (case vargs of
                  VarArgs | null params -> text o "..."
                          | otherwise   -> text o ", ..."
                  _                     -> return ())

        (case a of
          AlignNone     -> return ()
          AlignBytes a' -> text o " align " >> write o a')

        (case strategy of
          Nothing       -> return ()
          Just strat'   -> text o " gc " >> dquotes o (text o strat'))


instance Write Config TypeAlias where
 write o (TypeAlias n t)
  = do text o "%"; text o n; text o " = type "; write o t


instance Write Config Type where
 write o tt
  = case tt of
        TVoid           -> text o "void"
        TInt size       -> text o "i" >> write o size
        TFloat          -> text o "float"
        TDouble         -> text o "double"
        TFloat80        -> text o "x86_fp80"
        TFloat128       -> text o "fp128"
        TLabel          -> text o "label"
        TPointer x      -> write o x >> text o "*"

        TStruct tys
         -> do text o "<{"; punc o "," tys; text o "}>"

        TArray nr tp
         -> brackets o $ do write o nr; text o " x "; write o tp

        TAlias (TypeAlias s _)
         -> do  text o "%"; text o s

        TFunction (FunctionDecl _ _ _ r varg params _ _)
         -> do  write o r
                parens o $ do
                        punc o "," params

                        (case varg of
                          VarArgs | null params  -> text o "..."
                                  | otherwise    -> text o ", ..."
                          _                      -> return ())

